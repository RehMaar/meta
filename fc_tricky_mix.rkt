(require racket/trace)


(load "utility.rkt")
(load "fc_intrp.rkt")
(load "tm_intrp.rkt")

(define (display-nl msg)
  (begin
   (display msg)
   (display "\n")
))

; Block-in-pending contains:
; - first label (pp0, vs0)
; - Labels from `if
;
;
; (label (:= ..) (if ..))
;
; WARNING: without read statement
(define (find-block-in-pending div program)
  (define (add-label l) (pair l (lookup program l)))

  (define (to-last-command stmts)
    (match (car stmts)
     [`(:= ,v ,e) (to-last-command (cdr stmts))]
     [e e]
  ))  

  (define (find-block-in-pending-inner program-inner)
    (if (null? program-inner) '()
      ; car program-inner -- head bb
      (match (to-last-command (cdar program-inner))
	    	[`(if ,e ,g1 ,g2)
          (if (lookup-div div e)
            (find-block-in-pending-inner (cdr program-inner))
  	    		(append (list (add-label g1) (add-label g2))
                    (find-block-in-pending-inner (cdr program-inner)))
        )]
        [_ (find-block-in-pending-inner (cdr program-inner))]
      )))
   (find-block-in-pending-inner program)
)

(define (find-labels-in-pending div program)
  (map (lambda (x) (car x)) (find-block-in-pending div program))
)


;
; The trick
;
(define (run-mix-new args) (intrp fc-mix-new args))
(define fc-mix-new
  '((read program div vs0)
    (init
      (:= pending (list (pair (first-label program) vs0)))
      (:= marked '())
      (:= residual-code (list (generate-read (car program) vs0)))
      (:= program (cdr program))
      (:= block-in-pending (cons (car program) (find-block-in-pending div program)))
      (goto loop)
    )

    (loop
      (:= pp (caar pending))
      (:= vs (cdar pending))
      (:= pending (cdr pending))
      (:= label (pair pp vs))
      (if (elem? label marked) check-pending loop-mark)
    )

    (loop-mark
      (:= marked (cons (pair pp vs) marked))
      (:= code-block '())
      (goto lookup-pp)
    )

    ; static: program, lookup-program
    (lookup-pp
      (:= lookup-program block-in-pending)
      (goto lookup-pp-1)
    )

    ; static: program, lookup-program, pp-cur, bb-cur
    (lookup-pp-1
      (:= pp-cur (caar lookup-program))
      (:= bb (cdar lookup-program))
      (if (equal? pp pp-cur) loop-inner lookup-pp-2)
    )
    (lookup-pp-2
      (:= lookup-program (cdr lookup-program))
      (if (null? lookup-program) error lookup-pp-1)
    )

    (loop-inner
      (:= command (car bb))
      (:= bb (cdr bb))
      (if (equal? ':= (car command)) assign-case check-goto)
    )
    (check-goto
      (if (equal? 'goto (car command)) goto-case check-return)
    )
    (check-return
      (if (equal? 'return (car command)) return-case check-if)
    )
    (check-if
      (if (equal? 'if (car command)) if-case error-match-command)
    )
          
    (assign-case
      (if (lookup-div div (cadr command)) assign-static assign-dynamic)
    )
    (assign-static
      (:= vs (update vs (cadr command) (reduce (caddr command) vs)))
      (goto loop-inner-end)
    )
    (assign-dynamic
      (:= code-block (extend (generate-assign (cadr command) (caddr command) vs) code-block))
      (goto loop-inner-end)
    )

    (goto-case
      (:= bb (lookup program (cadr command)))
      (goto loop-inner-end)
    )

    (if-case
      (if (lookup-div div (cadr command)) if-static if-dynamic)
    )

    ; TODO: BUG, because vs is dynamic, but bb is static
    ; So self-app doesn't work, because reduce of '(if (reduce (cadr command) vs) ..)
    ; returns (if (reduce (cadr command) vs) ..) with substitutions.
    (if-static
      (if (reduce (cadr command) vs) g1 g2)
    )
    (g1
     (:= bb (lookup program (caddr command)))
     (goto loop-inner-end)
    )
    (g2
     (:= bb (lookup program (cadddr command)))
     (goto loop-inner-end)
    )          

    (if-dynamic
      (:= pending (unite (list (pair (caddr command) vs)
                               (pair (cadddr command) vs)) pending))
      (:= code-block (extend
                 (generate-if (cadr command) vs (caddr command) (cadddr command))
                 code-block))
      (goto loop-inner-end)
    )

    (return-case
      (:= code-block (extend (generate-return (cadr command) vs) code-block))
      (goto loop-inner-end)
    )
     
    (loop-inner-end
      (if (null? bb) loop-end loop-inner)
    )

    (loop-end 
      (:= residual-code (cons (cons label (reverse code-block)) residual-code))
      (goto check-pending)
    )
    (check-pending
      (if (null? pending) exit loop)
    )

    (exit
      (return (reverse residual-code))
    )

    (error-match-command
      (return (error "Wrong command to match: " command))
    )
    (error
      (return (error "Some stupid error"))
    )
  )
)

(define (run-mix-new-debug args) (intrp fc-mix-new-debug args))
(define fc-mix-new-debug
  '((read program div vs0)
    (init
      (:= residual-code (list (generate-read (car program) vs0)))
      (:= program (cdr program))

      ; (car program) contains first block with label (pp0, vs0), 
      ; which are always in block-in-pending
      (:= block-in-pending (cons (car program) (find-block-in-pending div program)))

      (:= pending (list (pair (caar program) vs0)))
      (:= marked '())
      (goto loop)
    )

    (loop
      (:= pp (caar pending))
      (:= vs (cdar pending))
      (:= pending (cdr pending))
      (:= label (pair pp vs))

      (if (elem? label marked) marked-print loop-mark)
    )
    (marked-print
      ;(:= debug (display "Marked: "))
      ;(:= debug (display-nl pp))
      (goto check-pending)
    )

    (loop-mark
      (:= marked (cons label marked))
      (:= code-block '())

      ;(:= debug (display "OUTER LOOP: "))
      ;(:= debug (display pp))
      ;(:= debug (display " "))
      ;(:= debug (display-nl vs))
      ;(:= debug (display-nl (length residual-code)))

      (goto lookup-pp)
    )

    ; static: program, lookup-program
    (lookup-pp
      (:= lookup-program block-in-pending)
      (goto lookup-pp-1)
    )

    ; static: program, lookup-program, pp-cur, bb-cur
    (lookup-pp-1
      (:= pp-cur (caar lookup-program))
      (if (equal? pp pp-cur) lookup-pp-end lookup-pp-2)
    )
    (lookup-pp-2
      (:= lookup-program (cdr lookup-program))
      (if (null? lookup-program) error lookup-pp-1)
    )
    ; dynamic: marked, pp vs, code-block
    ; static:  bb, bb-cur
    (lookup-pp-end
      (:= bb (cdar lookup-program))
      (goto loop-inner)
    )

    (loop-inner
      ;(:= debug (display "FOUND BB: "))
      ;(:= debug (display-nl bb))

      (:= command (car bb))
      (:= bb (cdr bb))
      (if (equal? ':= (car command)) assign-case check-goto)
    )
    (check-goto
      (if (equal? 'goto (car command)) goto-case check-if)
    )
    (check-if
      (if (equal? 'if (car command)) if-case check-return)
    )
    (check-return
      (if (equal? 'return (car command)) return-case error-match-command)
    )
          
    (assign-case
      (if (lookup-div div (cadr command)) assign-static assign-dynamic)
    )
    (assign-static
      ;(:= debug (display "UPDATE OLD-VS: "))
      ;(:= debug (display-nl vs))
      ;(:= debug (display "\n"))
      ;(:= debug (display-nl (cadr command)))
      ;(:= debug (display-nl (caddr command)))
      ;(:= debug (display (reduce (caddr command) vs)))
      ;(:= debug (display "\n\n"))

      (:= vs (update vs (cadr command) (reduce (caddr command) vs)))

      ;(:= debug (display "UPDATE NEW-VS: "))
      ;(:= debug (display-nl vs))
      ;(:= debug (display "\n\n\n"))

      (goto loop-inner-end)
    )
    (assign-dynamic
      (:= code-block (extend (generate-assign (cadr command) (caddr command) vs) code-block))
      (goto loop-inner-end)
    )

    (goto-case
      (:= bb (lookup program (cadr command)))
      (goto loop-inner-end)
    )

    (if-case
      (if (lookup-div div (cadr command)) if-static if-dynamic)
    )
    ; TODO: BUG, because vs is dynamic, but bb is static
    ; So self-app doesn't work, because reduce of '(if (reduce (cadr command) vs) ..)
    ; returns (if (reduce (cadr command) vs) ..) with substitutions.
    ;
    ; Suddenly, if we divide assignment and `if, it okay
    (if-static
      ;(:= bb (if (reduce (cadr command) vs)
      ;               (lookup program (caddr command))
      ;               (lookup program (cadddr command))))
      ;(goto loop-inner-end)
      (if (reduce (cadr command) vs) g1 g2)
    )
    (g1
     (:= bb (lookup program (caddr command)))
     (goto loop-inner-end)
    )
    (g2
     (:= bb (lookup program (cadddr command)))
     (goto loop-inner-end)
    )

    (if-dynamic
      ;(:= debug (display "NEW PENDING COMMAND: "))
      ;(:= debug (display-nl command))
      ;(:= debug (display "NEW PENDING 1: "))
      ;(:= debug (display-nl (caddr command)))
      ;(:= debug (display "NEW PENDING 2: "))
      ;(:= debug (display-nl (cadddr command)))
      ;(:= debug (display "NEW PENDING VS: "))
      ;(:= debug (display-nl vs))

      (:= pending (unite pending (list (pair (caddr command) vs)
                               (pair (cadddr command) vs))))
      (:= code-block (extend
                 (generate-if (cadr command) vs (caddr command) (cadddr command))
                 code-block))
      (goto loop-inner-end)
    )

    (return-case
      (:= code-block (extend (generate-return (cadr command) vs) code-block))
      (goto loop-inner-end)
    )
     
    (loop-inner-end
      (if (null? bb) loop-end loop-inner)
    )

    (loop-end
      (:= residual-code (cons (cons label (reverse code-block)) residual-code))
      (goto check-pending)
    )
    (check-pending
      (if (null? pending) exit loop)
    )

    (exit
     (return (reverse residual-code))
    )

    (error-match-command
      (return (error "Wrong command to match: " command))
    )
    (error
      (return (error "Some stupid error"))
    )
  )
)

(define (run-mix args) (intrp fc-mix args))
(define fc-mix
  ; read (program, division, vs0)
  '((read program div vs0)
    (init
      (:= pending (list (pair (first-label program) vs0)))
      (:= marked '())
      (:= residual-code (list (generate-read (car program) vs0)))    
      (goto loop)
    )

    (loop
      (:= pp (caar pending))
      (:= vs (cdar pending))
      (:= pending (cdr pending))
      (:= label (pair pp vs))
      (if (elem? label marked) check-pending loop-mark)
    )

    (loop-mark
      (:= marked (cons (pair pp vs) marked))
      (:= bb (lookup program pp))
      (:= code-block '())
      (goto loop-inner)
    )

    (loop-inner
      (:= command (car bb))
     
      ;(:= debug (display-nl "COMMAND: "))
      ;(:= debug (display-nl command))

      (:= bb (cdr bb))
      (if (equal? ':= (car command)) assign-case check-goto)
    )
    (check-goto
      (if (equal? 'goto (car command)) goto-case check-if)
    )
    (check-if
      (if (equal? 'if (car command)) if-case check-return)
    )
    (check-return
      (if (equal? 'return (car command)) return-case error-match-command)
    )
          
    (assign-case
      (if (lookup-div div (cadr command)) assign-static assign-dynamic)
    )
    (assign-static
      (:= vs (update vs (cadr command) (reduce (caddr command) vs)))
      (goto loop-inner-end)
    )
    (assign-dynamic
      (:= code-block (extend (generate-assign (cadr command) (caddr command) vs) code-block))
      (goto loop-inner-end)
    )

    (goto-case
      (:= bb (lookup program (cadr command)))
      (goto loop-inner-end)
    )

    (if-case
      (if (lookup-div div (cadr command)) if-static if-dynamic)
    )
    ;(if-static
    ;  (:= bb (if (reduce (cadr command) vs)
    ;                 (lookup program (caddr command))
    ;                 (lookup program (cadddr command))))
    ;  (goto loop-inner-end)
    ;)
          
    (if-static
      (if (reduce (cadr command) vs) g1 g2)
    )
    (g1
     (:= bb (lookup program (caddr command)))
     (goto loop-inner-end)
    )
    (g2
     (:= bb (lookup program (cadddr command)))
     (goto loop-inner-end)
    )

    (if-dynamic
      (:= pending (unite (list (pair (caddr command) vs)
                               (pair (cadddr command) vs)) pending))
      (:= code-block (extend
                 (generate-if (cadr command) vs (caddr command) (cadddr command))
                 code-block))
      (goto loop-inner-end)
    )

    (return-case
      (:= code-block (extend (generate-return (cadr command) vs) code-block))
      (goto loop-inner-end)
    )
     
    (loop-inner-end
      (if (null? bb) loop-end loop-inner)
    )

    (loop-end 

      ;(:= debug (display-nl (reverse code-block)))
      ;(:= debug (display-nl "\n\n"))

      (:= residual-code (cons (cons label (reverse code-block)) residual-code))
      (goto check-pending)
    )
    (check-pending
      (if (null? pending) exit loop)
    )

    (exit
     (return (reverse residual-code))
    )

    (error-match-command
      (return (error "Wrong command to match: " command))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (lookup-div div x)
  (elem? x div))

; program: ((read ..) (label ...) ..)
(define (first-label program) (caadr program))


; performs constant folding of static parts of an expression
(define (reduce expr vs)
  (if (constant? expr) expr
    (let ([pp (reduce-expr vs expr)])
      (if (car pp)
        (eval (cdr pp))
        (cdr pp)
      )
)))

(define (reduce-expr ctx expr)
   (cond

    [(number? expr) (pair #t expr)]

    [(constant? expr) (pair #t expr)]

    [(operator? expr)
      (reduce-op ctx expr)]

    [else
      (reduce-var ctx expr)]
    )
)

(define (reduce-op ctx op-stmt)
    (let ([l_eval_expr
       (map
          (lambda (x) (reduce-expr ctx x))
          (cdr op-stmt))
      ])
      
			(if (all (car (unzip l_eval_expr)))
        (pair #t (cons (car op-stmt) (cdr (unzip l_eval_expr))))
  			(pair #f (cons (car op-stmt) (map
                        (lambda (ee)
                          (if (car ee)
                            (eval-e (cdr ee))
                            (cdr ee)))
                 			   l_eval_expr)))
      )
))

(define (eval-e e)
  (if (number? (eval e)) (eval e) (normalize (eval e)))
)

(define (reduce-var ctx var)
  (if (key? ctx var)
    (pair #t (normalize (lookup ctx var)))
    (pair #f var)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-assign name expr vs)
  `(:= ,name ,(reduce expr vs))
)

(define (generate-if expr vs goto1 goto2)
 `(if ,(reduce expr vs)
    ,(pair goto1 vs)
    ,(pair goto2 vs))
)

(define (generate-return expr vs)
   `(return ,(reduce expr vs))
)

(define (generate-read read-stmt vs)
  (cons 'read (filter (lambda (e) (not (key? vs e))) (cdr read-stmt)))
)

(define (extend instr code) 
    (cons instr code)
)

;;;;;;;;; Pretty printing

(define (readable-labels fc-prog)
  (define (readable-label label nls)
    (pair (+ 1 (car nls))
      (cons (pair label `(label ,(+ 1 (car nls)))) (cdr nls))
    )
  )

  (let ([fc-labels (map (lambda (bb) (car bb)) fc-prog)])
    ;(map readabl-label fc-labels)
    (cdr (foldr readable-label (pair 0 '()) fc-labels))
))

(define (pretty-print fc-prog)
  (let ([new-labels (readable-labels (cdr fc-prog))])
    (cons (car fc-prog)
          (map (lambda (bb) (traverse-bblock new-labels bb))
               (cdr fc-prog)))
  )
;  (cons (car fc-prog) result)
)

; (car bb) -- label
; (cadr bb) -- first command
(define (traverse-bblock nlabels bb)
  (define (tb b)
    (match (car b)
      [`(goto ,label) (list `(goto ,(lookup nlabels label)))]
      [`(if ,expr ,label1 ,label2)
            (list `(if ,expr ,(lookup nlabels label1)
                       ,(lookup nlabels label2)))]
      [`(return ,expr) (list `(return ,expr))]
      [x (cons x (tb (cdr b)))]
    )
  )
  (cons (lookup nlabels (car bb)) (tb (cdr bb)))
)

;;;;;;;;;; Test program

(define find-name 
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont
      (:= valuelist (cdr valuelist))
      (:= namelist (cdr namelist))
      (goto search)
    )
    (found (return (car valuelist)))
   )
)
