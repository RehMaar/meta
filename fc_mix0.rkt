
(require racket/trace)

(load "utility.rkt")
(load "fc_intrp.rkt")
(load "tm_intrp.rkt")

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
  ; add read line
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

(define (lookup-div div x) (elem? x div))

; program: ((read ..) (label ...) ..)
(define (first-label program) (caadr program))

; performs constant folding of static parts of an expression
; 
; Well, if `vs` has the same structure as `ctx` everything
; will be okay.
; (define (reduce expr vs) ;(intrp-expr-subst vs expr))

(define (reduce expr vs)
  (if (null? expr) expr
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
                            (eval (cdr ee))
                            (cdr ee)))
                 			   l_eval_expr)))
      )
))

(define (reduce-var ctx var)
  (pair (key? ctx var) (normalize (lookup ctx var)))
)

; (trace reduce)
; (trace reduce-expr)
; (trace reduce-op)
; (trace reduce-var)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Transition compression mix
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
	(if (elem? instr code)
		code
    (cons instr code)
  )
)

(define (run-mix0 args) (intrp fc-mix0 args))
; poly := { (pp0, vs0) }
; while unmarked (pp, vs) in poly
;   mark (pp, vs)
;   generate code for bb at pp using values in vs
;   polu := polu \cup successors (pp, vs)
(define fc-mix0
  ; read (program, division, vs0)
  '((read program div vs0)
    ; pending <- { pp0, vs0 }
    ; marked  <- {}
    (init
      (:= pending (list (pair (first-label program) vs0)))
      (:= marked '())
      (:= residual-code '())
      (goto loop)
    )

    ; while pending != {}
    ;   (pp, vs) <- pop pending
    ;   marked   <- marked \cup (pp, vs)
    ;   bb       <- lookup program pp
    ;   code-block     <- init with label (pp, vs)
    (loop
      (:= pp (caar pending))
      (:= vs (cdar pending))
      (:= pending (cdr pending))
      (:= label (pair pp vs))
      (if (elem? (pair pp vs) marked) check-pending loop-mark)
    )

    (loop-mark
      (:= marked (cons (pair pp vs) marked))
      (:= bb (lookup program pp))
      (:= code-block '())
      (goto loop-inner)
    )

    ; while bb != {}
    ;   commnad <- first_command bb
    ;   bb      <- rest bb
    ;   case command of
    (loop-inner
      (:= command (car bb))
      (:= bb (cdr bb))
      (if (equal? ':= (car command)) assign-case check-goto)
    )
    ;     goto pp'
    (check-goto
      (if (equal? 'goto (car command)) goto-case check-if)
    )
    ;     if expt then goto pp' else goto pp''
    (check-if
      (if (equal? 'if (car command)) if-case check-return)
    )
    ;     return exp
    (check-return
      (if (equal? 'return (car command)) return-case error-match-command)
    )
          
    ; cases
          
    ; (:= name expr):
    ;   ':= -- (car command)
    ;   'name -- (cadr command)
    ;   'expr -- (caddr command)
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

    ; (goto label):
    ;   `goto -- (car command)
    ;   `label -- (cadr command)
    (goto-case
      (:= bb (lookup program (cadr command)))
      (goto loop-inner-end)
    )

    ; (if expr label1 label2):
    ;   'if -- (car command)
    ;   'expr -- (cadr command)
    ;   'label1 -- (caddr command)
    ;   'label2 -- (cadddr command)
    (if-case
      (if (lookup-div div (cadr command)) if-static if-dynamic)
    )
    (if-static
      (:= bb (if (reduce (cadr command) vs)
                     (lookup program (caddr command))
                     (lookup program (cadddr command))))
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

    ; (return expr)
    ;  `return -- (car command)
    ;  `expr   -- (cadr command)
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

    ; exit
    (exit
     (return (cons (generate-read (car program) vs0) (reverse residual-code)))
    )

    ; error messages
    (error-match-command
      (return (error "Wrong command to match: " command))
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;; Tests for find-name

(define vs0 (list
  (pair 'name 'z)
  (pair 'namelist '(a b z b d))
))

(define div (list
  'name
  'namelist
  '(equal? name (car namelist))
))

(define vs01 (list
  (pair 'name 'z)
))

(define div1 (list
  'name
))

; (define test-fn (run-mix0 (list find-name div vs0)))


;;;;;; Trace
; (trace fc-mix-simpl)
; (trace next-labels)
; (trace next-labels-step)
; (trace generate-code)
; (trace generate-code-step)
