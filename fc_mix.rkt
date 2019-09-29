
(require racket/trace)

(load "utility.rkt")
(load "fc_intrp.rkt")
(load "tm_intrp.rkt")


(define (lookup-div div x) (elem? x div))

; program: ((read ..) (label ...) ..)
(define (first-label program) (caadr program))

; performs constant folding of static parts of an expression
; 
; Well, if `vs` has the same structure as `ctx` everything
; will be okay.
(define (reduce expr vs) (intrp-expr-subst vs expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Transition compression mix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-assign name expr vs)
  `(:= ,name ,(reduce expr vs))
)

(define (generate-if expr vs goto1 goto2)
 `(if ,(reduce expr vs)
    ,(pair goto2 vs)
    ,(pair goto2 vs))
)

(define (generate-return expr vs)
   `(return ,(reduce expr vs))
)

(define (run-m0 args) (intrp fc-mix0 args))
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
      (:= vs (update vs (cadr command) (eval (reduce (caddr command) vs))))
      (goto loop-inner-end)
    )
    (assign-dynamic
      (:= code-block (cons (generate-assign (cadr command) (caddr command) vs) code-block))
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
      (:= bb (if (eval (reduce (cadr command) vs))
                     (lookup program (caddr command))
                     (lookup program (cadddr command))))
      (goto loop-inner-end)
    )
    (if-dynamic
      (:= pending (unite (list (pair (caddr command) vs)
                               (pair (cadddr command) vs)) pending))
      (:= code-block (cons
                 (generate-if (cadr command) vs (caddr command) (cadddr command))
                 code-block))
      (goto loop-inner-end)
    )

    ; (return expr)
    ;  `return -- (car command)
    ;  `expr   -- (cadr command)
    (return-case
      (:= code-block (cons (generate-return (cadr command) vs) code-block))
      (goto loop-inner-end)
    )
     
    (loop-inner-end
      (if (null? bb) loop-end loop-inner)
    )

    (loop-end 
      (:= residual-code (cons (cons (pair pp vs) code-block) residual-code))
      (goto check-pending)
    )
    (check-pending
      (if (null? pending) exit loop)
    )

    ; exit
    (exit
     (return (reverse residual-code))
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

; (define test-fn (run-ms (list find-name div vs0)))


;;;;;; Trace
; (trace fc-mix-simpl)
; (trace next-labels)
; (trace next-labels-step)
; (trace generate-code)
; (trace generate-code-step)
