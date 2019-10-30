(load "utility.rkt")
(load "fc_intrp.rkt")

(define (run-intrp-on-fc prog input)
  (intrp intrp-on-fc (list prog input))
)

(define intrp-on-fc
  '((read prog input)
    (init
      (:= ctx (zip (cdar prog) input))
      (:= bb (cdadr prog))
      (goto intrp-stmt))
    (intrp-stmt
      (:= stmt (car bb))
      (:= bb (cdr bb))
      (if (equal? ':= (car stmt)) handle-assign check-goto))
    (check-goto
      (if (equal? 'goto (car stmt)) handle-goto check-if))
    (check-if
      (if (equal? 'if (car stmt)) handle-if check-return))
    (check-return
      (if (equal? 'return (car stmt)) handle-return error))
    
    ; (:= val expr)
    (handle-assign
			(:= ctx (update ctx (second stmt) (intrp-expr ctx (third stmt))))
      (goto check-bb))
    ; (if expr then else)
    (handle-if
			(if (intrp-expr ctx (second stmt)) handle-if-then handle-if-else))
    (handle-if-then
      (:= bb (lookup prog (third stmt)))
      (goto intrp-stmt))
    (handle-if-else
      (:= bb (lookup prog (fourth stmt)))
      (goto intrp-stmt))

    ; goto label
    (handle-goto 
      (:= bb (lookup prog (second stmt)))
      (goto intrp-stmt))
    ; return expr
    (handle-return
      (return (intrp-expr ctx (second stmt))))

    (check-bb
      (if (null? bb) error intrp-stmt))

    (error (return 'ERROR))
   )
)
