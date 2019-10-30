
(load "fc_mix0.rkt")

;
; Generate a set of possible successors of point (pp, vs).
;
(define (next-labels bblock vs)
  (define (next-labels-step block vs)
    (match (car block)
      ; handle and then call next-labels-step
      ; `handle` returns new vs
      ; (cdr block) -- the rest commands
      [`(:= ,name ,expr) (next-labels-step (cdr block) vs)]
  
      ; don't care about result of an expression?
      [`(if expr ,label1 ,label2) (list label1 label2)]
  
      [`(goto ,label) (list label)]
  
      [`(return ,expr) '()]

      [else (error "next-labels-step: can't match block:" (car block))]))

  ; returns updated vs
  (define (handle vs name expr)
    (if (key? vs name)
      (update vs name expr)
      vs))

  (next-labels-step (cdr bblock) vs)
)

;
; `div` is a map from names to boolean.
;  : static -> true
;  : dynam  -> false
; return (pair vs new-code)
(define (generate-code div program pp vs)
	(define (generate-code-step bblock vs)
    (match (car bblock)
      [`(:= ,val ,expr) (if (lookup-div div val)
        ; static
        (generate-code-step (cdr bblock)
          (update vs val (eval (reduce expr vs))))
        ; dynamic
        (snd (lambda (x) (cons `(:= ,val ,(reduce expr vs)) x))
          (generate-code-step (cdr bblock) vs))
      )]

      [`(return ,expr) (pair vs `((return ,(reduce expr vs))))]

      [`(goto ,label)  (pair vs `((goto ,(pair label vs))))]

      [`(if ,expr ,label1 ,label2)
        (if (lookup-div div expr)
          ; static
          (pair vs (if (eval (reduce expr vs))
            `((goto ,(pair label1 vs)))
            `((goto ,(pair label2 vs)))
          ))
          ; dynamic
          (pair vs `((if ,(reduce expr vs)
                      ,(pair label1 vs)
                      ,(pair label2 vs)))))
      ]
      [else (error "generate-code-step: unable to generate code for " (car bblock))]
  ))
  (snd (lambda (x) (cons (pair pp vs) x))
    (generate-code-step (lookup program pp) vs))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Super simple mix
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-ms args) (intrp fc-mix-simpl args))
; poly := { (pp0, vs0) }
; while unmarked (pp, vs) in poly
;   mark (pp, vs)
;   generate code for bb at pp using values in vs
;   polu := polu \cup successors (pp, vs)
(define fc-mix-simpl
  ; read (program, division, vs0)
  '((read program div vs0)
    ; pending <- { pp0, vs0 }
    ; marked <- {}
    (init
      ; pp0 -- initial program point
      (:= pp0 (first-label program))
      (:= pending (list (pair pp0 vs0)))
      (:= marked '())
      (:= code '())
      (goto loop)
    )
    ;
    (loop
      (:= ppvs (car pending))
      (:= pending (cdr pending))
      (if (elem? ppvs marked) loop loop-mark)
    )

    ; for clarity
    (loop-mark
      (:= marked (cons ppvs marked))
      (goto generate)
    )

    (generate
      ; res = (vs, new-code)
      (:= res (generate-code div program (car ppvs) (cdr ppvs)))
      (:= code (cons (cdr res) code))
      ; next-labels bblock vs
      (:= pending (unite
                    (next-labels (cdr res) (car res))
                    pending))
      (goto loop-end)
    )
          
    (loop-end
      (if (null? pending) exit loop)
    )

    (exit (return (reverse code)))
   )
)
