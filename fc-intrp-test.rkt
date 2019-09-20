;
; Test section
;

(load "fc_intrp.rkt")

; test labels
(define labels
  '((label1 (return 10))
    (label2 (return 20))
    (label3 (return 30))
   )
)

; test exprs
(define expr1 '(cdr x))
(define expr2 '(+ 1 2))
(define expr3 1)
(define expr4 'x)
(define expr5 '(+ 1 (+ 2 x)))

; test jumps
(define jmp1 '(return (+ 1 2)))
(define jmp2 '(return 2))
(define jmp3 '(goto label))
(define jmp4 '(if (eq? 1 1) label1 label2))
(define jmp5 '(return x))

(define without-reading
  '((read) (label1 (return 'yes)))
)

; example programs
(define const10
  '((read x)
    (label (return 10))
   )
)

(define id-prog
  '((read x)
    (label1 (return x))
   )
)

(define p1
  '((read x)
    (label2 (if (eq? x 0) label3 label4))
    (label3 (return 1))
    (label4 (return 2))
   )
)

(define p2
  '((read x)
    (label2 (if (eq? (car x) 0) label3 label4))
    (label3 (return 1))
    (label4 (return 2))
   )
)

(define p3
  '((read x)
    (label2 (if (eq? (caar x) 0) label3 label4))
    (label3 (return 1))
    (label4 (return 2))
   )
)

(define tst-p
  (all (list
   (equal? (intrp p1 (list 0)) 1)
   (equal? (intrp p1 (list 1)) 2)
   (equal? (intrp p2 '((0 1 2))) 1)
   (equal? (intrp p2 '((1 1 2))) 2)
   (equal? (intrp p3 '(((0 0) (3 4) (1 2)))) 1)
   (equal? (intrp p3 '(((1 0) (3 4) (1 2)))) 2)
)))

(define a1
  '((read x)
    (label1 
            (:= i (car x))
            (:= x (cdr x))
            (if (eq? i 'a) label2 label3)
    )
    (label2 (return 0))
    (label3 (if (null? x) label2 label1))
   )
)

(define a2
  '((read x)
    (label1 
      (:= i (car x))
      (if (eq? i 'a) label2 label3)
    )
    (label2 (return 'eq))
    (label3 (return 'neq))
   )
)

(define a3
  '((read x)
    (label1 (if (eq? (car x) 'a) label2 label3))
    (label2 (return 'eq))
    (label3 (return 'neq))
   )
)

(define tst-a
  (all (list
    (equal? (intrp a3 '((a b c))) 'eq)
    (equal? (intrp a3 '((d d d))) 'neq)
    (equal? (intrp a2 '((a b c))) 'eq)
    (equal? (intrp a2 '((d d d))) 'neq)
  ))
)

(define tst-aa
  (list
    (intrp a3 '((a b c)))
    (intrp a3 '((d d d))) 
    (intrp a2 '((a b c)))
    (intrp a2 '((d d d))) 
  )
)
