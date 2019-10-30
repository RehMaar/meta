(load "fc_mix.rkt")
(load "tm_intrp.rkt")

;
; I Futamura projection
;
(define (fc-mix-fp1 div vs0)
  (run-mix (list tm-intrp div vs0)))

;
; Test FP1
;
(define div-tm (list
  'prog
  'next-label
  'instr
  '(equal? (car instr) 'goto)
  '(equal? (car instr) 'left)
  '(equal? (car instr) 'right)
  '(equal? (car instr) 'if)
  '(equal? (car instr) 'write)
  '(caar prog)
  '(lookup prog next-label)
  '(+ 1 next-label)
  '(car instr)
  '(cadr instr)
  '(cadddr instr)
  '(not (has-label next-label prog))
))

(define vs0-tm (list
  (pair 'prog tm-prog)))

(define vs0-tm2 (list
  (pair 'prog tm-write)))

(define vs0-tm3 (list
  (pair 'prog tm-goto)))

; test
;(define fc-mix-fp1-test
;  (fc-mix-fp1 div-tm vs0-tm))
;;
;;; pretty printed test
;(define fc-mix-fp1-test-pp
;  (pretty-print (fc-mix-fp1 div-tm vs0-tm)))
;;
;(define fc-mix-fp1-test-pp-2
;  (pretty-print (fc-mix-fp1 div-tm vs0-tm2)))

(define fc-mix-fp1-test-pp-3
  (pretty-print (fc-mix-fp1 div-tm vs0-tm3)))

;
; II Futamura projection
;
(define div-mix (list
	'program
  '(car program)
  '(first-label program)

  'div
))

(define vs-mix (list
  (pair 'program tm-intrp)
  (pair 'div div-tm)
))


(define fc-mix-fp2
  (run-mix (list fc-mix div-mix vs-mix))
)
;
(define fc-mix-fp2-pp
  (pretty-print (run-mix (list fc-mix div-mix vs-mix)))
)
;
;;;
;;; *Check that this is really a compiler.
;;;
(define fc-mix-fp2-test-pp
  (pretty-print (intrp fc-mix-fp2-pp (list vs0-tm)))
)
;
(define fc-mix-fp2-test-pp2
  (pretty-print (intrp fc-mix-fp2-pp (list vs0-tm2)))
)
(define fc-mix-fp2-test-pp3
  (pretty-print (intrp fc-mix-fp2-pp (list (list (pair 'prog tm-goto)))))
)
;
;;;
;;;
;;; fc-mix-fp2-test-pp and fc-mix-fp1-test-pp are equal.
;;
