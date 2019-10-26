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
  '(eq? (car instr) 'goto)
  '(eq? (car instr) 'left)
  '(eq? (car instr) 'right)
  '(eq? (car instr) 'if)
  '(eq? (car instr) 'write)
  '(not (has-label next-label prog))
))

(define vs0-tm (list
  (pair 'prog tm-prog)))

; test
(define fc-mix-fp1-test
  (fc-mix-fp1 div-tm vs0-tm))

; pretty printed test
(define fc-mix-fp1-test-pp
  (pretty-print (fc-mix-fp1 div-tm vs0-tm)))

;
; II Futamura projection
;
(define div-mix (list
	'program
  'div
))

(define vs-mix (list
  (pair 'program tm-intrp)
  (pair 'div div-tm)
))


(define fc-mix-fp2
  (run-mix (list fc-mix div-mix vs-mix))
)
 
(define fc-mix-fp2-pp
  (pretty-print (run-mix (list fc-mix div-mix vs-mix)))
)

;
; *Check that this is really a compiler.
;
(define fc-mix-fp2-test-pp
  (pretty-print (intrp fc-mix-fp2-pp (list vs0-tm)))
)

;
; fc-mix-fp2-test-pp and fc-mix-fp1-test-pp are equal.

