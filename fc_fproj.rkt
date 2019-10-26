(load "fc_mix0.rkt")
(load "fc_mix1.rkt")
(load "tm_intrp.rkt")

;
; I Futamura projection
;
(define (fc-mix-fp1 div vs0)
  (run-mix0 (list tm-intrp div vs0)))

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
; comp = [[mix {for} fc {on} fc]]
;         [ mix {for} fc {in} fc
;         , div {for} mix {fc, fc}
;         , vs_0 = [ program -- int
(define div-mix (list
	'program
  'div
))

(define vs-mix (list
  (pair 'program tm-intrp)
  (pair 'div div-tm)
))


(define fc-mix-fp2
  (run-mix1 (list fc-mix1 div-mix vs-mix))
)
 
(define fc-mix-fp2-pp
  (pretty-print (run-mix1 (list fc-mix1 div-mix vs-mix)))
)

;
; *Check that this is really a compiler.
;
(define fc-mix-fp2-test-pp
  (pretty-print (intrp fc-mix-fp2-pp (list vs0-tm)))
)

;
; fc-mix-fp2-test-pp and fc-mix-fp1-test-pp are equal.




