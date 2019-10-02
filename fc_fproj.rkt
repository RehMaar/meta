(load "fc_mix.rkt")
(load "tm_intrp.rkt")

(define (fc-mix-fp1 div vs0)
	(run-mix0 (list tm-intrp div_tm vs0_tm)))


;
; Test FP1
;

(define div_tm (list
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

(define vs0_tm (list
	(pair 'prog tm-prog)))

(define fc-mix-fp1-test
  (fc-mix-fp1 div_tm vs0_tm))

(define fc-mix-fp1-test-pp
  (pretty-print (fc-mix-fp1 div_tm vs0_tm)))
