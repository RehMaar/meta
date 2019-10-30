(load "tm_intrp.rkt")
(load "fc_tricky_mix.rkt")

;
; I Futamura projection
;

; New mixer
(define (fc-mix-fp1-new div vs0)
  (run-mix-new-without-debug (list tm-intrp div vs0)))

; The oldest
(define (fc-mix-fp1 div vs0)
  (run-mix (list tm-intrp div vs0)))

;;
;; Test FP1
;;
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

;; test
(define (fc-mix-fp1-test-new)
  (fc-mix-fp1-new div-tm vs0-tm))
;
(define (fc-mix-fp1-test)
  (fc-mix-fp1 div-tm vs0-tm))

;; pretty printed test
(define (fc-mix-fp1-test-pp-new)
  (pretty-print (fc-mix-fp1-test-new)))

(define (fc-mix-fp1-test-pp)
  (pretty-print (fc-mix-fp1-test)))

(define fc-mix-fp1-pp-3-new
  (pretty-print (fc-mix-fp1-new div-tm (list (list (pair 'prog tm-goto)))))
)

(define fc-mix-fp1-pp-3
  (pretty-print (fc-mix-fp1 div-tm (list (list (pair 'prog tm-goto)))))
)

;
;;
;; II Futamura projection
;;
(define div-mix (list
  'div

	'program
  '(cdr program)

;'program-dict

  'block-in-pending
  '(cons (car program) (find-block-in-pending div program))

  'lookup-program
  '(caar lookup-program)
  '(cdar lookup-program)
  '(cdr lookup-program)
  '(null? lookup-program)

  'pp-cur
  '(equal? pp pp-cur)

  'bb
  '(car bb)
  '(cdr bb)
  '(null? bb)

  'command
  '(car command)
  '(cadr command)
  '(equal? ':= (car command))
  '(equal? 'goto (car command))
  '(equal? 'if (car command))
  '(equal? 'return (car command))

  '(lookup-div div (cadr command))
  '(lookup program (cadr command))
  '(lookup program (caddr command))
  '(lookup program (cadddr command))
))

(define vs0-mix (list
  (pair 'program tm-intrp)
  (pair 'div div-tm)
))
;
;
(define (fc-mix-fp2 div vs)
  (run-mix-new-without-debug (list fc-mix-new-without-debug div vs))
)
; 
(define fc-mix-fp2-pp
  (pretty-print (fc-mix-fp2 div-mix (init-dict vs0-mix)))
)
;
;;
;; *Check that this is really a compiler.
;;
(define (fc-mix-fp2-test-pp)
  (pretty-print (intrp fc-mix-fp2-pp (list vs0-tm)))
)

(define fc-mix-fp2-test-pp2
  (pretty-print (intrp fc-mix-fp2-pp (list vs0-tm2)))
)
(define fc-mix-fp2-test-pp3
  (pretty-print (intrp fc-mix-fp2-pp (list (list (pair 'prog tm-goto)))))
)


;
; fc-mix-fp2-test-pp and fc-mix-fp1-test-pp are equal.

;(display (fc-mix-fp2-pp))

(define (fc-mix-fp2  div vs)
  (run-mix (list fc-mix div vs))
)
; 
;(define fc-mix-fp2-pp-orig
;  (fc-mix-fp2-orig div-mix (init-dict vs0-mix))
;)
;
;;
;; *Check that this is really a compiler.
;;
;(define (fc-mix-fp2-test-pp-orig)
;  (pretty-print (intrp fc-mix-fp2-pp-orig (list vs0-tm)))
;)
;
;(define fc-mix-fp2-test-pp2-orig
;  (pretty-print (intrp fc-mix-fp2-pp-orig (list vs0-tm2)))
;)
;(define fc-mix-fp2-test-pp3-orig
;  (pretty-print (intrp fc-mix-fp2-pp-orig (list (list (pair 'prog tm-goto)))))
;)
