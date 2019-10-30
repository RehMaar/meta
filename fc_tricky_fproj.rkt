(load "tm_intrp.rkt")
(load "fc_tricky_mix.rkt")
(load "fc_intrp_on_fc.rkt")

;
; I Futamura projection
;

; New mixer
(define (fc-mix-fp1-new div vs0)
  (run-mix-new (list tm-intrp div vs0)))

; Old mixer
(define (fc-mix-fp1 div vs0)
  (run-mix (list tm-intrp div vs0)))

; Division for TM interpreter.
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

;; test
(define (fc-mix-fp1-test-new)
  (pretty-print (fc-mix-fp1-new div-tm vs0-tm)))

(define (fc-mix-fp1-test)
  (pretty-print (fc-mix-fp1 div-tm vs0-tm)))


; test 2
(define vs0-tm2 (list
  (pair 'prog tm-write)))

(define (fc-mix-fp1-test-pp-2-new)
  (pretty-print (fc-mix-fp1-new div-tm vs0-tm2)))

(define (fc-mix-fp1-test-pp-2)
  (pretty-print (fc-mix-fp1 div-tm vs0-tm2)))


; test 3
(define vs0-tm3 (list
  (pair 'prog tm-goto)))

(define (fc-mix-fp1-test-pp-3-new)
  (pretty-print (fc-mix-fp1-new div-tm vs0-tm3)))

(define (fc-mix-fp1-test-pp-3)
  (pretty-print (fc-mix-fp1 div-tm vs0-tm3)))

;
;;
;; II Futamura projection
;;
;

; Division for a new mixer
(define div-mix (list
  'div

	'program
  '(cdr program)

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

(define (fc-mix-fp2 div vs)
  (run-mix-new (list fc-mix-new div vs)))

;
; Generate a compiler.
;
(define (fc-mix-fp2-pp)
  (pretty-print (fc-mix-fp2 div-mix vs0-mix)))

;
;;
;; *Check that this is really a compiler.
;;
;
; Compile several TM programs.
;

; DEATH
(define (fc-mix-fp2-test-pp)
  (pretty-print (intrp fc-mix-fp2-pp (list vs0-tm)))
)

; Good
(define (fc-mix-fp2-test-pp2)
  (pretty-print (intrp fc-mix-fp2-pp (list vs0-tm2)))
)

; Good
(define (fc-mix-fp2-test-pp3)
  (pretty-print (intrp fc-mix-fp2-pp (list (list (pair 'prog tm-goto)))))
)

;
;
; Test mixer on Flow Chart interpreter on Flow Chart
;
;

(define div-fc (list
  'prog
  '(cdadr prog)
  '(cdar prog)

  'bb
  '(car bb)
  '(cdr bb)
  '(null? bb)

  'stmt
  '(equal? ':= (car stmt))
  '(equal? 'goto (car stmt))
  '(equal? 'if (car stmt))
  '(equal? 'return (car stmt))
  '(second stmt)
  '(third stmt)
  '(fourth stmt)
  '(lookup prog (second stmt))
  '(lookup prog (third stmt))
  '(lookup prog (fourth stmt))
))

(define vs0-fc (list
  (pair 'prog find-name)))

(define vs0-mix-fc (list
  (pair 'program intrp-on-fc)
  (pair 'div div-fc)))

;
; I Futamura projection for intrp-on-fc
;
(define (fc-mix-fp1-fc-new vs0)
  (run-mix-new (list intrp-on-fc div-fc vs0)))

(define (fc-fp1-test-pp)
  (pretty-print (fc-mix-fp1-fc-new vs0-fc)))

(define (fc-fp1-test-pp-run)
  (intrp fc-test-pp (list (list 'c '(a b c) '(1 2 3)))))
  
;
; II Futamura projection for intrp-on-fc
;
  
; Compiler
(define fc-mix-fp2-fc-new
  (pretty-print (fc-mix-fp2 div-mix vs0-mix-fc)))

;
; The same problem as at DEATH example.
;
(define (fc-fp2-test-pp)
  (pretty-print (intrp fc-mix-fp2-fc-new (list vs0-fc))))

;
; III Futamura projection for intrp-on-fc
;

(define vs0-mix-mix (list
  (pair 'program fc-mix-new)
  (pair 'div     div-mix)))

(define fc3 (fc-mix-fp2 div-mix vs0-mix-mix))

;(define r (fc-fp2-test-pp))
(require racket/pretty)
(pretty-print fc3)
