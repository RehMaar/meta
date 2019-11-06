(load "tm_intrp.rkt")
(load "fc_intrp_on_fc.rkt")
(load "fc_tricky_mix.rkt")

;
; I Futamura projection
;

; New mixer
(define (fc-mix-fp1-new div vs0)
  (run-mix-new (list tm-intrp div vs0)))

; Old mixer
(define (fc-mix-fp1 div vs0)
  (run-mix (list tm-intrp div vs0)))

(define (fc-mix-fp1-off div vs0)
  (run-mix-new (list tm-intrp-official div vs0)))

; Division for TM interpreter.
(define div-tm (list
  'prog
  '(caar prog)

  'next-label
  '(+ 1 next-label)
  '(lookup prog next-label)
  '(not (has-label next-label prog))

  'instr
  '(car instr)
  '(cadr instr)
  '(cadddr instr)
  '(equal? (car instr) 'goto)
  '(equal? (car instr) 'left)
  '(equal? (car instr) 'right)
  '(equal? (car instr) 'if)
  '(equal? (car instr) 'write)


))

(define div-tm-official (list
  'Q

  'Qtail
  '(car Qtail)
  '(cdr Qtail)
  '(null? Qtail)

  'Instruction
  '(cadr Instruction)
  '(caddr Instruction)
  '(third Instruction)
  '(fifth Instruction)

  'Operator
  '(equal? Operator 'right)
  '(equal? Operator 'left)
  '(equal? Operator 'write)
  '(equal? Operator 'goto)
  '(equal? Operator 'if)

  'NextLabel
  '(list-tail Q NextLabel)
))

(define vs0-tm (list
  (pair 'prog tm-prog)))

;; test
(define (fc-mix-fp1-test-new)
  (pretty-print (fc-mix-fp1-new div-tm vs0-tm)))

(define (fc-mix-fp1-test)
  (pretty-print (fc-mix-fp1 div-tm vs0-tm)))

(define (fc-mix-fp1-test-off)
  (pretty-print (fc-mix-fp1-off div-tm-official (list (pair 'Q tm-prog)))))

; test 2
(define vs0-tm2 (list
  (pair 'prog tm-write)))

(define (fc-mix-fp1-test-pp-2-new)
  (pretty-print (fc-mix-fp1-new div-tm vs0-tm2)))

(define (fc-mix-fp1-test-pp-2)
  (pretty-print (fc-mix-fp1 div-tm vs0-tm2)))

(define (fc-mix-fp1-test-pp-2-off)
  (pretty-print (fc-mix-fp1-off div-tm-official (list (pair 'Q tm-write )))))

; test 3
(define vs0-tm3 (list
  (pair 'prog tm-goto)))

(define (fc-mix-fp1-test-pp-3-new)
  (pretty-print (fc-mix-fp1-new div-tm vs0-tm3)))

(define (fc-mix-fp1-test-pp-3)
  (pretty-print (fc-mix-fp1 div-tm vs0-tm3)))

(define (fc-mix-fp1-test-pp-3-off)
  (pretty-print (fc-mix-fp1-off div-tm-official (list (pair 'Q tm-goto)))))
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

(define vs0-mix-tm (list
  (pair 'program tm-intrp)
  (pair 'div div-tm)
))

(define vs0-mix-tm-official (list
  (pair 'program tm-intrp-official)
  (pair 'div div-tm-official)
))

(define (fc-mix-fp2 div vs)
  (run-mix-new (list fc-mix-new div vs)))


;
; Generate a compiler.
;
(define (fc-mix-fp2-tm)
  (pretty-print (fc-mix-fp2 div-mix vs0-mix)))

(define (fc-mix-fp2-tm-off)
  (pretty-print (fc-mix-fp2 div-mix vs0-mix-tm-official)))

;
;;
;; *Check that this is really a compiler.
;;
;
; Compile several TM programs.
;

; Good

(define (fc-mix-fp2-test-pp)
  (pretty-print (intrp (fc-mix-fp2-tm) (list vs0-tm)))
)

(define (fc-mix-fp2-test-pp-off)
  (pretty-print (intrp (fc-mix-fp2-tm-off) (list (list (pair 'Q tm-prog))))))

(define (fc-mix-fp2-test-pp-if)
  (pretty-print (intrp (fc-mix-fp2-tm) (list (list (pair 'prog tm-if))))))


(define (fc-mix-fp2-test-pp-if-off)
  (pretty-print (intrp (fc-mix-fp2-tm-off) (list (list (pair 'Q tm-if))))))

(define (fc-mix-fp2-test-pp-wg)
  (pretty-print (intrp (fc-mix-fp2-tm) (list (list (pair 'prog tm-write-goto))))))

(define (fc-mix-fp2-test-pp-wg-off)
  (pretty-print (intrp (fc-mix-fp2-tm-off) (list (list (pair 'Q tm-write-goto))))))

; Good
(define (fc-mix-fp2-test-pp2)
  (pretty-print (intrp (fc-mix-fp2-tm) (list vs0-tm2)))
)

(define (fc-mix-fp2-test-pp2-off)
  (pretty-print (intrp (fc-mix-fp2-tm-off) (list (list (pair 'Q tm-write)))))
)

; Good
(define (fc-mix-fp2-test-pp3)
  (pretty-print (intrp (fc-mix-fp2-tm) (list (list (pair 'prog tm-goto)))))
)

(define (fc-mix-fp2-test-pp3-off)
  (pretty-print (intrp (fc-mix-fp2-tm-off) (list (list (pair 'Q tm-goto)))))
)

;
; Test mixer on Flow Chart interpreter on Flow Chart
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

(define (fc-mix-fp1-fc-new)
  (pretty-print (fc-mix-fp1-fc-new vs0-fc)))

(define (fc-fp1-test-pp-run)
  (intrp (fc-mix-fp1-fc-new) (list (list 'c '(a b c) '(1 2 3)))))
  
;
; II Futamura projection for intrp-on-fc
;
  
; Compiler
(define (fc-mix-fp2-fc-new)
  (pretty-print (fc-mix-fp2 div-mix vs0-mix-fc)))

(define (fc-fp2-test-pp)
  (pretty-print (intrp (fc-mix-fp2-fc-new) (list vs0-fc))))

;
; III Futamura projection for intrp-on-fc
;

(define vs0-mix-mix (list
  (pair 'program fc-mix-new)
  (pair 'div     div-mix)))

; Takes a hella long time
(define (fc3) (fc-mix-fp2 div-mix vs0-mix-mix))

; *Official* tests, but better use the ones at the bottom.

; TM compiler
(define (fc3-tm) (intrp (fc3) (list vs0-mix-tm)))

; FC compiler
(define (fc3-fc) (intrp (fc3) (list vs0-mix-fc)))

; Compile tm code
(define (fc3-tm-example) (intrp (fc3-tm) (list vs0-tm)))

; Compile fc code
(define (fc3-fc-example) (intrp (fc3-fc) (list vs0-fc)))

;
; Generated generator
;
(load "generated_fp3.rkt")

; Better use these tests.

; TM compiler
(define (fc3-tm-r) (pretty-print (intrp fc3r (list vs0-mix-tm))))

; FC compiler
(define (fc3-fc-r) (pretty-print (intrp fc3r (list vs0-mix-fc))))

; Compile tm code
(define (fc3-tm-example-r) (pretty-print (intrp (fc3-tm-r) (list vs0-tm))))

; Compile fc code
(define (fc3-fc-example-r) (pretty-print (intrp (fc3-fc-r) (list vs0-fc))))


; Print with newlines and tabs

;(define f (fc-mix-fp2-tm))
;(define f (fc-mix-fp2-fc-new))
;(define f (pretty-print (fc3)))
;(define f (pretty-print (fc3-tm)))
;(define f (pretty-print (fc3-fc)))
;(require racket/pretty)
;(pretty-print f)
