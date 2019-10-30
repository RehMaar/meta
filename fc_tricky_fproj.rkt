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

  'Symbol

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
(define (fc-mix-fp2-pp)
  (pretty-print (fc-mix-fp2 div-mix vs0-mix)))

(define (fc-mix-fp2-pp-off)
  (pretty-print (fc-mix-fp2 div-mix vs0-mix-tm-official)))

;
;;
;; *Check that this is really a compiler.
;;
;
; Compile several TM programs.
;

; DEATH
;
; Program tm-goto (with 'if and 'goto)

(define (fc-mix-fp2-test-pp)
  (pretty-print (intrp (fc-mix-fp2-pp) (list vs0-tm)))
)

(define (fc-mix-fp2-test-pp-off)
  (pretty-print (intrp (fc-mix-fp2-pp-off) (list (list (pair 'Q tm-prog))))))

; Programs with only `if doesn't work
(define (fc-mix-fp2-test-pp-if)
  (pretty-print (intrp (fc-mix-fp2-pp) (list (list (pair 'prog tm-if))))))

; New interpreter isn't better!
; It isn't even smaller!
(define (fc-mix-fp2-test-pp-if-off)
  (pretty-print (intrp (fc-mix-fp2-pp-off) (list (list (pair 'Q tm-if))))))

; Works fine with only 'goto
(define (fc-mix-fp2-test-pp-wg)
  (pretty-print (intrp (fc-mix-fp2-pp) (list (list (pair 'prog tm-write-goto))))))

(define (fc-mix-fp2-test-pp-wg-off)
  (pretty-print (intrp (fc-mix-fp2-pp-off) (list (list (pair 'Q tm-write-goto))))))

; Good
(define (fc-mix-fp2-test-pp2)
  (pretty-print (intrp (fc-mix-fp2-pp) (list vs0-tm2)))
)

(define (fc-mix-fp2-test-pp2-off)
  (pretty-print (intrp (fc-mix-fp2-pp-off) (list (list (pair 'Q tm-write)))))
)

; Good
(define (fc-mix-fp2-test-pp3)
  (pretty-print (intrp (fc-mix-fp2-pp) (list (list (pair 'prog tm-goto)))))
)

(define (fc-mix-fp2-test-pp3-off)
  (pretty-print (intrp (fc-mix-fp2-pp-ff) (list (list (pair 'Q tm-goto)))))
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
(define (fc-mix-fp2-fc-new)
  (pretty-print (fc-mix-fp2 div-mix vs0-mix-fc)))

;
; The same problem as at DEATH example.
;
(define (fc-fp2-test-pp)
  (pretty-print (intrp (fc-mix-fp2-fc-new) (list vs0-fc))))

;
; III Futamura projection for intrp-on-fc
;

(define vs0-mix-mix (list
  (pair 'program fc-mix-new)
  (pair 'div     div-mix)))

; Generate some strange SHIT
(define (fc3) (fc-mix-fp2 div-mix vs0-mix-mix))

; Compilers below both falls with 'contract violation'

; TM compiler
(define (fc3-tm) (intrp fc3 (list vs0-mix-tm)))

; FC compiler
(define (fc3-fc) (intrp fc3 (list vs0-mix-fc)))

; Compile tm code
(define (fc3-tm-example) (intrp (fc3-tm) (list vs0-tm)))

; Compile fc code
(define (fc3-tm-example) (intrp (fc3-fc) (list vs0-fc)))


; Print with newlines and tabs
;(define f (fc-fp2-test-pp))
;(require racket/pretty)
;(pretty-print f)
  
(define fc-debug
'((read vs0)
  ((label 29)
   (:= pending (list (pair 'init vs0)))
   (:= marked '())
   (:= residual-code (list (generate-read '(read prog tape-right) vs0)))
   (:= pp (caar pending))
   (:= vs (cdar pending))
   (:= pending (cdr pending))
   (:= label (pair pp vs))
   (if (elem? label marked) (label 28) (label 25)))
  ((label 28) (if (null? pending) (label 27) (label 26)))
  ((label 27) (return (reverse residual-code)))
  ((label 26)
   (:= pp (caar pending))
   (:= vs (cdar pending))
   (:= pending (cdr pending))
   (:= label (pair pp vs))
   (if (elem? label marked) (label 28) (label 25)))
  ((label 25)
   (:= marked (cons (pair pp vs) marked))
   (:= code-block '())
   (:= vs (update vs 'next-label (reduce '(caar prog) vs)))
   (:= code-block (extend (generate-assign 'tape-left ''() vs) code-block))

   (:= vs (update vs 'instr (reduce '(lookup prog next-label) vs)))
          
   (:= display (begin (display "LABEL 25 INSTR: ") (display-nl (lookup vs 'instr))))

   (if (reduce '(equal? (car instr) 'goto) vs) (label 24) (label 18)))

  ((label 24)
   (:= vs (update vs 'next-label (reduce '(cadr instr) vs)))

   (:= display (begin (display "LABEL 24 (GOTO) NEXT-LABEL: ") (display-nl (lookup vs 'next-label))))

   (if (reduce '(not (has-label next-label prog)) vs) (label 23) (label 16)))
        

  ((label 23)
   (:=
    code-block
    (extend (generate-return '(cons tape-left tape-right) vs) code-block))
   (:= residual-code (cons (cons label (reverse code-block)) residual-code))
   (if (null? pending) (label 22) (label 21)))
  ((label 22) (return (reverse residual-code)))
  ((label 21)
   (:= pp (caar pending))
   (:= vs (cdar pending))
   (:= pending (cdr pending))
   (:= label (pair pp vs))
   (if (elem? label marked) (label 20) (label 19)))
  ((label 20) (if (null? pending) (label 22) (label 21)))
  ((label 19)
   (:= marked (cons (pair pp vs) marked))
   (:= code-block '())
   (:= vs (update vs 'next-label (reduce '(caar prog) vs)))
   (:= code-block (extend (generate-assign 'tape-left ''() vs) code-block))
   (:= vs (update vs 'instr (reduce '(lookup prog next-label) vs)))
   (if (reduce '(equal? (car instr) 'goto) vs) (label 24) (label 18)))

  ((label 18)

   (:= debug (begin (display "LABEL 18: check left: ") (display (lookup vs 'instr)) (display-nl (reduce '(equal? (car instr) 'left) vs))))

   (if (reduce '(equal? (car instr) 'left) vs) (label 17) (label 15)))

  ((label 17)
   (:=
    code-block
    (extend
     (generate-assign 'tape-right '(cons (tape-head tape-left) tape-right) vs)
     code-block))
   (:=
    code-block
    (extend (generate-assign 'tape-left '(tape-tail tape-left) vs) code-block))
   (:= vs (update vs 'next-label (reduce '(+ 1 next-label) vs)))
   (if (reduce '(not (has-label next-label prog)) vs) (label 23) (label 16)))
  ((label 16)
   (:= vs (update vs 'instr (reduce '(lookup prog next-label) vs)))
   (if (reduce '(equal? (car instr) 'goto) vs) (label 24) (label 18)))
  ((label 15)
   (:= debug (begin (display "LABEL 15: check right: ") (display-nl (reduce '(equal? (car instr) 'right) vs))))
   (if (reduce '(equal? (car instr) 'right) vs) (label 14) (label 13)))
  ((label 14)
   (:=
    code-block
    (extend
     (generate-assign 'tape-left '(cons (tape-head tape-right) tape-left) vs)
     code-block))
   (:=
    code-block
    (extend
     (generate-assign 'tape-right '(tape-tail tape-right) vs)
     code-block))
   (:= vs (update vs 'next-label (reduce '(+ 1 next-label) vs)))
   (if (reduce '(not (has-label next-label prog)) vs) (label 23) (label 16)))
  ((label 13)
      (:= debug (begin (display "LABEL 13: check if: ") (display-nl (reduce '(equal? (car instr) 'if) vs))))
      (if (reduce '(equal? (car instr) 'if) vs) (label 12) (label 7)))
  ((label 12)
   (:= debug (begin (display "LABEL 12: pending: ") (display-nl vs)))

   (:= pending (unite (list (pair 'handle-if-goto vs) (pair 'handle-if-next vs)) pending))
   (:= code-block (extend (generate-if '(equal? (tape-head tape-right) (cadr instr)) vs 'handle-if-goto 'handle-if-next) code-block))
   (:= residual-code (cons (cons label (reverse code-block)) residual-code))
   (if (null? pending) (label 11) (label 10)))

  ((label 11) (return (reverse residual-code)))
  ((label 10)
   (:= pp (caar pending))
   (:= vs (cdar pending))
   (:= pending (cdr pending))
   (:= label (pair pp vs))
   (if (elem? label marked) (label 9) (label 8)))
  ((label 9) (if (null? pending) (label 11) (label 10)))
  ((label 8)
   (:= debug (display-nl "LABEL 8"))


   (:= marked (cons (pair pp vs) marked))
   (:= code-block '())

   (:= debug (display-nl "BEFORE"))
   (:= debug (display-nl (reduce '(caar prog) vs)))
   (:= debug (display-nl vs))
   (:= debug (display-nl (reduce '(lookup prog next-label) vs)))           

   (:= vs (update vs 'next-label (reduce '(caar prog) vs)))
   (:= code-block (extend (generate-assign 'tape-left ''() vs) code-block))
   (:= vs (update vs 'instr (reduce '(lookup prog next-label) vs)))
   
   (:= debug (display-nl "AFTER"))
   (:= debug (display-nl vs))
   (:= debug (display-nl (reduce '(lookup prog next-label) vs)))          

   (if (reduce '(equal? (car instr) 'goto) vs) (label 24) (label 18)))
  ((label 7) (if (reduce '(equal? (car instr) 'write) vs) (label 6) (label 5)))
  ((label 6)
   (:=
    code-block
    (extend
     (generate-assign 'tape-right '(tape-write (cadr instr) tape-right) vs)
     code-block))
   (:= vs (update vs 'next-label (reduce '(+ 1 next-label) vs)))
   (if (reduce '(not (has-label next-label prog)) vs) (label 23) (label 16)))
  ((label 5)
   (:= code-block (extend (generate-return ''ERROR vs) code-block))
   (:= residual-code (cons (cons label (reverse code-block)) residual-code))
   (if (null? pending) (label 4) (label 3)))
  ((label 4) (return (reverse residual-code)))
  ((label 3)
   (:= pp (caar pending))
   (:= vs (cdar pending))
   (:= pending (cdr pending))
   (:= label (pair pp vs))
   (if (elem? label marked) (label 2) (label 1)))
  ((label 2) (if (null? pending) (label 4) (label 3)))
  ((label 1)
   (:= debug (display-nl "LABEL 1"))
   (:= marked (cons (pair pp vs) marked))
   (:= code-block '())
   (:= vs (update vs 'next-label (reduce '(caar prog) vs)))
   (:= code-block (extend (generate-assign 'tape-left ''() vs) code-block))
   (:= vs (update vs 'instr (reduce '(lookup prog next-label) vs)))
   (if (reduce '(equal? (car instr) 'goto) vs) (label 24) (label 18))))
)

(define (debug)
  (pretty-print (intrp fc-debug (list vs0-tm))))
