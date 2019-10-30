; #lang racket/load

(load "fc_intrp.rkt")

; (define (interp-tm tm-prog tm-intput))

; (define (fc-intrp tm-prog tm-tape) 10)

;
; alph: 0, 1, <space>
;   Here space is an empty list '()
;
; instr: left, right, write <alph>,
;        goto <index>, if <alph> goto <index>
;
; prog: [instr]
;
(define tm-intrp
  '((read prog tape-right)
    (init
      ; next-label is the label of the instruction we need to execute.
      ; We know that a label is a number, so to get the next one we need just to `inc`
      ; the previous value
      (:= next-label (caar prog))

      ; head of `tape-right` is where we are now
      ; init left side of the tape with empty list
      (:= tape-left '())
      (goto next)
    )

    (next
      ; stmt is the currently handled line (<instr>)
      (:= instr (lookup prog next-label))
      (goto loop)
    )
    
    ;  match command
    (loop
      ; ignore label
      (if (equal? (car instr) 'goto) handle-goto try-match-left)
    )
    (try-match-left  
     (if (equal? (car instr) 'left) handle-left try-match-right)
    )
    (try-match-right
     (if (equal? (car instr) 'right) handle-right try-match-if)
    )
    (try-match-if
     (if (equal? (car instr) 'if) handle-if try-match-write)
    )
    (try-match-write
     (if (equal? (car instr) 'write) handle-write error)
    )

    ; handlers
    (handle-right
      (:= tape-left (cons (tape-head tape-right) tape-left))
      (:= tape-right (tape-tail tape-right))
      (:= next-label (+ 1 next-label))
      (goto check-next)
    )

    (handle-left
      (:= tape-right (cons (tape-head tape-left) tape-right))
      (:= tape-left (tape-tail tape-left))
      (:= next-label (+ 1 next-label))
      (goto check-next)
    )

    ; <write <symbol>>
    (handle-write 
			(:= tape-right (tape-write (cadr instr) tape-right))
      (:= next-label (+ 1 next-label))
      (goto check-next)
    )

    ; <if <symbol> goto <index>>
    (handle-if    
      (if (equal? (tape-head tape-right) (cadr instr))
        handle-if-goto
        handle-if-next
      )
    )
    (handle-if-goto
			(:= next-label (cadddr instr))
      (goto check-next)
    )
    (handle-if-next
			(:= next-label (+ 1 next-label))
      (goto check-next)
    )

    ; <goto <index>>
    (handle-goto  
      (:= next-label (cadr instr))
      (goto check-next)
    )
          
    ; utility blocks
    (check-next
      (if (not (has-label next-label prog)) exit next))
    (error
      (return 'ERROR))
    ; we want to see the whole tape for the result
    (exit
      (return (cons tape-left tape-right))
    )
   )
)

(define tm-intrp-official
 '((read Q Right)
   (init
    (:= Qtail Q)
    (:= Left '())
    (goto loop))
   (loop
    (if (null? Qtail) stop cont))
   (cont
    (:= Instruction (car Qtail))
    (:= Qtail (cdr Qtail))
    (:= Operator (cadr Instruction))
    (if (equal? Operator 'right) do-right cont1))
   (cont1
     (if (equal? Operator 'left) do-left cont2))
   (cont2
     (if (equal? Operator 'write) do-write cont3))
   (cont3
     (if (equal? Operator 'goto) do-goto cont4))
   (cont4
     (if (equal? Operator 'if) do-if error))
   (do-right
     (:= Left (cons (car Right) Left))
     (:= Right (cdr Right))
     (goto loop))
   (do-left
     (:= Right (cons (car Left) Right))
     (:= Left (cdr Left))
     (goto loop))
   (do-write
     (:= Symbol (caddr Instruction))
     (:= Right (cons Symbol (cdr Right)))
     (goto loop))
   (do-goto
     (:= NextLabel (caddr Instruction))
     (:= Qtail (list-tail Q NextLabel))
     (goto loop))
   (do-if
     (:= Symbol (third Instruction))
     (:= NextLabel (fifth Instruction))
     (if (equal? Symbol (car Right)) jump loop))
   (jump
    (:= Qtail (list-tail Q NextLabel))
    (goto loop))
   (stop (return (cons Left Right)))
   (error
     (return "SYNTAX ERROR"))
  )
)

(define (tape-head xs)
  (if (null? xs) '() (car xs))
)

(define (tape-tail xs)
  (if (null? xs) '(()) (cdr xs))
)

(define (tape-write x xs)
  (cons x (tape-tail xs))
)

(define (has-label label prog)
  (if (findf (lambda (kv) (equal? label (car kv))) prog) #t #f)
)

(define (run-tm prog tape)
  (intrp tm-intrp (list prog tape))
)

(define (run-tm-display prog tape)
  (let ([tape (run-tm prog tape)])
    (begin
      (display "Left side: ")
      (display (car tape))
      (display "\nRight size: ")
      (display (cdr tape))
      (display "\n")
      tape
    )
  )
)

; example tm programs
(define tm-prog
  '((0 if 0 goto 3)
    (1 right)
    (2 goto 0)
    (3 write 1)
   )
)

(define tm-prog2
  '((0 goto 1)
    (1 if 0 goto 3)
    (2 right)
    (3 goto 0)
    (4 write 1)
   )
)

(define tm-write
  '((0 write 0))
)

(define tm-write-goto
  '((0 goto 1)
    (1 write 32))
)

(define tm-if
  '((0 write 0)
    (1 if 0 goto 3)
    (2 write 1)
    (3 right)
    (4 write 0)
   )
)

(define tm-if2
  '((0 write 1)
    (1 if 0 goto 3)
    (2 write 1)
    (3 right)
    (4 write 0)
   )
)

; write 1
; [_, 1, _]
;     ^
; right
; [_, 1, _]
;        ^
; write 2
; [_, 1, 2]
;        ^
; right
; [_, 1, 2, _]
;           ^
; write 3
; [_, 1, 2, 3]
;           ^
; left
; [_, 1, 2, 3]
;        ^
(define tm-goto
  '((0 write 1)
    (1 right)
    (2 write 2)
    (3 right)
    (4 write 3)
   )
)
