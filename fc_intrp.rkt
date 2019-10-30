; #lang racket

(require racket/trace)

(load "utility.rkt")

;
; Auxiliary functions.
;

; return block by its label
(define (find-block label blocks)
  (let ([result (findf (lambda (x) (equal? (car x) label)) blocks)])
    (if (equal? result #f)
      (error "No such block with a label " label)
      result
    )
  )
)

; return a label of the basic block
(define (block-label block) (car block))

; return a block of statements of the basic block
(define (block-stmts  block) (cdr block))



;
; Build initial context from a read statement and a program's input.
; Just creates a map from variables in the given read statement
; to corresponding values from the input.
;
; *Context* is a dictionary (aka a list of pairs (key, value)).
;
(define (build-ctx stmt input)
  (match stmt
    [`(read . ,xs) (init-dict (zip xs input))]
    [ _  (error "Expect `read` command, but got:" stmt)]
  )
)

; update context with new pair (key, value)
(define (update-ctx ctx key value)
  (update ctx key value)
  ;(cons `(,key . ,value) ctx)
)


;
; Predicates on FlowChart's statements.
;
; For *pattern-match* and validation (at least to catch some stupid errors).
;

;
; The only way to detect operators is to check
; the expression is a list.
;
(define (operator? x) (list? x))

;
; Constant is a quoted value.
;
; The problem is to differ constants from operators.
;
(define (constant? x)
  (match x
    [`(quote ,x) #t]
    [else #f]
  )
)


; By definition, a context is a list of pairs.
(define (ctx? ctx)
  #t
  ;(and (list? ctx) (andmap pair? ctx))
)

;
; Just try to pattern-match all available "constructors".
;
(define (stmt? stmt)
  (and (list? stmt)
    (match stmt
      [`(:= ,var ,expr) #t]
      [`(goto ,label) #t]
      [`(return ,expr) #t]
      [`(if ,expr ,label1 ,label2) #t]
      [else #f]
    )
   )
)

; just check list of statements
(define (stmts? stmts)
  (and (list? stmts)
       (andmap stmt? stmts)
  )
)

; check basic block
(define (block? block)
  (and
    (match block
      [(list-rest label stmts) (stmts? stmts)]
      [else #f]
    )
    (and (list? block)
         ; we know that a block contains a label
         ; and at least one statement
         (< 1 (length block))
    )
  )
)

; check list of basic blocks
(define (blocks? blocks) 
  (and (list? blocks) 
       (andmap block? blocks)))

; check read statement
(define (read? entry)
  (match entry
    [`(read . ,lst) #t]
    [else #f]
  )
)

; check if the program is correct
(define (prog? prog)
  (and (read? (car prog))
    (blocks? (cdr prog))
  )
)

;
; *FlowChart interpreter implementation*
;

;
; prog -- a program; a list of statements: 
;   (read ..) or (label (<assigments?> <jump>))
;
; input -- a list of `prog` parameters.
;
(define (intrp prog input)
  (if (not (and (list? input) (prog? prog)))
    (error "intrp: `prog` must be a correct program and `input` must be a list!")

    (let ([ctx (build-ctx (car prog) input)]
          [blocks (cdr prog)])
      (intrp-block ctx blocks (car blocks))
    )
  )
)

;(define (intrp-ctx prog input) (build-ctx (car prog) input))


; block:
;  <label> : <assigment> ;
;             ...        ;
;            <assigment> ;
;            <jump>
;
; We keeps a immutable list of all blocks for jumps.
(define (intrp-block ctx blocks block)
  (if (not (and (blocks? blocks) (block? block)))
    (error "intrp-block: Bad input: <" blocks '> 'Block: '< block '>)
    
    ; we don't need a label of currently executed block
    (let ([stmts (cdr block)])
      (intrp-stmts ctx blocks stmts)
    )
  )
)

; assigment:
;   (:= <variable> <expr>)
;
; jump:
;   (return <expr>)
;   (goto <label>)
;   (if <expr> <label> <label>)
;
; So, a statement is an assigment or a jump.
;
(define (intrp-stmts ctx blocks stmts)
  (if (not (and (not (null? stmts)) 
           (and (ctx? ctx) (and (blocks? blocks) (stmts? stmts)))))
    (error "intrp-stmts: incorrect input!")

    (match (car stmts)
      [`(:= ,val ,expr)
        (intrp-stmts
          (update-ctx ctx val (intrp-expr ctx expr))
          blocks (cdr stmts))
      ]

      [jmp-block
        (intrp-jmp ctx blocks jmp-block)
      ]
    )
  )
)


;
; handle jumps
(define (intrp-jmp ctx blocks jmp-block)
  (if (not (and (ctx? ctx) (and (blocks? blocks) (stmt? jmp-block))))
    (error "intrp-jmp: bad input!")

    (match jmp-block
      [`(return ,expr) (intrp-expr ctx expr)]

      [`(goto ,label) (intrp-block ctx blocks (find-block label blocks))]

      [`(if ,expr ,goto1 ,goto2)
        (if (intrp-expr ctx expr)
          (intrp-block ctx blocks (find-block goto1 blocks))
          (intrp-block ctx blocks (find-block goto2 blocks)))
      ]
    )
  )
)

(define (intrp-expr ctx expr)
  (eval (intrp-expr-subst ctx expr))
)

;
; Need to pattern-match to understand what exactly
; and how exactly we must evaluate an expression.
;
(define (intrp-expr-subst ctx expr)
  (cond
    [(number?   expr) (eval expr)                     ]
    [(constant? expr)  expr                           ]
    [(operator? expr) (intrp-op ctx expr)             ]
    [else             (normalize (intrp-var ctx expr))]
))

(define (normalize e) (cons 'quote (list e)))

; return the result of the operation
(define (intrp-op ctx op-stmt)
  (if (not (and (ctx? ctx) (operator? op-stmt)))
    (error "intrp-op: bad input!")

    ; (car op-stmt) gives a name of an operator
    ; (cdr op-stmt) gives a list of arguments of the operator
    
    (let ([norm-op (cons (car op-stmt)
        (map
          (lambda (x) (intrp-expr-subst ctx x))
          (cdr op-stmt)
        )
      )])
      ; (cons 'quote (list (eval norm-op)))
      ;(eval norm-op)
      norm-op
    )
))

; just returns a value of given variable
(define (intrp-var ctx var) (lookup ctx var))


;
;;;; Trace execution.
;
;(trace intrp)
;(trace intrp-block)
;(trace intrp-stmts)
;(trace intrp-jmp)
;(trace intrp-expr)
;(trace intrp-var)
;(trace intrp-expr-subst)
;(trace intrp-op)

(define find-name 
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont
      (:= valuelist (cdr valuelist))
      (:= namelist (cdr namelist))
      (goto search)
    )
    (found (return (car valuelist)))
   )
)
