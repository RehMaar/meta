;
; Utility functions
;

(define (id x) x)

;
; Debug-print-development.
;
(define (flip-const a b) b)
(define (debug-print str expr)
  (flip-const 
    0
    ;(display str)
    expr))

; zip two lists
(define (zip xs ys) (map cons xs ys))

; unzip
(define (unzip xs)
  (if (null? xs) (pair '() '())
    (match (car xs)
      [`(,fst . ,snd)
        (match (unzip (cdr xs))
          [`(,fs . ,ss) (pair (cons fst fs) (cons snd ss))]
        )
      ]
    )
))

; return #t, if all elements of the 'xs' are true
; return #f, if at least one element in the list is false
(define (all xs) (foldr (lambda (v acc) (and v acc)) #t xs))

(define (elem? x xs)
  (cond
    [(null? xs) #f]
    [(equal? x (car xs)) #t]
    [else (elem? x (cdr xs))]
))

;
; Get value by key from a dictionary.
;
; *Dictionary* is a list of key-value pairs.
;
;(define (lookup dict key)
;  (let ([value (findf (lambda (key-value) (equal? key (car key-value))) dict)])
;    (if value
;      (cdr value)
;      ;(error "Couldn't find key in the given dict." key)
;      key
;    )
;  )
;)
;
;(define (key? dict key)
;  (let ([value (findf (lambda (key-value) (eq? key (car key-value))) dict)])
;    (if value #t #f))
;)
;
;(define (update dict key value)
;	(cond
;    [(null? dict) (cons (pair key value) dict)]
;    [(equal? key (caar dict)) (cons (pair key value) (cdr dict))]
;    [else (cons (car dict) (update (cdr dict) key value))]
;  )
;)

; dict -- list of key-value pairs
(define (init-dict dict)
  dict
  ;(make-immutable-hash dict)
)

(define (lookup dict key)
  (let ([value (findf (lambda (key-value) (equal? key (car key-value))) dict)])
    (if value
      (cdr value)
      ;(error "Couldn't find key in the given dict." key)
      (begin
       (display "WARNING: key wasn't found:")
       (display key)
       (display "\n")
      key)
    )
  )
)

(define (key? dict key)
  (let ([value (findf (lambda (key-value) (equal? key (car key-value))) dict)])
    (if value #t #f))
)

(define (update dict key value)
	(cond
    [(null? dict) (cons (pair key value) dict)]
    [(equal? key (caar dict)) (cons (pair key value) (cdr dict))]
    [else (cons (car dict) (update (cdr dict) key value))])
)

(define (elem? x xs)
  (cond
    [(null? xs) #f]
    [(equal? x (car xs)) #t]
    [else (elem? x (cdr xs))])
)

(define (pair f s) `(,f . ,s))

(define (unite lst1 lst2) (append lst1 lst2))

(define (snd f p)
  (match p
    [`(,ft . ,sd) `(,ft . ,(f sd))]
    [else (error "snd: argument is not a pair!")]
  ))
