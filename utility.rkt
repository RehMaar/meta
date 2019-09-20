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

; return #t, if all elements of the 'xs' are true
; return #f, if at least one element in the list is false
(define (all xs) (foldr (lambda (v acc) (and v acc)) #t xs))

;
; Get value by key from a dictionary.
;
; *Dictionary* is a list of key-value pairs.
;
(define (lookup dict key)
  (let ([value (findf (lambda (key-value) (eq? key (car key-value))) dict)])
    (if value
      (cdr value)
      (error "Couldn't find key in the given dict." key)
    )
  )
)
