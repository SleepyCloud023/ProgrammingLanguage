#lang racket
(provide (all-defined-out))

(define (extract xs)
  (if (empty? xs)
      empty
      (let ([center (car xs)]
            [left (car (cdr xs))]
            [right (car (cddr xs))]
       )
      (cond [(number? center)
             (append (extract left) (append (list center) (extract right)))]
            )
        )
      )
  )

(define (check_ascending xs)
  (cond [(empty? xs) #t]
        [(and (number? (car xs)) (empty? (cdr xs))) #t]
        [#t (and (< (car xs) (car (cdr xs))) (check_ascending (cdr xs)))]
        )
)

(define (proper xs)
  (cond [(empty? xs) #t]
        [(and (number? (car xs)) (list? (car (cdr xs))) (list?(car (cddr xs))) )
         (and (proper (car (cdr xs))) (proper (car (cddr xs))) )]
        [#t #f]
        )
  )
  

(define (check_bst xs)
  (cond [(empty? xs) #t]
        [(and (proper xs) (check_ascending (extract xs)))
      (let ([center (car xs)]
            [left (car (cdr xs))]
            [right (car (cddr xs))]
            )
        (cond [(empty? xs) #t]
              [(and (number? center) (empty? left) (empty? right)) #t]
              [(and (number? center) (not (empty? left)) (empty? right))
               (and (< (car left) center) (check_bst left))]
              [(and (number? center) (not (empty? right)) (empty? left))
               (and (< center (car right)) (check_bst right))]
              [(and (number? center) (not (empty? left)) (not (empty? right)))
               (and (< (car left) center) (< center (car right)) (check_bst left) (check_bst right))]
              [(and (not (number? (car xs))) (not (empty? (car xs)))) #f]
              [#t #f]
              )
        )]
        [#t #f]
      )
)
          
(define (apply f xs)
  (if (empty? xs)
      empty
      (let ([center (car xs)]
            [left (car (cdr xs))]
            [right (car (cddr xs))]
        )
      (cond [(empty? xs)]
            [(number? center)
             (list (f center) (apply f left) (apply f right))]
            )
      )
  )
 )

(define (list_equal as bs)
  (cond [(empty? as) (empty? bs)]
        [(empty? bs) (empty? as)]
        [#t (and (= (car as) (car bs) ) (list_equal (cdr as) (cdr bs)))]
        )
  )

(define (equals as bs)
  (let ([first (extract as)]
        [second (extract bs)]
        )
    (list_equal first second)
    )
  )
    
;(define (f x) (* x x))
;(define a ' (8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
;(define b '(7 (6 ()(10 () ())) (8 ()()))) ; not BST
;(define c '(6 () (7 () ())))
;(define d '(7 (6 ()()) (8 ()(10 () ()))))
;(define e '(6 (-3 () (-1 () ())) (7 () (8 () (10 () ())))))
;(define k '(6 () (8 (6 () ()) ())))


