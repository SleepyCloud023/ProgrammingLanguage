#lang racket

(define x 3)
(define y (+ x 2))

(define (cube x)
  (* x x x))

(define pow
  (lambda (x)
    (lambda (y)
      (if (= y 0)
          1
          (* x ((pow x) (- y 1)) )))))

(define (sum xs)
  (cond [(null? xs) 0]
        [(number? xs) xs]
        [(list? xs) (+ (sum (car xs)) (sum (cdr xs)))]
        [#t 0]
        ))

(define (check_bst xs)
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
        [#t #f]
        )
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
    

(define a ' (8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
(define b '(7 (6 ()()) (8 ()())))
(define c '(6 () (7 () (8 ()()))))
        
      
      



