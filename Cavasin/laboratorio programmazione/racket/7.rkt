;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define prime-factors
  (lambda (n)
    (if (> n 1)
        (let ((closest-divisible-prime (closest-divisible-prime n n)))
          (append (prime-factors (/ n closest-divisible-prime)) (list closest-divisible-prime))
          )
        null
        )
    )
  )

(define closest-divisible-prime
  (lambda (n v)
    (if (> n 1)
        (let ((closest-prime (closest-prime n)))
          (if (= (remainder v closest-prime) 0)
              closest-prime
              (closest-divisible-prime (- closest-prime 1) v)
              )
          )
        1
        )
    )
  )

(define closest-prime
  (lambda (n)
    (if (> n 1)
        (if (is-prime n)
            n
            (closest-prime (- n 1))
            )
        1
        )
    )
  )

(define is-prime
  (lambda (n)
    (if (is-divisible n (floor (sqrt n)))
        #false
        #true
        )
    )
  )

(define is-divisible
  (lambda (n v)
    (if (< v 2)
        #false
        (if (= (remainder n v) 0)
            #true
            (is-divisible n (- v 1))
            )
        )
    )
  )

(define short-prime-factors
  (lambda (n)
    (remove-duplicates (prime-factors n))
    )
  )

(define remove-duplicates
  (lambda (l)
    (duplicate-remover l 0)
    )
  )

(define duplicate-remover
  (lambda (l v)
    (if (null? l)
        null
        (if (= (car l) v)
            (duplicate-remover (cdr l) v)
            (append (list (car l)) (duplicate-remover (cdr l) (car l)))
            )
        )
    )
  )

(define prime-facts-degs
  (lambda (n)
    (count-duplicates (prime-factors n))
    )
  )

(define count-duplicates
  (lambda (l)
    (duplicate-counter l (car l) 0)
    )
  )

(define duplicate-counter
  (lambda (l v c)
    (if (null? l)
        (list (list v c))
        (if (= (car l) v)
            (duplicate-counter (cdr l) v (+ c 1))
            (append (list (list v c)) (duplicate-counter (cdr l) (car l) 1))
            )
        )
    )
  )

(prime-factors 7)
(prime-factors 9)
(prime-factors 28)
(prime-factors 39)
(prime-factors 540)
(prime-factors 1617)
"----------------------------"
(short-prime-factors 7)
(short-prime-factors 39)
(short-prime-factors 9)
(short-prime-factors 540)
(short-prime-factors 28)
(short-prime-factors 1617)
"----------------------------"
(prime-facts-degs 7)
(prime-facts-degs 39)
(prime-facts-degs 9)
(prime-facts-degs 28)
(prime-facts-degs 540)
(prime-facts-degs 1617)
