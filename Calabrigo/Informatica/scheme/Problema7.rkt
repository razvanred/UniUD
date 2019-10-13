;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Problema7) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
;dato n>1 dai in output una lista con la fattorizzazione ordinata di n (prime-factors)
;27 | 3
;9  | 3
;3  | 3
;1


(define prime-facts-degs
  (lambda (n)
    (cane n (primi 2 n))
    ))


(define cane
  (lambda (n primi)
    (if (or (null? primi) (= n 1))
        (list )
        (if (= (remainder n (car primi)) 0)
            (cons (list (car primi) (primeN n (car primi)))
                  (cane (quotient n (car primi)) (cdr primi)))
            (cane n (cdr primi)))
        )
    
    ))




;short-prime-factors
(define short-prime-factors
  (lambda(n)
    (can n (primi 2 n))
    ))


(define can
  (lambda (n primi)
    (if (or (null? primi) (= n 1))
        (list )
        (if (= (remainder n (car primi)) 0)
            (cons (car primi) (can (quotient n (car primi)) (cdr primi)))
            (can n (cdr primi)))
        )
    
    ))

;prime-factors
(define prime-factors
  (lambda (n)
    (ca n (primi 2 n))
    ))

(define ca
  (lambda (n primi)
    (if (or (null? primi) (= n 1))
        (list )
        (if (= (remainder n (car primi)) 0)
            (cons (car primi) (ca (quotient n (car primi)) primi))
            (ca n (cdr primi)))
        )
    
    ))
(define primeN;val: lista fattori primo
(lambda (n primo);in: n, primo    int

  (cond ((= (remainder n primo) 0)
         (+ (primeN (quotient n primo) primo) 1)
      )
      (else
           0))))
      

;procedura che ritorna una lista di numeri primi
(define primi ;val: lista di n primi
  (lambda (a n) ;n: int
    (if ( < a n)
        (if (isPrime? a)
            (cons a (primi (+ a 1) n))
            (primi (+ a 1) n))
        (list 2)
        )))

;è primo? divido n per s da n-1 e 2, se non è divisibile è primo, altrimenti no
(define isPrime? ;val: boolean
  (lambda (num)  ;in: int
    (if (not(haDivisoriIn? num 2 (- num 1)))
        true
        false
    )))

(define haDivisoriIn?
  (lambda (n a b)
    (cond ((= (remainder n b) 0)
        true
        )
    ((<= b a)
     false
     )
    (else
        (haDivisoriIn? n a (- b 1))
     ))
   )
 )

(prime-factors 39)
(prime-factors 9)
(prime-factors 540)

(short-prime-factors 7)
(short-prime-factors 39)
(short-prime-factors 9)

(prime-facts-degs 7)
(prime-facts-degs 39)
(prime-facts-degs 9)
    

