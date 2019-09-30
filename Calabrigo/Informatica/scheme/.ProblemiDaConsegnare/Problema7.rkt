;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Problema7) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
;prime-factors   ->   (prime-factors 7) → '(7) , (prime-factors 28) → '(2 2 7) , (prime-factors 540) → '(2 2 3 3 3 5)

(define primeFactors
  (lambda (n)
    (if (isPrime? n)
        (list n)
        (append (list (car (primeFactors-rec n (listaPrimi n))))
                (primeFactors (quotient n (car (primeFactors-rec n (listaPrimi n)))))
                ))))
        
        
        
      
(define primeFactors-rec;out: lista
   (lambda (n lista);in: intero, lista
     (if (null? lista)
        (list )
        (if (= (remainder n (car lista)) 0)
            (append (list (car lista)) (primeFactors-rec n (cdr lista)))
            (primeFactors-rec n (cdr lista)))
        )
     ))
      
      
(define short-prime-factors
  (lambda (n)
    (if (isPrime? n)
        (list n)
        (short-prime-factors-rec n (listaPrimi n))
    )
    ))

(define short-prime-factors-rec;out: lista
  (lambda (n lista) ;in: intero, lista
    (if (null? lista)
        (list )
        (if (= (remainder n (car lista))0)
            (append (list (car lista)) (short-prime-factors-rec n (cdr lista)))
            (short-prime-factors-rec n (cdr lista)))
        )
    ))

(define prime-facts-degs
  (lambda (n)
    (if (isPrime? n)
        (list n 1)
        (prime-facts-degs-rec n (listaPrimi n))
    )))


(define prime-facts-degs-rec
  (lambda (n primi)
    (if (or (null? primi) (= n 1))
        (list )
        (if (= (remainder n (car primi)) 0)
            (cons (list (car primi) (numberOfPOccurrences n (car primi)))
                  (prime-facts-degs-rec (quotient n (car primi)) (cdr primi)))
            (prime-facts-degs-rec n (cdr primi)))
        )
    
    ))

(define numberOfPOccurrences;intero
  (lambda (n p);in: intero, numero primo
    (if (= (remainder n p) 0)
        (+ (numberOfPOccurrences (quotient n p) p) 1)
        0
        )
    ))
   


(define isPrime? ;out: intero
  (lambda (p) ;in: intero
    (if (= p 2)
        #t
        (if (> (DivisoriInRange 1 (- p 1) p) 0)
            #f
            #t
            )
        )
    ))

(define DivisoriInRange
  (lambda (min max num)
    (if (= max min)
        0
        (+ (if (= (remainder num max) 0) 1 0) (DivisoriInRange min (- max 1) num)))
    ))


(define listaPrimi
  (lambda (p)
    (cond ((<= p 1)
          (list ))
          ((= p 2)
           (list 2))
          (else
           (listaPrimi-rec (- p 1) p))
          )
    ))

(define listaPrimi-rec
  (lambda (min num); in: min=num - 1
    (if (= min 1)
        (list )
        (append (listaPrimi-rec (- min 1) num)
                (if (isPrime? min) (list min) (list )))
        )
    ))

(primeFactors 7)
(primeFactors 39)
(primeFactors 9)
(primeFactors 540)
(primeFactors 28)
(primeFactors 1617)

(short-prime-factors 7)
(short-prime-factors 39)
(short-prime-factors 9)
(short-prime-factors 540)
(short-prime-factors 28)
(short-prime-factors 1617)

(prime-facts-degs 7)
(prime-facts-degs 39)
(prime-facts-degs 9)
(prime-facts-degs 28)
(prime-facts-degs 540)
(prime-facts-degs 1617)

        