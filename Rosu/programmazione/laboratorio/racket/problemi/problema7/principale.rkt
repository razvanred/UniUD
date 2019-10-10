;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname principale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Razvan Rosu
;; Fattorizzazione in numeri primi

;; Procedure di appoggio

;; verifica se il numero dato in pasto è primo
(define prime? ; Boolean
  (lambda (n) ; int
    (if (even? n)
        (= n 2)
        (no-odd-divisors? n 3 (floor (sqrt n))))
    ))

;; restiuisce il numero incrementato di 2
(define add2 ; int
  (lambda (x) ; int
    (+ x 2)
    ))

;; verifica se ci sono divisori dispari di n in un range definito
(define no-odd-divisors? ; Boolean
  (lambda (n start end) ; int, int, int
    (cond ((> start end) #t)
          ((= 0 (remainder n start)) #f)
          (else (no-odd-divisors? n (add2 start) end)))
    ))

;; restituisce il primo numero primo uguale o successivo a n
(define first-prime ; int
  (lambda (n) ; int
    (if (even? n)
        2
        (first-prime-tr n 3))
    ))

(define first-prime-tr ; int
  (lambda (n last) ; int, int
    (cond ((and
            (prime? last)
            (= 0 (remainder n last))
            )
           last)
          (else (first-prime-tr n (add2 last)))
          )))

;; restituisce n semplificato da den tante volte quanto da renderlo non multiplo di den
(define aggressive-simplifier ; int
  (lambda (n den) ; int, int
    (if (= 0 (remainder n den))
        (aggressive-simplifier (/ n den) den)
        n)
    ))

;; restituisce n semplificato da den tante volte quanto da renderlo non multiplo di den
;; conta anche il grado del denominatore
(define aggressive-simplifier-with-counter ; Pair<Int, List<Int>>
  (lambda (n den) ; int, int
    (aggressive-simplifier-with-counter-tr n den 0)
    ))

(define aggressive-simplifier-with-counter-tr ; Pair<Int, List<Int>>
  (lambda (n den i) ; int, int, int
    (if (= 0 (remainder n den))
        (aggressive-simplifier-with-counter-tr (/ n den) den (add1 i))
        (list n (list den i)))
    ))


;; Procedure principali

;; restuisce la lista ordinata dei fattori primi di n
;; nella lista il fattore è ripetuto tante volte quanto il suo grado
(define prime-factors ; List<Int>
  (lambda (n) ; int (maggiore di 1)
    (if (= n 1)
        '()
        (let ([f (first-prime n)])
          (append (list f) (prime-factors (/ n f)))))
    ))

;; restituisce la lista ordinata e senza ripetizioni dei fattori primi di n
(define short-prime-factors ; List<Int>
  (lambda (n) ; int (maggiore di 1)
    (if (= n 1)
        '()
        (let ([f (first-prime n)])
          (append (list f) (short-prime-factors (aggressive-simplifier n f)))))
    ))

;; restituisce una lista ordinata di coppie, dove ciascuna rappresenta
;; un fattore primo e il rispettivo grado di fattorizzazione
(define prime-facts-degs ; List<List<Int>>
  (lambda (n) ; int (maggiore di 1)
    (if (= n 1)
        '()
        (let ([result (aggressive-simplifier-with-counter n (first-prime n))])
          (append (cdr result) (prime-facts-degs (car result)))))
    ))

;; testing

; parte A
(equal? (prime-factors 7) (list 7))
(equal? (prime-factors 9) (list 3 3))
(equal? (prime-factors 28) (list 2 2 7))
(equal? (prime-factors 39) (list 3 13))
(equal? (prime-factors 540) (list 2 2 3 3 3 5))
(equal? (prime-factors 1617) (list 3 7 7 11))

; parte B
(equal? (short-prime-factors 7) (list 7))
(equal? (short-prime-factors 9) (list 3))
(equal? (short-prime-factors 28) (list 2 7))
(equal? (short-prime-factors 39) (list 3 13))
(equal? (short-prime-factors 540) (list 2 3 5))
(equal? (short-prime-factors 1617) (list 3 7 11))

; parte C
(equal? (prime-facts-degs 7) (list (list 7 1)))
(equal? (prime-facts-degs 9) (list (list 3 2)))
(equal? (prime-facts-degs 39) (list (list 3 1) (list 13 1)))
(equal? (prime-facts-degs 28) (list (list 2 2) (list 7 1)))
(equal? (prime-facts-degs 540) (list (list 2 2) (list 3 3) (list 5 1)))
(equal? (prime-facts-degs 1617) (list (list 3 1) (list 7 2) (list 11 1)))

