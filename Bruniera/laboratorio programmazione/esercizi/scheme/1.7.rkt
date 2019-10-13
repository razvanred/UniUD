;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1.7|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define lista-primi    ; val: lista
  (lambda (n)          ; n: intero >= 0
    (lista-primi+ 2 n)
    ))


(define lista-primi+   ; val: lista
  (lambda (a b)        ; a: intero, b: intero >= a
    (if (= a b)
        (if (prime? a)
            (list a)
            null)
        (if (prime? a)
            (cons a (lista-primi+ (+ a 1) b))
            (lista-primi+ (+ a 1) b)))
    ))

(define prime?
  (lambda (n)
    (if (even? n)
        (= n 2)
        (not (ha-divisori-dispari-in? n 3 (floor (sqrt n)))))
    ))

(define ha-divisori-dispari-in?
  (lambda (n a b)
    (cond ((> a b) false)
          ((= (remainder n a) 0) true)
          (else
           (ha-divisori-in? n (+ a 2) b)))
    ))

(define ha-divisori-in?
  (lambda (n a b)
    (cond ((> a b) false)
          ((= (remainder n a) 0) true)
          (else
           (ha-divisori-in? n (+ a 1) b)))
    ))

;;;;;;;;;;;;;;;;;;;
(define prime-factors ; val: lista
  (lambda (n) ; n: intero
    (list-factor
     n
     (lista-primi n))
    ))


(define list-factor ; val: lista
  (lambda (n l) ; n: intero, l: lista
    (if (null? l)
        (list n)
        (if (= n 1)
            null
            (if (= (remainder n (car l)) 0)
                (cons (car l) (list-factor (/ n (car l)) l))
                (list-factor n (cdr l)))))
    ))

(prime-factors 7)
(prime-factors 9) 
(prime-factors 28)
(prime-factors 39)
(prime-factors 540)
(prime-factors 1617)

;;;;;;;;;;;;;
(define short-prime-factors ; val: lista
  (lambda (n)   ; n: intero
    (short-list-factor
     n
     (lista-primi n))
    ))


(define short-list-factor ; val: lista
  (lambda (n l) ; n: intero, l: lista
    (if (null? l)
        null
        (if (= (remainder n (car l)) 0)
            (cons (car l) (short-list-factor n (cdr l)))
            (short-list-factor n (cdr l))))
    ))

(short-prime-factors 7)
(short-prime-factors 9) 
(short-prime-factors 28)
(short-prime-factors 39)
(short-prime-factors 540)
(short-prime-factors 1617)

;;;;;;;;;;;;;
(define prime-facs-degs ; val: lista
  (lambda (n) ; n: intero
    (list-facts-deg
     n
     (lista-primi n))
    ))


(define list-facts-deg ; val: lista
  (lambda (n l)  ; n: intero, l: lista
    (if (null? l)
        null
        
            (if (= (remainder n (car l)) 0)
                (cons
                 (list (car l) (fact-deg n (car l)))
                 (list-facts-deg n (cdr l)))
                (list-facts-deg n (cdr l))))
    ))

(define fact-deg ; val: intero
  (lambda (n f)  ; n: intero, f: intero
    (if (= (remainder n f) 0)
        (+ 1
           (fact-deg (/ n f) f))
        0)
    ))

(prime-facs-degs 7)
(prime-facs-degs 9) 
(prime-facs-degs 28)
(prime-facs-degs 39)
(prime-facs-degs 540)
(prime-facs-degs 1617)