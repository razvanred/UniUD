;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.9.3) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
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

(lista-primi 30)