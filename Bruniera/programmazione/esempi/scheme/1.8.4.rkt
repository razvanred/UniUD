;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.8.4) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define fertili     ; val: intero
  (lambda (i)       ; i: intero
    (if (= i 0)
        1
        (+ (fertili (- i 1))
           (cuccioli (- i 1))))
    ))

(define cuccioli     ; val: intero
  (lambda (i)       ; i: intero
    (if (= i 0)
        0
        (fertili (- i 1)))
    ))

(define fib
  (lambda (i)
    (+ (fertili i) (cuccioli i))
    ))