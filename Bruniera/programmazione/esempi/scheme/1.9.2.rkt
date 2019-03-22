;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.9.2) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
;(define intervallo
;  (lambda (n)
;    (if (= n 0)
;        (list 0)
;        (append (intervallo (- n 1)) (list n)))
;    ))
(define intervallo     ; val: lista
  (lambda (n)          ; n: intero >= 0
    (intervallo+ 0 n)
    ))


(define intervallo+    ; val: lista
  (lambda (a b)        ; a: intero, b: intero >= a
    (if (= a b)
        (list a)
        (cons a (intervallo+ (+ a 1) b)))
    ))