;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.5.2) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define s                   ; val: misura
  (lambda (k)               ; k: intero >=0
    (cond ((>= k 2)
           (/ (s (- k 2)) 2))
          ((= k 0)
           s1)
          (else
           s0)
          )
    ))

(define s0 (* (expt 2  1/4) 100))
(define s1 (* (expt 2 -1/4) 100))