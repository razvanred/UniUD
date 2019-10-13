;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2018PotenzeDi2) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define powers-of-two
  (lambda (n)
    (if (= n 0)
        (list )
        (powers-of-two-rec n))
    ))


(define powers-of-two-rec
  (lambda (n)
    (if (<= n 1)
        (list 1)
        (append
         (list (GreaterPowerOfTwo n))
         (powers-of-two-rec (- n (GreaterPowerOfTwo n)))
         ))
    ))
         
         
(define GreaterPowerOfTwo
  (lambda (n)
    (if (= n 1)
        1
        (* 2 (GreaterPowerOfTwo (quotient n 2)))
        )))
         
(powers-of-two 167)