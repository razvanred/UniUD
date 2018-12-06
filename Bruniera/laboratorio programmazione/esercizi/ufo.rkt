;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ufo) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
;
(define mul     ; val: inter
  (lambda (x y) ; x,y >= 0 interi
    (mul-rec x y 0)
    ))

(define mul-rec   ; val: intero
  (lambda (x y z) ; x,y,z >= 0 interi
    (cond ((= y 0) z)
          ((even? y) (mul-rec (* 2 x) (quotient y 2) z))
          (else (mul-rec (* 2 x) (quotient y 2) (+ z x)))
          )))

(define ufo
  (lambda (x)
    (cond  ((= x 1) 1)
           ((even? x) (- (* 2 (ufo (quotient x 2))) 1))
           (else (+ (* 2 (ufo (quotient x 2))) 1)))
    ))

(ufo 1)
(ufo 2)
(ufo 3)
(ufo 4)
(ufo 5)
(ufo 6)
(ufo 7)
(ufo 8)
(ufo 9)
(ufo 10)
(ufo 11)
(ufo 12)
(ufo 13)
(ufo 14)
(ufo 15)
(ufo 16)
(ufo 17)
(ufo 18)
(ufo 19)
(ufo 20)
(ufo 21)
(ufo 22)
(ufo 23)
(ufo 24)
(ufo 25)
(ufo 26)
(ufo 27)
(ufo 28)
(ufo 29)