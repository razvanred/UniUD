;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Piastrelle2014) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
;piastrella 1x1, piastrella 2x1. No 2x1 adiacenti
(define fill
  (lambda (n)
    (cond ((< n 2)
           1)
          ((= n 2)
           2)
          (else
           (+ (fill (- n 1)) (fill (- n 3))))
          )
    ))
(fill 1); → 1 
(fill 4); → 4
(fill 2); → 2 
(fill 5); → 6
(fill 3); → 3 
(fill 7); → 13