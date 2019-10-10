;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Piastrelle2013) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
;dati n e k. calcola in quanti modi posso disporre in un cordolo di lunghezza n piastrelle 1x1 e k piastrelle 2x1
(define xTessellation
  (lambda (n k) ;n-> lunghezza cordolo, k-> numero di piastrelle 2x1
    (cond ((or (< n 2) (= k 0))
          1)
          ((and(= n 2) (= k 1))
           2)
          (else
           (+ (xTessellation (- n 2) (- k 1)) (xTessellation (- n 1) k)))
          )
    ))
(xTessellation 7 1)
(xTessellation 7 2)