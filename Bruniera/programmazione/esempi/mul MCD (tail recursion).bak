;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |mul MCD (tail recursion)|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define f          ; val: intero
  (λ (x y)         ; x,y: interi
    (cond ((= y 0) 0)
          ((even? y) (f (* 2 x) (quotient y 2)))
          (else (+ x (f (* 2 x) (quotient y 2)))))
    ))

(define g          ; val: intero
  (λ (x y)         ; x,y: interi
    (cond ((= y x) x)
          ((< x y) (g x (- y x)))
          (else (g (- x y) y)))
    ))