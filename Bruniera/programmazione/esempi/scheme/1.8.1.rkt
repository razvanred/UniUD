;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.8.1) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define fib
  (lambda (num)
    (if (> num 1)
        (+
         (fib (- num 1))
         (fib (- num 2)))
        1)
    ))

(define fib-alt
  (lambda (num)
    (let ((s (sqrt 5))
          (n (+ num 1)))
      (round 
       (/ (- (expt (/ (+ 1 s) 2) n)
             (expt (/ (- 1 s) 2) n))
          s))
      )
    ))

(fib 12)
(fib-alt 12)