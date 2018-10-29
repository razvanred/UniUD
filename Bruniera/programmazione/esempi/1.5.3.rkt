;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.5.3) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define bin
  (lambda (num)
    (if (< num 2)
        (if (even? num) "0" "1")
        (string-append
         (bin (quotient num 2))
         (if (even? num) "0" "1"))
        )
    ))

(define bin-rep
  (lambda (num)
    ( let ((lsb (if (even? num) "0" "1"))
       )
    (if (< num 2)
        lsb
        (string-append
         (bin-rep (quotient num 2))
         lsb
         )
        ))
    ))