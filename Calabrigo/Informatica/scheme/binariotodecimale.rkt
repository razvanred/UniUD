;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname binariotodecimale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;binario-decimale
;r(i)=r(c*2^k)

(define decimale
  (lambda (bin)
    
    (if (> (string-length bin) 0)
        (+
            (* (if (string=? (substring bin 0 1) "1")1 0)
            (expt 2 (-(string-length bin)1)))
        (decimale (substring bin 1 (string-length bin))))
        0
     )
    )
  )
                 