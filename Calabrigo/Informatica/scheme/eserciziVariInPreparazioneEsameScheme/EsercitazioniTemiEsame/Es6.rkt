;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Es6) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define FractionaryBinaryConverter
  (lambda (bin)
    (if (= (string-length bin) 0)
        0
        (+ (* (FractionaryBinaryConverter (substring bin 1)) 1/2) (binVal (string-ref bin 0)))
        )
    ))

(define binVal
  (lambda (bin) ;char
    (if (char=? bin #\1)
        1/2
        0)
    ))

(FractionaryBinaryConverter "11")