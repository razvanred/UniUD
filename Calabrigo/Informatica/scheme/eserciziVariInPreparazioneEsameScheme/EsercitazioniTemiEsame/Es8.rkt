;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Es8) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define parity-check-failures
  (lambda (lis); lis -> lista di stringhe binarie
    (parity-check-failures-rec lis 0)
    ))

(define  parity-check-failures-rec
  (lambda (lis index)
    (if (= (length lis) 0)
        (list )
        (cons (if (not (even? (ufo (car lis))))
                    index
                    (list ))
                (parity-check-failures-rec (cdr lis) (+ index 1)))
        )
    ))

(define ufo
  (lambda (str)
    (if (= (string-length str) 0)
        0
        (+ (if (= (binVal (string-ref str 0)) 1) 1 0) (ufo (substring str 1)))
        )))

(define binVal
  (lambda (bin)
    (if (char=? bin #\1)
        1
        0)
    ))

(parity-check-failures '("0110" "1101" "0000" "1011")); → '(1 3)
(parity-check-failures '("0111" "1011" "0100" "1110")); → '(0 1 2 3)
(parity-check-failures '("0110" "1001" "0000" "1010")); → '()