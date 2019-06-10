;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ones-complement) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Razvan Rosu
;; Complemento a uno

(define bit-complement   ; val: stringa
  (lambda (bit)          ; bit: stringa
    (if (string=? bit "0")
        "1"
        "0"
        )))

(define ones-complement  ; val: stringa di 0/1
  (lambda (bin)          ; bin: stringa di 0/1
    (if (string=? bin "")
        ""
        (string-append
         (ones-complement (substring bin 0 (- (string-length bin) 1)))
         (bit-complement (substring bin (- (string-length bin) 1)))
         ))
    ))

;; testing
(string=? (ones-complement "1111") "0000")
(string=? (ones-complement "101") "010")