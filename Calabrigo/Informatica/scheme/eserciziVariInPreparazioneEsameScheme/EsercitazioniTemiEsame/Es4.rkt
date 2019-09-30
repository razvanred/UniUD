;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Es4) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define cyclic-string; stringa formata da un pattern ripetuto finchè length non diecnta 0 (length diminuisce ad ogni carattere ripetuto di pattern).
  (lambda (pattern length); pattern -> stringa da ripetere : length -> lunghezza della stringa
    (if (= length 0)
        ""
        (let ((rotPattern
               (string-append (substring pattern 1) 
                              (string (string-ref pattern 0)) )
               ))
          (string-append
           (string (string-ref pattern 0))
           (cyclic-string rotPattern (- length 1)))
          ))
    ))
(cyclic-string "abcde" 13)