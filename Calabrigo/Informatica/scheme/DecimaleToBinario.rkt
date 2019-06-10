;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname DecimaleToBinario) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;dato valore intero > 0 , restituisce una stringa binaria (del numero)

;k intero = 2.mod.k binaria

(define intToString
(lambda (num)
  (cond ((> num 0)
  (string-append (intToString (quotient num 2)) (if(even? num) "0" "1")))
  ((= num 1)
  "1")
  ((= num 0)
  "0")
  )
  )
  )

;(if (even? n) "0" "1")
;quotient -> da il quoziente della divisione