;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname principale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Razvan Rosu
;; Cifrario di Cesare

(define string-empty? ; Boolean
  (lambda (str) ; String
    (= 0 (string-length str))))

(define latin-alphabet "ABCDEFGHILMNOPQRSTVX")

;; Punto di partenza
(define encrypt ; String
  (lambda (str rule) ; String, lambda(string)
    (if (string-empty? str)
        ""
        (string-append (string (rule (string-ref str 0))) (encrypt (substring str 1) rule)))))

;; ritorna la posizione del carattere nella stringa, altrimenti -1 se non presente
(define find-char-in-string
  (lambda (str character)
    (find-char-in-string-tr str character 0)))

(define find-char-in-string-tr ; int
  (lambda (str character position)
    (cond ((string-empty? str)
           -1)
          ((char=? (string-ref str 0) character) position)
          (else (find-char-in-string-tr (substring str 1) character (add1 position))))))

;; cifrario di Cesare
(define caesar-cipher
  (lambda (shift)
    (lambda (character)
      (string-ref
       latin-alphabet
       (let ([newCharPosition (+ (find-char-in-string latin-alphabet character) shift)])
         (if (< (sub1 (string-length latin-alphabet)) newCharPosition)
             (- newCharPosition (string-length latin-alphabet))
             newCharPosition))))
    ))

; testing 
(string=?
 (encrypt "ALEAIACTAESTIVLIVSCAESARDIXIT" (caesar-cipher 3))
 "DOHDNDFADHXANBONBXFDHXDVGNCNA"
 )
