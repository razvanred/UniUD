;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname singolareToPlurale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define sm-plurale
  (lambda(nome)
    (string-append
     (substring
      nome 0 (- (string-length nome) 1))
     "i")
    )
  )

 (define sf-plurale
   (lambda(nome)
      (string-append
       (substring
        nome 0
        (-(string-length nome) 1)) "e")))

(define maleOrFemale (lambda(nome) (substring nome (- (string-length nome) 1) (- (string-length nome) 0))))

(define s-plurale(lambda(nome) (if (string=? (maleOrFemale nome) "o") (sm-plurale nome) (sf-plurale nome))))