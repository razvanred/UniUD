;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.9.1) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define list-ref1         ; val: tipo di elemento
  (lambda (s i)           ; s: lista, i: intero
    (if (= i 0)
        (car s)
        (list-ref (cdr s) (- i 1)))
    ))

(define length1           ; val: intero
  (lambda (s)             ; s: lista
    (if (null? s)
        0
        (+ 1 (length (cdr s))))
    ))

(define append1           ; val: lista
  (lambda (s t)           ; s: lista, t: lista
    (if (null? s)
        t
        (cons (car s)
              (append (cdr s) t)))
    ))

(define reverse1          ; val: lista
  (lambda (s)             ; s: lista
    (if (null? s)
        null
        (append
         (reverse (cdr s))
         (list (car s))))
    ))


;soluzione del reverse più efficente
(define reverse2          ; val: lista
  (lambda (s)             ; s: lista
    (reverse-rec s null)
    ))

(define reverse-rec       ; val: lista
  (lambda (s r)           ; s: lista
    (if (null? s)
        r
        (reverse-rec (cdr s) (cons (car s) r)))
    ))
