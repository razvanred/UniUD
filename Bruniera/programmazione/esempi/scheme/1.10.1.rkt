;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.10.1) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
(define encryption      ; val: stringa
  (lambda (msg rule)    ; msg: stringa, rule: procedura
    (if (string=? msg "")
        ""
        (string-append
         (string
          (rule (string-ref msg 0)))
         (encryption
          (substring msg 1) rule)))
    ))

(define cod-cesare      ; val: procedura
  (lambda (rot)         ; rot: intero
    (lambda (c)           ; c: carattere
      (let ((i (char->integer c))
            (k (char->integer #\z))
            )
        (if (> (+ i 3) k)
            (integer->char (- i -26 rot))
            (integer->char (+ i rot))
            )))
    ))

(define cod3            ; val: carattere
  (cod-cesare 3))

(define codr            ; val: carattere
  (lambda (c r)
    ((cod-cesare r) c)
    ))

(define decrypt
  (lambda (rule)
    (let ((c (rule #\a)))
      (let ((r (-
                (char->integer c)
                (char->integer #\a))))
        (cod-cesare (- 26 r))))
    ))

(define dec3            ; val: carattere
  (decrypt cod3))

(define map1
  (lambda (f s)
    (if (null? s)
        null
        (cons (f (car s))
              (map (cdr s))))
    ))

(define decrypt2
  (lambda (rule)
    (lambda (c)
      (find (char->integer #\a) c rule)
      )
  ))

(define find
  (lambda (i c rule)
    (let ((v (integer->char i)))
      (if (char=? (rule v) c)
          v
          (find (+ i 1) c rule)))
    ))