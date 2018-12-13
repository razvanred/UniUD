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
  (lambda (c)           ; c: carattere
    (let ((i (char->integer c))
          (k (char->integer #\z))
          )
      (if (> (+ i 3) k)
          (integer->char (- i 23))
          (integer->char (+ i 3))))
    ))

(define codr            ; val: carattere
  (lambda (c r)           ; c: carattere
    (let ((i (char->integer c))
          (k (char->integer #\z))
          )
      (if (> (+ i 3) k)
          (integer->char (- i -26 r))
          (integer->char (+ i r))))
    ))