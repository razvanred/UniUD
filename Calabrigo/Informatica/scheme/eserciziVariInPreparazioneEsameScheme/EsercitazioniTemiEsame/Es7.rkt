;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Es7) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define shared; lista dei numeri comuni tra u e v, inoltre sono ordinati
  (lambda (u v); u,v -> liste ordinate di numeri interi positivi
    (cond ((or (= (length u) 0) (= (length v) 0))
           (list ))
          ((= (car u) (car v))
           (cons (car u) (shared (cdr u) (cdr v))))
          (else
           (longer (shared (cdr u) v) (shared u (cdr v)))
          ))
    ))

(define longer
  (lambda (u v)
    (if (> (length u) (length v)) u v)
    ))
(shared (list 1 3 5 6 7 8 9 10) (list 0 1 2 3 4 5 7 9)); → (1 3 5 7 9)
