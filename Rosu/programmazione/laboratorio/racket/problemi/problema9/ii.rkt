;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Razvan Rosu
;; Ridefinizione di alcune funzioni in maniera induttiva

;; (define succ
;;   (lambda (n)
;;     (add1 n)
;;     ))

;; (define add
;;   (lambda (m n)
;;     (if (= n 0)
;;         m
;;         (succ (add m (sub1 n))))
;;     ))

;; (define mul
;;   (lambda (m n)
;;     (if (= n 0)
;;         0
;;        (add m (mul m (sub1 n))))
;;     ))

(define H ; lambda
  (lambda (f g) ; int, int
    (lambda (m n) ; int, int
      (if (= n 0)
          (f m)
          (g m ((H f g) m (sub1 n))))
      )
    ))

(define s2 ; int
  (lambda (u v) ; int, int
    (add1 v)
    ))

(define add (H (lambda (x) x) s2))
(define mul (H (lambda (x) 0) add))
(define pow (H (lambda (x) 1) mul))

;; testing
(= (add 0 8) 8)
(= (add 45 9) 54)
(= (add 23 1) 24)

(= (mul 25 8) 200)
(= (mul 1 0) 0)
(= (mul 0 67) 0)
(= (mul 1 89) 89)

(= (pow 45 2) 2025)
(= (pow 34 0) 1)