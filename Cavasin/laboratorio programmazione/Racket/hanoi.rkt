;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hanoi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define hanoi-disks ; val: lista di aste + n di dischi
  (lambda (n k) ; n > 0 intero, 0 ≤ k ≤ 2n–1 intero
    (hanoi-rec n k 0 0 0 1 2 3)
    ))


(define hanoi-rec ; val: lista di coppie
  (lambda (n k s d t a b c) ; n intero, 0 ≤ k ≤ (2^n)–1 intero, s, d, t: n di dischi
    (if (> n 0)
        (if (< k (expt 2 (- n 1)))
            (hanoi-rec (- n 1) k (+ s 1) t d a c b);si
            (hanoi-rec (- n 1) (- k (expt 2 (- n 1))) t (+ d 1) s c b a);no
            )
        (list (list a s) (list b d) (list c t))
        )
    )
  )

