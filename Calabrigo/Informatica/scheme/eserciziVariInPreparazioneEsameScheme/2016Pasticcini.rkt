;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2016Pasticcini) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define st-list ; val: lista di stringhe di cifre
  (lambda (n k) ; n, k: interi tali che 1 ≤ k ≤ n e k ≤ 9
    (cond ((and (= n 1) (= k 1))
           (list "1"));-
          ((= k 1)
           (list (string-append (car (st-list (- n 1) 1)) "1" )));-
          ((= k n)
           (list (string-append (car (st-list (- n 1) (- k 1))) (number->string k))))
          (else
           (append
            (map (lambda (x) (string-append x (number->string k)))
                 (st-list (- n 1) (- k 1)))
            (comb-append (st-list (- n 1) k) k)));-
          )))
(define comb-append
  (lambda (u i)
    (if (= i 0)
        null
        (append
         (comb-append u (- i 1))
         (map (lambda (x) (string-append x (number->string i))) u))
        )))

(st-list 4 3)

(comb-append (list "12" "23") 4)