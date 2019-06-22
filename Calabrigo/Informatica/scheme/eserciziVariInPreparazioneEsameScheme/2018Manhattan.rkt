;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2018Manhattan) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define manhattan-var ; val: intero
  (lambda (i j k) ; i, j, k: interi non negativi tali che k ≤ i e k ≤ j
    (let (
          (x (if (= i k) 0 (manhattan-var (- i 1) j k)))
          (y (if (= j k) 0 (manhattan-var i (- j 1) k)));-
          (z (if (= k 0) 0 (manhattan-var (- i 1) (- j 1) (- k 1))));-
          )
      (if (and (> i 0) (> j 0))
          (+ x y z);-
          1));-
    ))

(manhattan-var 3 2 0)
(manhattan-var 3 2 2)
(manhattan-var 2 2 2)