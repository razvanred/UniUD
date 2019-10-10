;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mirolandia2) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define s-list
  (lambda (i j k)
    (let ((x (if (= i 0) null (comb-append (s-list (- i 1) j k) "a")))
          (y (if (= j 0) null (comb-append (s-list i (- j 1) k) "b")))
          (z (if (= k 0) null (comb-append (s-list i j (- k 1)) "c")))
          )
      (if (= (+ i j k) 0)
          (list "")
          (append x y z)
          ))))

(define comb-append
  (lambda (l c)
    (map (lambda (x) (string-append x c))
         l)
    ))

(s-list 0 5 0)
(s-list 3 0 1)
(s-list 1 1 1)