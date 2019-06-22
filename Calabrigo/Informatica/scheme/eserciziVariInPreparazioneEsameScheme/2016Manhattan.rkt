;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2016Manhattan) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define s-list
  (lambda (u v w)
    (let ((x (if (= u 0) (list ) (s-list (- u 1) v w)))
          (y (if (= v 0) (list ) (s-list u (- v 1) w)))
          (z (if (= w 0) (list ) (s-list u v (- w 1)))))
      (if (= (+ (length x) (length y) (length z)) 0)
          (list "")
          (append
           (map (lambda (x) (string-append "a" x)) x)
           (map (lambda (x) (string-append "b" x)) y)
           (map (lambda (x) (string-append "c" x)) z))
          ))
    ))
(s-list 0 5 0); → ("bbbbb")
(s-list 3 0 1); → ("aaac" "aaca" "acaa" "caaa")
(s-list 1 1 1); → ("abc" "acb" "bac" "bca" "cab" "cba")