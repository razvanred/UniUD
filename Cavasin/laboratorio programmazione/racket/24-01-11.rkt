;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 24-01-11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
"1"
(define sorted-ins
  (lambda (w s)
    (if (or (null? s) (string<=? w (car s)))
        (append (list w) s)
        (append (list (car s)) (sorted-ins w (cdr s))))))

(sorted-ins "giacinto" '("begonia" "gardenia" "giglio" "viola"))
(sorted-ins "viola" '("begonia" "gardenia" "giacinto" "violaciocca"))

"3"
(define permutation-rule
  (lambda (plain encrypted)
    (lambda (c) (permutation-rec c 0 plain encrypted))
    ))
(define permutation-rec
  (lambda (x i plain encrypted)
    (if (char=? x (string-ref plain i))
        (string-ref encrypted i)
        (permutation-rec x (+ i 1) plain encrypted)
        )))
  
(define pr (permutation-rule "ABCDEFGHILMNOPQRSTVX" "DEFGHILMNOPQRSTVXABC"))
(pr #\C)
(pr #\A)
(pr #\E)
(pr #\S)
(pr #\V)
(pr #\R)

"5"
(define sorter
   (lambda (l min)
     (if (null? l)
         null
         (let ((v (smallest l (car l) min)))
         (append (list v) (sorter (cdr l) v))))))
  
(define append-in-order
  (lambda (l v)
    (if (null? l)
        (list v)
        (if (string<=? (car l) v)
            (append (car l) (append-in-order (cdr l)))
            (append (list v) (cdr l))))))

(sorter '("101" "1011" "10" "111" "10000" "1110" "1" "100" "0" "110" "11"))

