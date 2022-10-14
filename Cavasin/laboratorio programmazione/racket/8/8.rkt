;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |8|) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss" "installed-teachpacks")) #f)))
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

(define hanoi-picture
  (lambda (n k)
    (hanoi-picture-rec n k 1 2 3 0 0 0 (towers-background n))
    ))

(define hanoi-picture-rec
  (lambda (n k s d t a b c pic)
    (if (= n 0)
        pic
        (let ((nn (- n 1)) ; nn = n-1, per comodità
              (all (+ a (+ b (+ c n)))))
          (if (< k (expt 2 nn))
              (hanoi-picture-rec nn k s t d (+ a 1) c b
                                 (above (disk-image n all s a) pic))
              (hanoi-picture-rec nn (- k (expt 2 nn)) t d s c (+ b 1) a
                                 (above (disk-image n all d b) pic)))))
    ))

(hanoi-disks 3 0); → '((1 3) (3 0) (2 0)) 
(hanoi-disks 3 1); → '((3 0) (2 1) (1 2))
(hanoi-disks 3 2); → '((2 1) (1 1) (3 1)) 
(hanoi-disks 3 3); → '((1 1) (3 2) (2 0))
(hanoi-disks 3 4); → '((3 2) (2 1) (1 0))
(hanoi-disks 3 5); → '((2 1) (1 1) (3 1))
(hanoi-disks 3 6); → '((1 1) (3 0) (2 2))
(hanoi-disks 3 7); → '((3 0) (2 3) (1 0))
(hanoi-disks 5 13); → '((3 2) (2 1) (1 2))
(hanoi-disks 15 19705); → '((3 4) (2 9) (1 2))
(hanoi-disks 15 32767); → '((3 0) (2 15) (1 0))

(hanoi-picture 5 0)
(hanoi-picture 5 13)
(hanoi-picture 5 22)
(hanoi-picture 5 31)
(hanoi-picture 15 19705)