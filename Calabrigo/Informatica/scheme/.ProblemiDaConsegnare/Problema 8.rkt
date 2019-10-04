;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Problema 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define hanoi-moves ; val: lista di coppie
  (lambda (n) ; n > 0 intero
    (hanoi-rec n 1 2 3)
 ))

(define hanoi-rec ; val: lista di coppie
  (lambda (n s d t) ; n intero, s, d, t: posizioni
    (if (= n 1)
        (list (list s d))
        (let ((m1 (hanoi-rec (- n 1) s t d))
              (m2 (hanoi-rec (- n 1) t d s))
              )
          (append m1 (cons (list s d) m2))
          ))
    ))

(define hanoi-disks
  (lambda(n k)
    (hanoi-rec-disks n k 1 2 3 0 0 0)
    ))

(define hanoi-rec-disks
  (lambda (n k s d t n1 n2 n3)
    (cond ((= n 0)
        (list (list s n1) (list d n2) (list t n3)))
          ((< k (expt 2 (- n 1)))
           (hanoi-rec-disks (- n 1) k s t d (+ n1 1) n3 n2)
           )
          (else
           (hanoi-rec-disks (- n 1) (- k  (expt 2 (- n 1))) t d s n3 (+ n2 1) n1)
           )
          )
    ))

(hanoi-disks 3 1)
(hanoi-disks 3 2)

        
        
