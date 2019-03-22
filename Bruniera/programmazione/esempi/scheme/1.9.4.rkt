;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.9.4) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define all-lcs        ; val : stinga
  (lambda (x y)     ; x, y : stringhe
    (cond ((or (string=? x "")
               (string=? y ""))
           (list ""))
          ((string=? (substring x 0 1)
                     (substring y 0 1))
           (all-append (substring x 0 1)
                       (all-lcs (substring x 1)
                                (substring y 1))))
          (else
           (all-longer
            (merge (all-lcs (substring x 1) y)
                   (all-lcs x (substring y 1))))))
    ))

(define merge
  (lambda (x y)
    (if (null? x)
        y
        (if (or (contain? (car x) y) (contain? (car x) (cdr x)))
            (merge (cdr x) y)
            (cons (car x)
                  (merge (cdr x) y))))
    ))

(define all-longer
  (lambda (x)
    (all-longer-at-least 0 x)
    ))

(define all-longer-at-least
  (lambda (n x)
    (if (null? x)
        null
        (if (or (contain>? (car x) (cdr x)) (< (string-length (car x)) n))
            (all-longer-at-least n (cdr x))
            (cons (car x) (all-longer-at-least (string-length (car x)) (cdr x)))))
    ))

(define contain?
  (lambda (s l)
    (if (null? l)
        #f
        (if (string=? s (car l))
            #t
            (contain? s (cdr l))))
    ))

(define contain>?
  (lambda (s l)
    (if (null? l)
        #f
        (if (< (string-length s) (string-length (car l)))
            #t
            (contain>? s (cdr l))))
    ))

(define all-append
  (lambda (s l)
    (if (null? l)
        null
        (cons (string-append s (car l))
              (all-append s (cdr l))))
    ))

(all-lcs "atrio" "arto")