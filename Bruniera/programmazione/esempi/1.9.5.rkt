;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.9.5) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define lcs        ; val : stinga
  (lambda (x y)     ; x, y : stringhe
    (cond ((or (null? x)
               (null? y))
           (list ""))
          ((string=? (car x)
                     (car y))
           (cons (car x)
                 (lcs (cdr x)
                      (cdr y))))
          (else
           (longer (lcs (cdr x) y)
                   (lcs x (cdr y)))))
    ))

(define longer
  (lambda (x y)
    (if (> (length x) (length y))
        x
        y)
    ))

(define list=?
  (lambda (a b)
    (if (= (car a) (car b))
        (list=? (cdr a) (cdr b))
        #f)
    ))