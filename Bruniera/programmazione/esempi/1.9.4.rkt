;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.9.4) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define all-cs      ; val : stinga
  (lambda (x y)     ; x, y : stringhe
    (cond ((or (string=? x "")
               (string=? y ""))
           (list ""))
          ((string=? (substring x 0 1)
                     (substring y 0 1))
           (all-append (substring x 0 1)
                          (all-cs (substring x 1)
                               (substring y 1))))
          (else
           (longer (all-cs (substring x 1) y)
                   (all-cs x (substring y 1))
                   )))
    ))

(define all-append
  (lambda (u s)
    (if (null? s)
        null
        
        (all-append u (cdr s))
    ))

(define all-longer
  (lambda (x y)
    (if (< (string-length (car x)) (string-length (car y)))
        y
        (if (> (string-length (car x)) (string-length (car y)))
        x
        (append x y)))
    ))