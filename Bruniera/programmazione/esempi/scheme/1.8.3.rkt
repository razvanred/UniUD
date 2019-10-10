;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.8.2) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define llcs        ; val : intero
  (lambda (x y)     ; x, y : stringhe
    (cond ((or (string=? x "")
               (string=? y ""))
           0)
          ((string=? (substring x 0 1)
                     (substring y 0 1))
           (+ 1
              (llcs (substring x 1)
                    (substring y 1))))
          (else
           (max (llcs (substring x 1) y)
                (llcs x (substring y 1)))))
    ))

(define lcs        ; val : stinga
  (lambda (x y)     ; x, y : stringhe
    (cond ((or (string=? x "")
               (string=? y ""))
           "")
          ((string=? (substring x 0 1)
                     (substring y 0 1))
           (string-append (substring x 0 1)
                          (lcs (substring x 1)
                               (substring y 1))))
          (else
           (longer (lcs (substring x 1) y)
                   (lcs x (substring y 1)))))
    ))

(define longer
  (lambda (x y)
    (if (> (string-length x) (string-length y))
        x
        y)
    ))

(lcs "acggctagctaggtcaactgcta" "tacgcgatatcgggctaacgtca")
(llcs "acggctagctaggtcaactgcta" "tacgcgatatcgggctaacgtca")