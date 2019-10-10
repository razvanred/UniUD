;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.6.1) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define dec
  (lambda (num)
    (let ((l (string-length num))
          )
      (let ((lsb (string-ref num (- l 1)))
            (pr (substring num 0 (- l 1)))
            )
        (if (> l 1)
            (+
             (*
              (dec pr)
              2)
             (bit-val lsb)
             )
            (bit-val lsb)
            )
        )
      )
    ))

(define bit-val
  (lambda (bit)
    (if (char=? bit #\0)
        0
        1)
       
    ))