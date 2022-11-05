;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 21-01-13) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
"es1"
(define xtessellations
  (lambda (n k)
    (cond
      ((< n 0)
       0)
      ((= n 0)
       1)
      ((> k 0)
       (+ (xtessellations (- n 1) k) (xtessellations (- n 2) (- k 1))))
      (else
       (xtessellations (- n 1) k)))))

(xtessellations 5 0)
(xtessellations 5 1)
(xtessellations 5 2)
(xtessellations 5 3)
(xtessellations 7 1)
(xtessellations 7 2)

"es2"
(define cA (char->integer #\A))
(define vigenere-cipher
  (lambda (key)
    (lambda (msg)
      (if (string=? msg "")
          ""
          (let ((r (- (char->integer (string-ref key 0)) cA))
                (c (- (char->integer (string-ref msg 0)) cA))
                (k (string-append (substring key 1) (substring key 0 1)))
                )
            (string-append
             (string (integer->char (+ cA (modulo (+ r c) 26))))
             ((vigenere-cipher k) (substring msg 1))
             ))
          ))
    ))

(define encrypt (vigenere-cipher "AKEY"))
(encrypt "ASHORTSAMPLEMESSAGE")

"es3"
(define ufo
  (lambda (c)
    (cond
      ((char=? c #\+) 1)
      ((char=? c #\-) -1)
      (else 0))))
(define fract-part
  (lambda (s)
    (if (= (string-length s) 0)
        0
        (* (+ (ufo (string-ref s 0))
              (fract-part (substring s 1)))
           1/3))))

(fract-part "--")
(fract-part ".++")
(fract-part "-.+")
(fract-part "+.-")
(fract-part ".")
(fract-part "+")