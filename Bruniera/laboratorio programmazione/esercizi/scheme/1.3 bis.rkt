;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1.3 bis|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define bin-rep->number
  (lambda (num)
    (if (con-char? #\- num)
        (* -1 (absolute "01" num))
        (absolute "01" num)
        )
    ))

(define rep->number
  (lambda (c num)
    (if (con-char? #\- num)
        (* -1 (absolute c num))
        (absolute c num)
        )
    ))

(define absolute
  (lambda (c num)
    (if (con-char? #\. num)
        (+
         (int c (substring num 0 (pos num #\.)))
         (float c (substring num (+ (pos num #\.) 1))))
        (int c num))
    ))

(define int
  (lambda (c num)
    (let ((l (string-length num))
          (b (string-length c)))
      (if (> l 0)
          (let ((v (string-ref num (- l 1)))
                (sb (substring num 0 (- l 1))))
            (cond ((char=? v #\+) 0)
                  ((char=? v #\-) 0)
                  (else (+ (pos c v)
                           (* (int c sb) b))))
            )
          0))
    ))

(define float
  (lambda (c num)
    (let ((l (string-length num))
          (b (string-length c)))
      (if (> l 0)
          (let ((v (string-ref num 0))
                (sb (substring num 1)))
            (/
             (+ (pos c v) (float c sb))
             b))
          0))
    ))

(define con-char?
  (lambda (v num)
    (let ((l (string-length num)))
      (if (> l 0)
          (or
           (char=? (string-ref num (- l 1)) v)
           (con-char? v (substring num 0 (- l 1))))
          #f))
    ))

(define pos
  (lambda (c v)
    (if (> (string-length c) 1)
        (if (char=? v (string-ref c 0))
            0
            (+ 1 (pos (substring c 1) v)))
        0)
    ))
 
(bin-rep->number "+1101")
(bin-rep->number "0")
(bin-rep->number "10110.011") 
(bin-rep->number "-0.1101001")

(rep->number "zu" "-uuzz")
(rep->number "0123" "+21.1")
(rep->number "01234" "-10.02")
(rep->number "0123456789ABCDEF" "0.A")
(rep->number "0123456789ABCDEF" "1CF.0")