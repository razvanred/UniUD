;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1.3|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define bin-rep->number
  (lambda (bin)
    (if (neg? bin)
        (* -1 (absolute bin))
        (absolute bin)
        )
    ))

(define neg?
  (lambda (bin)
    (char=? (string-ref bin 0) #\-)
    ))

(define absolute
  (lambda (bin)
    (+
     (int bin)
     (float bin))
    ))

 (define int
   (lambda (bin)
     (let ((l (string-length bin)))
       (if (> l 1)
           (if (con.? bin)
               (int (substring bin 0 (- l 1)))
               (cond ((char=? (string-ref bin (- l 1)) #\1)
                      (+ 1  (* (int (substring bin 0 (- l 1))) 2)))
                     ((char=? (string-ref bin (- l 1)) #\0)
                      (* (int (substring bin 0 (- l 1))) 2))
                     (else 0))
               )
           0))
     ))

 (define float
   (lambda (bin)
     (let ((l (string-length bin)))
       (if (> l 1)
           (if (con.? bin)
               (cond ((char=? (string-ref bin (- l 1)) #\1)
                      (+ 1  (/ (float (substring bin 0 (- l 1))) 2)))
                     ((char=? (string-ref bin (- l 1)) #\0)
                      (/ (float (substring bin 0 (- l 1))) 2))
                     (else 0))
                (/ (float (substring bin 0 (- l 1))) 2)
               )
           0))
     ))

 (define con.?
   (lambda (bin)
     (let ((l (string-length bin)))
     (if (> l 0)
         (or
          (char=? (string-ref bin (- l 1)) #\.)
          (con.? (substring bin 0 (- l 1))))
         #f))
     ))

(bin-rep->number "+1101")
(bin-rep->number "0")
(bin-rep->number "10110.011") 
(bin-rep->number "-0.1101001")