;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;binary
(define bin-rep->number
  (lambda (n)
    (* (sign n) (bin-convert (crop n)))
    )
  )

(define bin-convert
  (lambda (n)
    (let ((comma (find n #\.)))
      (+ (bin-convert-integer (substring n 0 comma)) (bin-convert-decimal (if (< comma (string-length n)) (substring n (+ comma 1)) "")))
      )
    )
  )

(define bin-convert-integer
  (lambda (n)
    (if (= (string-length n) 0)
        0
        (let ((last (- (string-length n) 1)))
          (+ (* 2 (bin-convert-integer (substring n 0 last))) (find "01" (string-ref n last)))
          )
        )
    )
  )

(define bin-convert-decimal
  (lambda (n)
    (if (= (string-length n) 0)
        0
        (+ (* 1/2 (bin-convert-decimal (substring n 1))) (* 1/2 (find "01" (string-ref n 0))))
        )
    )
  )

;any
(define rep->number
  (lambda (alphabet n)
    (* (sign n) (convert alphabet (crop n)))
    )
  )

(define convert
  (lambda (alphabet n)
    (let ((comma (find n #\.)))
      (+ (convert-integer alphabet (substring n 0 comma)) (convert-decimal alphabet (if (< comma (string-length n)) (substring n (+ comma 1)) "")))
      )
    )
  )

(define convert-integer
  (lambda (alphabet n)
    (if (= (string-length n) 0)
        0
        (let ((last (- (string-length n) 1)))
          (+ (* (string-length alphabet) (convert-integer alphabet (substring n 0 last))) (find alphabet (string-ref n last)))
          )
        )
    )
  )

(define convert-decimal
  (lambda (alphabet n)
    (if (= (string-length n) 0)
        0
        (+ (* (/ 1 (string-length alphabet)) (convert-decimal alphabet (substring n 1))) (* (/ 1 (string-length alphabet)) (find alphabet (string-ref n 0))))
        )
    )
  )

;common
(define find
  (lambda (n character)
    (if (and (> (string-length n) 0) (not (char=? (string-ref n 0) character)))
        (+ 1 (find (substring n 1) character))
        0
        )
    )
  )

(define crop
  (lambda (n)
    (if (or (char=? (string-ref n 0) #\+) (char=? (string-ref n 0) #\-))
        (substring n 1)
        n
        )
    )
  )

(define sign
  (lambda (n)
    (if (char=? (string-ref n 0) #\-)
        -1
        1
        )
    )
  )

(bin-rep->number "+1101")
(bin-rep->number "0")
(bin-rep->number "10110.011")
(bin-rep->number "-0.1101001")

(rep->number "zu" "-uuzz")
(rep->number "0123" "+21.1")
(rep->number "01234" "-10.02")
(rep->number "0123456789ABCDEF" "0.A")
(rep->number "0123456789ABCDEF" "1CF.0")