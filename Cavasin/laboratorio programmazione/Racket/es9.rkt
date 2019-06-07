;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define caesar-cipher
  (lambda (s alphabet)
    (if (> (string-length s) 0)
        (if (char=? (string-ref s 0) #\ )
            (caesar-cipher (substring s 1) alphabet)
            (string-append
             (string (string-ref alphabet (modulo (+ (find (string-ref s 0) alphabet) 3) (string-length alphabet))))
             (caesar-cipher (substring s 1) alphabet)
             )
            )
        ""
        )
    )
  )

(define find
  (lambda (c s)
    (if (char=? (string-ref s 0) c)
        0
        (+ 1 (find c (substring s 1)))
        )
    )
  )

"PARTE I"
(caesar-cipher "ALEA IACTA EST IVLIVS CAESAR DIXIT" "ABCDEFGHILMNOPQRSTVX")

"PARTE 2"
(define s2
  (lambda (u v)
    (+ v 1)
    )
  )

(define H
  (lambda (f g)
    (lambda (m n)
      (if (= n 0)
          (f m)
          (g m ((H f g) m (- n 1)))
          )
      )
    )
  )

(define add (H (lambda (x) x) s2))
(define mul (H (lambda (x) 0) add))
(define pow (H (lambda (x) 1) mul))
(define tet (H (lambda (x) 1) pow))

(tet 3 2)