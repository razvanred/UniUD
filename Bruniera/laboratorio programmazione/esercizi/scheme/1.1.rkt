;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define frase
  (lambda (soggetto predicato oggetto)
    (string-append
     (articol soggetto)
     " "
     (coniug soggetto predicato)
     " "
     (articol oggetto)
     )
    )
  )

(define articol
  (lambda (sostantivo)
    (string-append
     (if (plurale sostantivo)
         (if (maschile sostantivo)
             "i"
             "le"
             )
         (if (maschile sostantivo)
             "il"
             "la"
             )
         )
     " "
     sostantivo)
    )
  )

(define plurale
  (lambda (sostantivo)
    (or
     (char=? (string-ref sostantivo (- (string-length sostantivo) 1)) #\i)
     (char=? (string-ref sostantivo (- (string-length sostantivo) 1)) #\e)
     )
    )
  )

(define maschile
  (lambda (sostantivo)
    (or
     (char=? (string-ref sostantivo (- (string-length sostantivo) 1)) #\i)
     (char=? (string-ref sostantivo (- (string-length sostantivo) 1)) #\o)
     )
    )
  )

(define coniug
  (lambda (soggetto verbo)
    (string-append
     (substring verbo 0 (- (string-length verbo) 3))
     (if (plurale soggetto)
         (if (prima verbo)
             "ano"
             "ono"
             )
         (if (prima verbo)
             "a"
             "e"
             )
         )
     )
    )
  )

(define prima
  (lambda (verbo)
    (char=? (string-ref verbo (- (string-length verbo) 3)) #\a)
    )
  )