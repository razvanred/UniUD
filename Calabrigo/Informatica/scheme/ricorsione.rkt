;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ricorsione) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define compl
  (lambda (bit)                 ;bit: stringa di un bit
     (if (string=? bit "0")
         "1"
         "0"
     )
  )
 )

(define complA1               ;val: stringa
  (lambda (seq)               ;seq: stringa di bit
    (if (> (string-length seq) 0)
        (string-append
         (compl (substring seq 0 1))
         (complA1 (substring seq 1)) ; ricorsione, 
         )
        ""  ;(compl (substring seq 0 1))
     )
  )
)



;esercizio-> isolo l'ultimo bit della sequenza e faccio la ricorsione (al contrario rispetto a questo)
