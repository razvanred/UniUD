;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.3.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define sm-plurale              ; val: stringa plurale
  (lambda (p)                   ; p: stringa nome maschile da pluralizzare
    (string-append
     (substring
      p
      0                         ; prima lettera
      (- (string-length p) 1)   ; lunghezza della radice
      )
     "i")                       ; desinenza plurale
    )
  )

(define sf-plurale              ; val: stringa nome plurale
  (lambda (p)                   ; p: stringa nome femminile da pluralizzare
    (string-append
     (substring
      p
      0                         ; prima lettera
      (- (string-length p) 1)   ; lunghezza della radice
      )
     "e")                       ; desinenza plurale
    )
  )

(define last-char-comparator    ; boolean: l'ultimo carattere della lettera è uguale a quello passato
  (lambda (word char)           ; word: stringa di qui prendere l'ultimo carattere ; char: carattere da controllare
    (char=?                     ; sono uguali...
     (string-ref
      word
      (- (string-length word) 1); l'ultimo carattere,
      )
     char)                      ; e la lettera?
    )
  )

(define s-plurale               ; val: stringa plurale
  (lambda (nome)                ; nome: stringa da pluralizzare
    (if (last-char-comparator   ; se la parola finisce con 'a'
         nome 
         #\a
         )
        (sf-plurale nome)       ; trattalo come femminile
        (sm-plurale nome)       ; altrimenti come maschile
        )
    )
  )