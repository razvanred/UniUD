;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define frase
  (lambda (soggetto verbo complemento)
    (string-append (articolo soggetto) " " soggetto " " (coniuga soggetto verbo) " " (articolo complemento) " " complemento)
    ))

(define articolo
  (lambda (parola)
    (cond
      ((char=? (string-ref parola (- (string-length parola) 1)) #\o) "il")
      ((char=? (string-ref parola (- (string-length parola) 1)) #\i) "i")
      ((char=? (string-ref parola (- (string-length parola) 1)) #\a) "la")
      (else "le")
      )
    )
  )

(define coniuga
  (lambda (soggetto verbo)
    (if (or (char=? (string-ref verbo (- (string-length verbo) 3)) #\e) (char=? (string-ref verbo (- (string-length verbo) 3)) #\i))
        (if (or (char=? (string-ref soggetto (- (string-length soggetto) 1)) #\o) (char=? (string-ref soggetto (- (string-length soggetto) 1)) #\a))
            (string-append (substring verbo 0 (- (string-length verbo) 3)) "e")
            (string-append (substring verbo 0 (- (string-length verbo) 3)) "ono")
            )
        (if (or (char=? (string-ref soggetto (- (string-length soggetto) 1)) #\o) (char=? (string-ref soggetto (- (string-length soggetto) 1)) #\a))
            (string-append (substring verbo 0 (- (string-length verbo) 3)) "a")
            (string-append (substring verbo 0 (- (string-length verbo) 3)) "ano")
            )
        )
    )
  )


(frase "gatto" "cacciare" "topi")
(frase "mucca" "mangiare" "fieno")
(frase "sorelle" "leggere" "novella")
(frase "bambini" "amare" "favole")
(frase "musicisti" "suonare" "pianoforti")
(frase "cuoco" "friggere" "patate")
(frase "camerieri" "servire" "clienti")
(frase "mamma" "chiamare" "figlie")