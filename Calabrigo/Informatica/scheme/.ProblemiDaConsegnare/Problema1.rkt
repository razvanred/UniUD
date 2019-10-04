;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Problema1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; femminile a-singolare e-plurale preceduti da la o le
; maschile o-singolare i-plurale preceduti da il o i
(define CreateSentence
   (lambda(nome verbo complOggetto)
     (string-append
      (CreateConiugazione nome)
      " "
      nome
      " "
      (coniugaVerbo nome verbo)
      " "
      (CreateConiugazione complOggetto)
      " "
      complOggetto
     )
  )
 )


(define CreateConiugazione
  (lambda(sost)
    (if (sostantivoIsMaschile sost)
        (if (sostantivoIsSingolare sost)
            "il"
            "i"
            );maschile
        (if (sostantivoIsSingolare sost);femminile
            "la"
            "le"
        )
  )
 )
)

  (define sostantivoIsSingolare
    (lambda(sost)
         (if (or(string=?
                  (substring sost (-(string-length sost)1) (string-length sost))
                  "a"
              )
                (string=?
                  (substring sost (-(string-length sost)1) (string-length sost))
                  "o"
              )
              )
             #t
             #f
         )
      )
    )

    (define sostantivoIsMaschile
    (lambda(sost)
          (if (or(string=?
                  (substring sost (-(string-length sost)1) (string-length sost))
                  "o"
              )
              (string=?
                  (substring sost (-(string-length sost)1) (string-length sost))
                  "i"
              )
              )
             #t
             #f
         )
    )
    )

(define coniugaVerbo
(lambda(sost verbo)
  (if (string=?
       (substring verbo (-(string-length verbo)3) (string-length verbo))
       "are")
      (if (sostantivoIsSingolare sost);se are
          (string-append
             (substring verbo 0 (-(string-length verbo)3))
             "a")
      (string-append
             (substring verbo 0 (-(string-length verbo)3))
             "ano")
      )
      (coniugaVerbo2-3 sost verbo);se non are
   )
  )
  )

(define coniugaVerbo2-3
  (lambda(sost verbo)
    (if (sostantivoIsSingolare sost)
    (string-append
    (substring verbo 0 (-(string-length verbo)3))
    "e")
    (string-append
    (substring verbo 0 (-(string-length verbo)3))
    "ono")
    )
  )
  )