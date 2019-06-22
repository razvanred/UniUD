;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname participiopassato) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;PRIMO METODO PER TRASFORMARE DA INFINITO A PARTICIPIO PASSATO
(define partPp1-3                   ;procedura, i verbi da infinito a participio passato di 1^ o 3^ forma
  (lambda(verbo)
    (string-append
     (substring
      verbo
      0
      (-(string-length verbo)2)
      )
     "to"
     )
   )
 )

(define partPP                   ;procedura, prevede 1 if che controlla le ultime 3 lettere dei verbi, se -ere va nell'if, altrimenti va nell'else e chiama un'altra procedura
  (lambda(verbo)
    (if                      ;condizione
     (string=?
      (substring
     verbo
     (- (string-length verbo) 3)
     (string-length verbo)
     )
      "ere"
      )
     (string-append
      (substring
       verbo
       0
       (- (string-length verbo) 3)
       )
      "uto"
      )                   ;if
     (partPp1-3 verbo)                   ;else
     )
  )
 )

;SECONDO METODO PER TRASFORMARE DA INFINITO A PARTICIPIO PASSATO

(define partPassato1             ;procedura, prevede if-else in cascata: if(-----|are)
  (lambda(verbo)
    (if
     (string=?                   ;condizione, controllo se la parola finisce in are
      (substring
       verbo
       (-(string-length verbo)3)
       (string-length verbo)
       )
      "are"
      )
     (string-append             ;modifico gli ultimi 3 caratteri della stringa: ato
      (substring
       verbo
       0
       (-(string-length verbo)3)
       )
      "ato"
      )
     (if 
      (string=?              ;condizione, controllo se la parola finisce in ere
      (substring
       verbo
       (-(string-length verbo)3)
       (string-length verbo)
       )
      "ere"
      )
     (string-append             ;modifico gli ultimi 3 caratteri della stringa: uto
      (substring
       verbo
       0
       (-(string-length verbo)3)
       )
      "uto"
      )
     (if
      (string=?                 ;condizione, controllo se la parola finisce in ire
      (substring 
       verbo
       (-(string-length verbo)3)
       (string-length verbo)
       )
      "ire"
      )
     (string-append             ;modifico gli ultimi 3 caratteri della stringa: ito
      (substring
       verbo
       0
       (-(string-length verbo)3)
       )
      "ito"
      )
     (string-append verbo " Non valido")
      )
     )
     )
    )
  )