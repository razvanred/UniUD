;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname principale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Razvan Rosu
; Balanced Ternary Representation

(define zero-str (string #\.))

(define is-string-empty ; boolean
  (lambda (str) ; string
    (= 0 (string-length str))))

; restituisce la rappresentazione non vuota equivalente in cui le eventuali cifre zero in testa, ininfluenti, sono rimosse
(define normalized-btr ; string
  (lambda (btr) ; string
    (let ([next-str-length (sub1 (string-length btr))])
      (if (< next-str-length 0)
          zero-str
          (let ([msd (string-ref btr 0)]
                [next-string (substring btr 1 (string-length btr))])
            (if (char=? msd #\.)
                (normalized-btr next-string)
                btr)
            )
          )
      )
    ))

; restituisce la cifra meno significativa o 0 se l'argomento è la stringa vuota
(define lsd ; char
  (lambda (btr) ; string
    (let ([lsd-position (sub1 (string-length btr))])
      (if (< lsd-position 0)
          #\.
          (string-ref btr lsd-position))
      )
    ))

; restituisce la parte che precede l'ultima cifra oppure la stringa vuota se la stringa è vuota
(define head ; string
  (lambda (btr) ; string
    (let ([lsd-position (sub1 (string-length btr))])
      (if (< lsd-position 0)
          (string)
          (substring btr 0 lsd-position)))
    ))

; date due cifre BTR incolonnate e il relativo importo in entrata restituire la cifra BTR corrisponsedente della rappresentazione della somma
(define btr-digit-sum                    ; val:     carattere +/./-
  (lambda (u v c)                        ; u, v, c: caratteri +/./-
    (cond ((char=? u #\-)                ; u v c
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; - - -
                         #\.)
                        ((char=? c #\.)  ; - - .
                         #\+)
                        ((char=? c #\+)  ; - - +
                         #\-)))
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; - . -
                         #\+)
                        ((char=? c #\.)  ; - . .
                         #\-)
                        ((char=? c #\+)  ; - . +
                         #\.)))
                 ((char=? v #\+)         ; - + c
                  c)))
          ((char=? u #\.)
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; . - -
                         #\+)
                        ((char=? c #\.)  ; . - .
                         #\-)
                        ((char=? c #\+)  ; . - +
                         #\.)))
                 ((char=? v #\.)         ; . . c
                  c)
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; . + -
                         #\.)
                        ((char=? c #\.)  ; . + .
                         #\+)
                        ((char=? c #\+)  ; . + +
                         #\-)))))
          ((char=? u #\+)
           (cond ((char=? v #\-)         ; + - c
                  c)
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; + . -
                         #\.)
                        ((char=? c #\.)  ; + . .
                         #\+)
                        ((char=? c #\+)  ; + . +
                         #\-)))
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; + + -
                         #\+)
                        ((char=? c #\.)  ; + + .
                         #\-)
                        ((char=? c #\+)  ; + + +
                         #\.)))))
          )))

; date due cifre incolonnate e il relativo riporto BTR in entrata restituisce il riporto BTR in uscita conseguente alla somma delle cifre
(define btr-carry
  (lambda (u v c)                        ; u, v, c: caratteri +/./-
    (cond ((char=? u #\-)                ; u v c
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; - - -
                         #\-)
                        ((char=? c #\.)  ; - - .
                         #\-)
                        ((char=? c #\+)  ; - - +
                         #\.)))
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; - . -
                         #\-)
                        ((char=? c #\.)  ; - . .
                         #\.)
                        ((char=? c #\+)  ; - . +
                         #\.)))
                 ((char=? v #\+)         ; - + c
                  #\.)))
          ((char=? u #\.)
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; . - -
                         #\-)
                        ((char=? c #\.)  ; . - .
                         #\.)
                        ((char=? c #\+)  ; . - +
                         #\.)))
                 ((char=? v #\.)         ; . . c
                  #\.)
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; . + -
                         #\.)
                        ((char=? c #\.)  ; . + .
                         #\.)
                        ((char=? c #\+)  ; . + +
                         #\+)))))
          ((char=? u #\+)
           (cond ((char=? v #\-)         ; + - c
                  #\.)
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; + . -
                         #\.)
                        ((char=? c #\.)  ; + . .
                         #\.)
                        ((char=? c #\+)  ; + . +
                         #\+)))
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; + + -
                         #\.)
                        ((char=? c #\.)  ; + + .
                         #\+)
                        ((char=? c #\+)  ; + + +
                         #\+)))))
          )))

; date le rappresentazioni BTR di due interi e il riporto in entrata restituisce la rappresentazione BTR della somma inclusiva del riporto
(define btr-carry-sum ; string
  (lambda (btr-a btr-b c) ; string, string, char
    (let [(head-a (head btr-a))
          (head-b (head btr-b))
          (lsd-a (lsd btr-a))
          (lsd-b (lsd btr-b))]
      (if (and
           (is-string-empty btr-a)
           (is-string-empty btr-b)
           (char=? #\. c))
          ""
          (string-append
           (btr-carry-sum head-a head-b (btr-carry lsd-a lsd-b c))
           (string (btr-digit-sum lsd-a lsd-b c)))
          )
      )))

; procedura principale
(define btr-sum ; string
  (lambda (btr-a btr-b) ; string, string
    (normalized-btr (btr-carry-sum (normalized-btr btr-a) (normalized-btr btr-b) #\.))
    ))

; testing
(string=? (btr-sum "-+--" "+") "-+-.")
(string=? (btr-sum "-+--" "-") "-.++")
(string=? (btr-sum "+-.+" "-+.-") ".")
(string=? (btr-sum "-+--+" "-.--") "--++.")
(string=? (btr-sum "-+-+." "-.-+") "-.-.+")
(string=? (btr-sum "+-+-." "+.+-") "+.+.-")

