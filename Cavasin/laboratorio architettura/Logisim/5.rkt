;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Somma di tre cifre nel sistema ternario bilanciato (caratteri):
;; - u, v rappresentano le cifre "incolonnate",
;; - c rappresenta il riporto "in entrata";
;; - la cifra restituita rappresenta la cifra "incolonnata"
;;   con u, v nel risultato.
;;
;; Il riporto va considerato a parte,
;; definendo una procedura "carry" con analoga struttura per casi.

(define btr-sum
  (lambda (u v)
  (btr-carry-sum u v #\.)
   )
  )

(define btr-carry-sum
  (lambda (u v c)
    (if (or (= (string-length u) 0) (= (string-length v) 0))
        ""
        (string-append (string (btr-digit-sum (lsd u) (lsd v) c)) (btr-carry-sum (head u) (head v) (btr-carry (lsd u) (lsd v) c)))
        )
    )
  )

(define normalized-btr
  (lambda (a)
    (if (and (>	(string-length a) 0) (char=? #\. (string-ref a 0)))
        (normalized-btr (substring a 1))
        a
        )               
    )               
  )

(define lsd
  (lambda (a)
    (if (> (string-length a) 0)
        (string-ref a (- (string-length a) 1))
        #\.
        )
    )
  )

(define head
  (lambda (a)
    (if (> (string-length a) 0)
        (substring a 0 (- (string-length a) 1))
        ""
        )
    )
  )

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

(define btr-carry                        ; val:     carattere +/./-
  (lambda (u v c)                        ; u, v, c: caratteri +/./-
    (cond ((char=? u #\-)                ; u v c
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; - - -<
                         #\-)
                        ((char=? c #\.)  ; - - .<
                         #\-)
                        ((char=? c #\+)  ; - - +<
                         #\.)))
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; - . -<
                         #\-)
                        ((char=? c #\.)  ; - . .<
                         #\.)
                        ((char=? c #\+)  ; - . +<
                         #\.)))
                 ((char=? v #\+)         ; - + c<
                  c)))
          ((char=? u #\.)
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; . - -<
                         #\-)
                        ((char=? c #\.)  ; . - .<
                         #\.)
                        ((char=? c #\+)  ; . - +<
                         #\.)))
                 ((char=? v #\.)         ; . . c<
                  c)
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; . + -<
                         #\.)
                        ((char=? c #\.)  ; . + .<
                         #\.)
                        ((char=? c #\+)  ; . + +<
                         #\+)))))
          ((char=? u #\+)
           (cond ((char=? v #\-)         ; + - c<
                  c)
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; + . -<
                         #\.)
                        ((char=? c #\.)  ; + . .<
                         #\.)
                        ((char=? c #\+)  ; + . +<
                         #\+)))
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; + + -<
                         #\.)
                        ((char=? c #\.)  ; + + .<
                         #\+)
                        ((char=? c #\+)  ; + + +<
                         #\+)))))
          ))
  )