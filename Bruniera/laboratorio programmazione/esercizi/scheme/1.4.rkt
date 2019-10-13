;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1.4|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
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


(define btr-sum       ;val: somma di due numeri btr
  (lambda (a b)       ;a: primo addendo ;b: secondo addendo
    (normalized-btr (btr-carry-sum a b #\.))
    ))

(define head          ;val: parte precedente all'ultima cifra
  (lambda (btr)       ;btr: numero btr
    (let ((l (string-length btr)))
      (if (> l 0)
          (substring btr 0 (- l 1))
          ""))
    ))

(define lsd           ;val: cifra meno significativa del numero btr
  (lambda (btr)       ;btr: numero btr
    (let ((l (string-length btr)))
      (if (> l 0)
          (string-ref btr (- l 1))
          ""))
    ))

(define normalized-btr;val: numero btr senza . in testa
  (lambda (btr)       ;btr: numero btr
    (let ((l (string-length btr)))
      (if (and (> l 1) (char=? #\. (string-ref btr 0)))
          (normalized-btr (substring btr 1 l))
          btr))
    ))

(define btr-carry-sum;val: numero btr somma dei due numeri ed il carry
  (lambda (a b c)    ;a: primo addendo btr ;b: secondo addendo btr ;c: carattere di carry
    (let ((la (string-length a))
          (lb (string-length b)))
      (cond ((and (= la 1) (= lb 1))
             (string-append
              (string (btr-carry (lsd a) (lsd b) c))
              (string (btr-digit-sum (lsd a) (lsd b) c)))
             )
            ((= la 1)
             (string-append
             ; (head (head b))
             ; (string (btr-digit-sum (lsd (head b)) (btr-carry (lsd a) (lsd b) c) #\.))
             ; (string (btr-digit-sum (lsd a) (lsd b) c)))
              (btr-carry-sum "." (head b) (btr-carry (lsd a) (lsd b) c))
              (string (btr-digit-sum (lsd a) (lsd b) c)))
             )
            ((= lb 1)
              (string-append
             ;  (head (head a))
             ;  (string (btr-digit-sum (lsd (head a)) (btr-carry (lsd a) (lsd b) c) #\.))
             ;  (string (btr-digit-sum (lsd a) (lsd b) c)))
               (btr-carry-sum (head a) "." (btr-carry (lsd a) (lsd b) c))
               (string (btr-digit-sum (lsd a) (lsd b) c)))
             )
            (else
             (string-append
              (btr-carry-sum (head a) (head b) (btr-carry (lsd a) (lsd b) c))
              (string (btr-digit-sum (lsd a) (lsd b) c)))
             )
            ))
      ))

  (btr-sum "-+--" "+")
  (btr-sum "-+--" "-")
  (btr-sum "+-.+" "-+.-")
  (btr-sum "-+--+" "-.--")
  (btr-sum "-+-+." "-.-+")
  (btr-sum "+-+-." "+.+-") 