;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Problema4) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define btr-sum
  (lambda (s1 s2)
        
        (if (<= (string-length s1) (string-length s2))
                              (btr-carry-sum 
                               (normalized-btr-length (normalized-btr s1) (normalized-btr s2))
                                                      (normalized-btr s2) #\.)
                              (btr-carry-sum 
                                                       (normalized-btr s1)
                               (normalized-btr-length (normalized-btr s1) (normalized-btr s2)) #\.))
        ))

(define btr-carry-sum ;out: 
  (lambda (s1 s2 r) ;s1: prima stringa da sommare, s2: seconda stringa da sommare, r: riporto (carattere)
    (let ((resto (btr-carry (lsd s1) (lsd s2) r) ))
    (if (or (= (string-length s1) 0) (= (string-length s2) 0))
        (string r)
        (string-append (btr-carry-sum (head s1) (head s2) resto) (string (btr-digit-sum (lsd s1) (lsd s2) r)))
        ))))
        


(define normalized-btr ;out: stringa
  (lambda (s) ;in: stringa
    (if (char=? (string-ref s 0) #\.)
        (normalized-btr (substring s 1))
        s)))

(define normalized-btr-length
  (lambda (s1 s2)
    (normalized-btr-length-rec (if (<= (string-length s1) (string-length s2))
                                  s1
                                  s2
                               )
                              (if (>= (string-length s1) (string-length s2))
                                  (- (string-length s1) (string-length s2))
                                  (- (string-length s2) (string-length s1))
                                  ))))
                              
(define normalized-btr-length-rec
  (lambda (s length)
    (if (= length 0)
        s
        (string-append "." (normalized-btr-length-rec s (- length 1)))
        )))
        
           
        


(define lsd    ;out: carattere
  (lambda (s)  ;s: stringa
    (if (= (string-length s) 0)
        #\.
        (string-ref s (- (string-length s) 1))
    )))
  
(define head  ;out: stringa
  (lambda (s) ;s: stringa
    (if (= (string-length s) 0)
        ""
        (substring s 0 (- (string-length s) 1))
        )))


;; Somma di tre cifre nel sistema ternario bilanciato (caratteri):
;; - u, v rappresentano le cifre "incolonnate",
;; - c rappresenta il riporto "in entrata";
;; - la cifra restituita rappresenta la cifra "incolonnata"
;;   con u, v nel risultato.
;;
;; Il riporto va considerato a parte,
;; definendo una procedura "carry" con analoga struttura per casi.


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


(define btr-carry                    ; val:     carattere +/./-
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
                 ((char=? v #\+)         
                  (cond ((char=? c #\-)  ; - + -
                         #\.)
                        ((char=? c #\.)  ; - + .
                         #\.)
                        ((char=? c #\+)  ; - + +
                         #\.))
                  )))
          ((char=? u #\.)
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; . - -
                         #\-)
                        ((char=? c #\.)  ; . - .
                         #\.)
                        ((char=? c #\+)  ; . - +
                         #\.)))
                 ((char=? v #\.)         
                  (cond ((char=? c #\-)  ; . . -
                         #\.)
                        ((char=? c #\.)  ; . . .
                         #\.)
                        ((char=? c #\+)  ; . . +
                         #\.)))
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; . + -
                         #\.)
                        ((char=? c #\.)  ; . + .
                         #\.)
                        ((char=? c #\+)  ; . + +
                         #\+)))))
          ((char=? u #\+)
           (cond ((char=? v #\-)         
                  (cond ((char=? c #\-)  ; + - -
                         #\.)
                        ((char=? c #\.)  ; + - .
                         #\.)
                        ((char=? c #\+)  ; + - +
                         #\.)))
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
(btr-sum "-+--" "+")
(btr-sum "-+--" "-")
(btr-sum "+-.+" "-+.-")
(btr-sum "-+--+" "-.--")
(btr-sum "-+-+." "-.-+")
(btr-sum "+-+-." "+.+-")