;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname principale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Razvan Rosu
; Problema 3

; Parte A: conversione dei numeri razionali da base 2 a base 10
; si accettano +, - e il decimal point

; metodo che permette di rimuovere il segno
(define remove-sign ; string
  (lambda (str) ; string
    (let ([f (string-ref str 0)])
      (if (or (char=? #\+ f) (char=? #\- f))
          (substring str 1 (string-length str))
          str))
    ))

; metodo che permette di determinare il valore del segno
(define sign-value ; int
  (lambda (str) ; string
    (let ([f (string-ref str 0)])
      (if (char=? #\- f) -1 1))))

; metodo che rimuove il decimal point da una stringa
(define remove-decimal-point ; string
  (lambda (str) ; string
    (let ([next-str-length (sub1 (string-length str))])
      (if (< next-str-length 0)
          str
          (let ([act (string-ref str 0)]
                [next-string (substring str 1 (string-length str))])
            (if (char=? act #\.)
                next-string
                (string-append (string act) (remove-decimal-point next-string))))
          ))
    ))
                      
; metodo che trova la posizione della virgola all'interno di una stringa usando la tail recursion
(define find-decimal-point ; int
  (lambda (str) ; string
    (find-decimal-point-tr str 0)))

; tail recursion
(define find-decimal-point-tr ; int
  (lambda (str index) ; string, int
    (let ([next-str-length (sub1 (string-length str))])
      (if (< next-str-length 0)
          0
          (if (char=? #\. (string-ref str next-str-length))
              index
              (find-decimal-point-tr (substring str 0 next-str-length) (add1 index)))
          )
      )
    ))
        
; metodo che traduce il carattere binario in numero
(define bin-digit ; int
  (lambda (c) ; char
    (cond
      ((char=? #\0 c) 0)
      ((char=? #\1 c) 1)
      )
    ))

; metodo che prende una stringa binaria e la converte in intero
(define bin-rep->integer ; int
  (lambda (str) ; string
    (let ([next-str-length (sub1 (string-length str))])
      (let ([bin (bin-digit (string-ref str next-str-length))])
        (if (= next-str-length 0)
            bin
            (+ bin (* 2 (bin-rep->integer (substring str 0 next-str-length))))
            )
        )
      )
    ))

; metodo che normalizza la stringa scritta in rappresentazione generica
(define normalize-rep
  (lambda (str)
    (remove-sign (remove-decimal-point str))
    ))

(define bin-rep->number ; number
  (lambda (str) ; string
    (let ([sub-power (find-decimal-point str)])
      (* (sign-value str) (/ (bin-rep->integer (normalize-rep str)) (expt 2 sub-power))))
    ))
          
; testing
"--- Parte A ---"
(= (bin-rep->number "+1101") 13)
(= (bin-rep->number "0") 0)
(= (bin-rep->number "10110.011") 22.375)
(= (bin-rep->number "-0.1101001") -0.8203125)
 
; Parte B: estendere la tecnica usata sopra per convertire i numeri di qualsiasi base b
; per esprimere numeri in base b sono necessarie b cifre di valore 0, ..., b-1
;
; definire rep->number, che accetta due stringhe come argomenti
; la prima stringa contiene una sequenza di b cifre ordinate per valore crescente
; la seconda stringa contiene il numero scritto in base b da convertire in base 10 (razionale)

; metodo che converte la cifra di base b in valore intero
(define rep-digit
  (lambda (digits c)
    (if (= (string-length digits) 0)
        (error "char not valid")
        (if (char=? c (string-ref digits 0))
            0
            (add1 (rep-digit (substring digits 1 (string-length digits)) c)))
        )
    ))
; metodo che prende l'alfabeto della base b, una stringa scritta in base b e la converte in intero
(define rep->integer ; string
  (lambda (digits str)
    (let ([next-str-length (sub1 (string-length str))])
      (let ([bin (rep-digit digits (string-ref str next-str-length))])
        (if (= next-str-length 0)
            bin
            (+ bin (* (string-length digits) (rep->integer digits (substring str 0 next-str-length))))
            )
        )
      )
    ))

(define rep->number ; string
  (lambda (digits str) ; string, string
    (* (sign-value str) (/ (rep->integer digits (normalize-rep str)) (expt (string-length digits) (find-decimal-point str))))
    ))

; testing
"--- Parte B ---"
(= (rep->number "zu" "-uuzz") -12)
(= (rep->number "0123" "+21.1") 9.25)
(= (rep->number "01234" "-10.02") -5.08)
(= (rep->number "0123456789ABCDEF" "0.A") 0.625)
(= (rep->number "0123456789ABCDEF" "1CF.0") 463)
  

       

