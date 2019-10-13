;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Problema9) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss" "installed-teachpacks")))))
;(list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\V #\X)
; N.B.: l'algoritmo di criptazione shifta solo le LETTERE MAIUSCOLE dell'ALFABETO LATINO,
;       tutti gli altri caratteri ascii restano invariati


;ESEMPIO
;((crypt 3) "ALEA IACTA EST IVLIVS CAESAR DIXIT")

(define crypt   ;ritorno una procesura che shifta di key
  (lambda (key)
    (lambda (message)
      (shift-message message key)
      )))
       
(define shift-message   ;shifto tutte le lettere di una messaggio di shift posizioni
  (lambda (message shift)
    (shift-message-rec message shift)
    ))

(define shift-message-rec
  (lambda (message shift)
    (if (= (string-length message) 0)
        ""
        (string-append (string (shift-letter (string-ref message 0) shift)) (shift-message-rec (substring message 1) shift))
        )
    ))

(define shift-letter   ;shifto una lettera (letter) di n posizioni(shift) dell' alfabeto
  (lambda(letter shift)
    (if (or (= shift 0) (< (char->latinInt letter) 300)) ;300 corrisponde al carattere #\A
        letter
        (if (> (- 19 (letter-pos letter (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\V #\X))) 0)
            (shift-letter (latinInt->char (+ (char->latinInt letter) 1)) (- shift 1))
            (shift-letter (latinInt->char 300) (- shift 1))
            )
        )
    ))

(define letter-pos  ;ritorno la posizione di una lettera nell'alfabeto
  (lambda(letter alphabet)
    (if (char=? letter (car alphabet))
        0
        (+ (letter-pos letter (cdr alphabet)) 1)
        )
    ))

;ho dato un codice cosi alto al mio alfabeto
;perchè doveva essere superiore a qualsiasi simbolo ascii (128)/ascii extended(256)
(define char->latinInt
  (lambda (char)
    (cond ((char=? char #\A) 300)
          ((char=? char #\B) 301)
          ((char=? char #\C) 302)
          ((char=? char #\D) 303)
          ((char=? char #\E) 304)
          ((char=? char #\F) 305)
          ((char=? char #\G) 306)
          ((char=? char #\H) 307)
          ((char=? char #\I) 308)
          ((char=? char #\L) 309)
          ((char=? char #\M) 310)
          ((char=? char #\N) 311)
          ((char=? char #\O) 312)
          ((char=? char #\P) 313)
          ((char=? char #\Q) 314)
          ((char=? char #\R) 315)
          ((char=? char #\S) 316)
          ((char=? char #\T) 317)
          ((char=? char #\V) 318)
          ((char=? char #\X) 319)
          (else (char->integer char))
    )))
  
  (define latinInt->char
  (lambda (latinInt)
    (cond ((= latinInt 300) #\A)
          ((= latinInt 301) #\B)
          ((= latinInt 302) #\C)
          ((= latinInt 303) #\D)
          ((= latinInt 304) #\E)
          ((= latinInt 305) #\F)
          ((= latinInt 306) #\G)
          ((= latinInt 307) #\H)
          ((= latinInt 308) #\I)
          ((= latinInt 309) #\L)
          ((= latinInt 310) #\M)
          ((= latinInt 311) #\N)
          ((= latinInt 312) #\O)
          ((= latinInt 313) #\P)
          ((= latinInt 314) #\Q)
          ((= latinInt 315) #\R)
          ((= latinInt 316) #\S)
          ((= latinInt 317) #\T)
          ((= latinInt 318) #\V)
          ((= latinInt 319) #\X)
          (else (integer->char latinInt))
    )))