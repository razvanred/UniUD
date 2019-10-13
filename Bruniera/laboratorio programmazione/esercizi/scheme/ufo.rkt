;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ufo) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
;
(define mul     ; val: inter
  (lambda (x y) ; x,y >= 0 interi
    (mul-rec x y 0)
    ))

(define mul-rec   ; val: intero
  (lambda (x y z) ; x,y,z >= 0 interi
    (cond ((= y 0) z)
          ((even? y) (mul-rec (* 2 x) (quotient y 2) z))
          (else (mul-rec (* 2 x) (quotient y 2) (+ z x)))
          )))
;;;;;;;;;;;
(define ufo
  (lambda (x)
    (cond  ((= x 1) 1)
           ((even? x) (- (* 2 (ufo (quotient x 2))) 1))
           (else (+ (* 2 (ufo (quotient x 2))) 1)))
    ))
; a) (ufo 2^k) --> 1       per k>0 intero
; b) (ufo n) --> d         ==> d dispari
; c) (ufo n) --> d         ==> (ufo n+1) --> d+2 se n+1!=2^k 
; d) (ufo n) --> d         ==> d<=n
; e) (ufo 2^k-1) --> 2^k-1 per k>0 intero

; per ogni k appartenente ad N e j appartenente a [0,2^k-1]
; (*) = (ufo 2^k+j) --> 2j+1

; (*) ==> a   j=0 ==> 2j+1=1
; (*) ==> b   2j-1 è dispari
; (*) ==> c   2(j+1)+1 = 2j+1+2
; (*) ==> d   j<2^k ==> 2j<=2^k
; (*) ==> e   2^k-1 = 2^(k-1)+2^(k-1)-1 ==> 2(2^(k-1)-1)+1 = 2^k-2+1 = 2^k-1

; DIMOSTRAZIONE PER INDUZIONE
; caso base [k=0]:
;  0<=j<=2^0-1 -> j=0 ->
;  -> 2j+1=1 = (ufo 1)
; passo induttivo [scelgo k in N]:
; [assumo per ogni j in [0,2^k-1] (ufo 2^k+j) --> 2j+1]
;  per ogni j in [0,2^(k+1)-1]
;  (ufo 2^(k+1)+j) --> ?
;   a) assumo che j sia pari
;    (- (* 2 (ufo (quotient [2^(k+1)+j] 2))) 1) ->
;    (- (* 2 (ufo [2k+j/2])) 1) ->
;      j in [0,2^(k+1)-1] ==> j/2 in [0,2^k] ?
;      è vera perché j è pari (il più grande numero pari nel primo intervallo è 2*(2^k-1))
;    (- (* 2 [j+1]) 1) -> (- [2j+2] 1) ->
;    -> 2j+1
;   b) assumo che j sia dispari
;    (+ (* 2 (ufo (quotient [2^(k+1)+j] 2))) 1) ->
;    (+ (* 2 (ufo [2k+(j-1)/2])) 1) ->
;      j in [0,2^(k+1)-1] ==> (j-1)/2 in [0,2^k] ?
;      è vera perché j-1 può valere al massimo  2*(2^k-1)
;    (+ (* 2 [(j-1)+1]) 1)->
;    (+ [2(j-1)+2] 1) ->
;    (+ [2j-2+2]) ->
;    (+ [2j] 1) ->
;    -> 2j+1
(ufo 1)
(ufo 2)
(ufo 3)
(ufo 4)
(ufo 5)
(ufo 6)
(ufo 7)
(ufo 8)
(ufo 9)
(ufo 10)
(ufo 11)
(ufo 12)
(ufo 13)
(ufo 14)
(ufo 15)
(ufo 16)
(ufo 17)
(ufo 18)
(ufo 19)
(ufo 20)
(ufo 21)
(ufo 22)
(ufo 23)
(ufo 24)
(ufo 25)
(ufo 26)
(ufo 27)
(ufo 28)
(ufo 29)