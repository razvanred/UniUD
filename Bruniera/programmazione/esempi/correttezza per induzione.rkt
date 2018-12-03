;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |correttezza per induzione|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
;correttezza del codice rispetto al problema

(define f          ; val: intero      2n-1  D[n>0]
  (lambda (i)      ; i>0 intero
    (if (= i 1)
        i
        (+ (f (- i 1)) 2)
        )))

;INDUZIONE:
;caso base:
; dimostro l'espressione valida per n=1
;  (f 1) = 1 = 2*1-1
;caso ricorsivo:
; scelgo un m naturale
; assumo (f m)=2m-1
; dimostro (f m+1)= 2(m+1)-1
;  (f m+1) -> (+ (f (- m+1 1)) 2) -> (+ (f m) 2) -> (+ (2m-1) 2) ->
;  -> 2m-1+2 -> (2m+2)-1 -> 2(m+1)-1




(define unknown     ; val: intero     n^2   D[N]
  (lambda (x)       ; i: intero
    (if (= x 0)
        0
        (+ (unknown (- x 1)) (f x))
        )))

;INDUZIONE:
;caso base:
; dimostro l'espressione valida per n=0
;  (unknown 0) = 0 = 0^2
;caso ricorsivo:
; scelgo un m naturale
; assumo (unknown n)=n^2
; dimostro (unknown m+1)= (m+1)^2
;  (unknown m+1) -> (+ (unknown (- m+1 1)) (f m+1)) -> (+ (unknown m) 2(m+1)-1) ->
;  -> (+ m^2 2m+1) -> m^2+2m+1 -> (m+1)^2




(define power     ; val: intero        n^k     D[n[N+], k[N]]
  (lambda (x y)   ; x>0,y>=0 interi
    (if (= y 0)
        1
        (* x (power x (- y 1)))
        )))

;INDUZIONE su k[N]:
;caso base:
; dimostro l'espressione valida per k=0
;  (power n 0) = 1 = n^0
;caso ricorsivo:
; scelgo un m naturale
; assumo (power n m)=n^m
; dimostro (power n m+1)= n^(m+1)
;  (power n m+1) -> (* n (power n (- m+1 1))) -> (* n (power n m)) ->
;  (* n n^m) -> n*n^m -> n^(m+1)