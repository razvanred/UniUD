;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.es1) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
;1
(define powers-of-two        ; val: lista
  (lambda (n)                ; n: intero>=0
    (powers-of-two-rec n 1 null)
    ))

(define powers-of-two-rec    ; val: lista
  (lambda (n a l)            ; n: intero>=0, a: intero>=1, l: lista
    (cond ((= n 0) l)
        ((= (remainder n 2) 1)
         (powers-of-two-rec (quotient n 2) (* 2 a) (cons a l)))
        (else
         (powers-of-two-rec (quotient n 2) (* 2 a) l)))
    ))

(powers-of-two 0)
(powers-of-two 1)
(powers-of-two 5)
(powers-of-two 8)
(powers-of-two 26)
(powers-of-two 45)
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;2
(define manhattan-var       ; val: intero
  (lambda (i j k)           ; i,j,k: interi k<=i e k<=j
    (let ((x (if (= i k) 0 (manhattan-var (- i 1) j k)))
          (y (if (= j k) 0 (manhattan-var i (- j 1) k)))
          (z (if (= k 0) 0 (manhattan-var (- i 1) (- j 1) (- k 1)))))
      (if (and (> i 0) (> j 0))
          (+ x y z)
          1))
    ))

(manhattan-var 3 2 0)
(manhattan-var 3 2 2)
(manhattan-var 2 2 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;3
(define ufo
  (lambda (n)
    (cond ((= n 1) 1)
          ((even? n)
           (- (* 2 (ufo (quotient n 2))) 1))
          (else
           (+ (* 2 (ufo (quotient n 2))) 1)))
    ))

(ufo 7)
(ufo 14)
(ufo 28)
(ufo 56)
;3.1
; per ogni k appartenente ad N (ufo di 7*2^k) -> (3*2^(k+1)+1)
;3.2
; caso base: k=0
; (ufo 7*2^0)->(ufo 7)
; 3*2^(0+1)+1=7
; (ufo 7)
; (+ (* 2 (ufo (quotient 7 2))) 1)
; (+ (* 2 (ufo 3)) 1)
; (+ (* 2 (+ (* 2 (ufo 1)) 1))) 1)
; (+ (* 2 (+ (* 2 1) 1))) 1)=7
; 7=7
;3.3
; scelgo k in N ed assumo che (ufo di 7*2^[k]) -> (3*2^([k]+1)+1)
; dimostro (ufo di 7*2^([k+1])) -> (3*2^([k+1]+1)+1)
;  (ufo [7*2^(k+1)])->(- (* 2 (ufo (quotient [7*2^(k+1)] 2))) 1)->
;  ->(- (* 2 (ufo [7*2^k])) 1)->(- (* 2 (3*2^([k]+1)+1)) 1)->
;  ->(- [3*2^(k+1)+2] 1)->[3*2^([k+1]+1)+1]->
;  ->(3*2^([k+1]+1)+1)


;1
(define eratosthenes
  (lambda (n)
    (sieve 2 n (lambda (i) (and (>= i 2) (<= i n))));;;;;
  ))

(define sieve
  (lambda (k n p?)
    (cond ((> k n)
           p?)
          ((p? k)
           (sieve (+ k 1) n
                  (lambda (x)
                    (if (and (> x k) (= (remainder x k) 0));;;;;
                        false
                        (p? x)));;;;;
                  ))
          (else
           (sieve (+ k 1) n p?));;;;;
          )))

(eratosthenes 11)

;2
(define f
  (lambda (s)
    (g s 0)
    ))

(define g
  (lambda (s b)
    (if (null? s)
        0
        (if (<= (car s) b)
            (g (cdr s) b)
            (max (g (cdr s) b)
                 (+ (g (cdr s) (car s)) 1))
            ))))

(f '(5))
(f '(1 2))
(f '(1 3 5))
(f '(5 3))
(f '(5 3 1))
(f '(1 5 3))
(f '(4 2 3 1 5))

;3
(define mh
  (lambda (i j)
    (if (or (= i 0) (= j 0))
        1;;;;;
        (+ (md (- i 1) j) (mr i (- j 1)))
        )))

(define md
  (lambda (i j)
    (if (or (= i 0) (< j 2));;;;;
        1
        (+ (md (- i 1) j) (mr i (- j 2)));;;;;
        )))

(define mr
  (lambda (i j)
    (if (or (< i 2) (= j 0));;;;;
        1
        (+ (md (- i 2) j) (mr i (- j 1)));;;;;
        )))

(mh 2 2)
(mh 2 3)

;4
; in relazione alla rocedura g definira nell'esercizio 2
; si può dimostrare per induzione sul parametro k che (g (n+1, n+2, ... , n+k) q) -> k
; con n>=q>=0
;4.1 formalizza
; per ogni k>0 , blah blah blah
;4.2 caso base k=1
; per ogni n,q n>=1>=0
; (g (n+1) q) -> 1
; (if (null? (n+1))       else
;   0
;   (if (<= [n+1] q)
;      (g [null] q)
;      (max (g [null] q)  0
;           (+ (g [null] [n+1]) (g [null] q) 1)   0+0+1
;      ))))
; =
; (if (<= [n+1] q)     else
;      (g [null] q)
;      (max (g [null] q)  0
;           (+ (g [null] [n+1]) (g [null] q) 1)   0+0+1
;      ))
; =
;  (max (g [null] q)  0
;       (+ (g [null] [n+1]) (g [null] q) 1)   0+0+1
;  )
; =
;  (max 0 1)
; =
; 1
;4.3 passo induttivo
; ipotesi induttiva:
;  per ogni n,q n>=1>=0
;  (g (n+1, n+2, ... , n+h) q) -> h
; per  scelto sopra dimostro che (g (n+1, n+2, ... , n+[h+1]) q) -> [h+1]
;    (if (null? (n+1, n+2, ... , n+[h+1]))
;        0
;        (if (<= (car (n+1, n+2, ... , n+[h+1])) q)
;            (g (cdr (n+1, n+2, ... , n+[h+1])) q)
;            (max (g (cdr s) q)
;                 (+ (g (cdr (n+1, n+2, ... , n+[h+1])) (car (n+1, n+2, ... , n+[h+1]))) 1))
;            )
; =
;        (if (<= (car (n+1, n+2, ... , n+[h+1])) q)
;            (g (cdr (n+1, n+2, ... , n+[h+1])) q)
;            (max (g (cdr s) q)
;                 (+ (g (cdr (n+1, n+2, ... , n+[h+1])) (car (n+1, n+2, ... , n+[h+1]))) 1))
;            )
; =
;        (if (<= [n+1] q)    q<=n ==> q<[n+1]
;            (g ([n-1]+1, [n-1]+2, ... , [n-1]+h) q)
;            (max (g ([n+1]+1, [n+1]+2, ... , [n+1]+h) q)                  ->   q<=[[n+1]+1] ==> (g ([n+1]+1, [n+1]+2, ... , [n+1]+h) q)->h
;                 (+ (g ([n+1]+1, [n+1]+2, ... , [n+1]+h) [n+1]) 1))       ->   [n+1]<=[[n+1]+1] ==> (g ([n+1]+1, [n+1]+2, ... , [n+1]+h) [n+1])->h
;            )
; =
;            (max [h]
;                 (+ [h] 1))
; =
;            (max [h] [h+1])
; =
;  h+1