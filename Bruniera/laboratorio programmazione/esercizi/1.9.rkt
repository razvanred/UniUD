;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1.9|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
(define naugthy (list ))
(define nice (list ))
;; i'm gonna find out!


;es 1
(define alfabeto "ABCDEFGHILMNOPQRSTVX")

(define encryption      ; val: stringa
  (lambda (msg rule)    ; msg: stringa, rule: procedura
    (if (string=? msg "")
        ""
        (let ((fc (string-ref msg 0)))
        (string-append
         (if (char=? fc #\space)
             ""
             (string (rule fc)))
         (encryption
          (substring msg 1) rule))))
    ))

(define position     ; val: intero
  (lambda (c al)     ; c: char, al: stringa
    (if (or (string=? al "")
         (char=? c (string-ref al 0)))
        0
        (+ 1
           (position c (substring al 1))))
    ))

(define ruler        ; val: procedura
  (lambda (k)        ; k: intero 0<=k<=19
    (lambda (c)      ; c: carattere
      (let ((ic (position c alfabeto)))
        (let ((nc (modulo (+ ic k) (string-length alfabeto))))
          (string-ref alfabeto nc)))
      )
    ))

(encryption "ALEA IACTA EST IVLIVS CAESAR DIXIT" (ruler 3))
(encryption "ABCDEFGHILMNOPQRSTVX" (ruler 0))
(encryption "ABCDEFGHILMNOPQRSTVX" (ruler 1))
(encryption "ABCDEFGHILMNOPQRSTVX" (ruler 2))
(encryption "ABCDEFGHILMNOPQRSTVX" (ruler 3))
(encryption "ABCDEFGHILMNOPQRSTVX" (ruler -1))
(encryption "ABCDEFGHILMNOPQRSTVX" (ruler -2))
(encryption "ABCDEFGHILMNOPQRSTVX" (ruler -3))
;;;;;;;;;;;;;;;;;;

;es 2
(define s2           ; val: intero
  (lambda (u v)      ; u: intero, v: intero
    (+ v 1)
    ))

(define H            ; val: procedura
  (lambda (f g)      ; f: procedura, g: procedura
    (lambda (u v)    ; u: intero, v: intero
      (if (= v 0)
          (f u)
          (g u ((H f g) u (- v 1))))
      )
    ))

(define i            ; val: intero
  (lambda (n)        ; n: intero
    n
    ))

(define z            ; val: intero
  (lambda (n)        ; n: intero
    0
    ))

(define u            ; val: intero
  (lambda (n)        ; n: intero
    1
    ))
  
(define add (H i s2))
(define mul (H z add))
(define pow (H u mul))
(define tet (H u pow)); tetrazione, non usare per nessun motivo
(define te1 (H u expt)); tetrazione, non usare possibilmente

(s2 789 0)
(s2 245 1)
(s2 264 7)
(s2 874 18)
(add 6 0)
(add 6 1)
(add 6 3)
(add 6 8)
(mul 6 0)
(mul 6 1)
(mul 6 3)
(mul 6 8)
(pow 6 0)
(pow 6 1)
(pow 6 3)
(te1 6 0)
(te1 6 1)
(te1 6 3)