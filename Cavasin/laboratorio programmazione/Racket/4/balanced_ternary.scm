;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname balanced_ternary) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Rappresentazione ternaria bilanciata
;; Balanced Ternary Representation (BTR)
;;
;; Procedure ricorsive che operano su stringhe
;; Ultimo aggiornamento: 17/11/11


;; Interpretazione della rappresentazione ternaria bilanciata:
;;
;;   rappresentazione: stringa BTR  -->  valore: intero

(define btr-val                                               ; valore: intero
  (lambda (btr)                                               ; btr: stringa non vuota di -/./+
    (let ((k (- (string-length btr) 1))
          )
      (let ((pre (substring btr 0 k))                         ; pre = prefix
            (lsd (string-ref btr k))                          ; lsd = least significant digit
            )                                                 ; per facilitare la leggibilita'
        (if (= k 0)
            (btd-val lsd)
            (+ (* 3 (btr-val pre)) (btd-val lsd))
            )))
    ))


;; Interpretazione di una cifra:

(define btd-val                                               ; valore: [-1, 0, 1]
  (lambda (btd)                                               ; d: carattere -/./+
    (cond ((char=? btd #\-) -1)
          ((char=? btd #\.)  0)
          ((char=? btd #\+) +1)
          )
    ))


;; Codifica in forma ternaria bilanciata:
;;
;;   valore: intero  -->  rappresentazione: stringa BTR

(define btr-rep                                               ; valore: stringa di +/./-
  (lambda (n)                                                 ; n: intero
    (let ((r (remainder n 3)) (q (quotient n 3))              ; n = 3q + r  dove  -2 <= r <= +2
          )
      (cond ((= r -2)
             (string-append (btr-rep (- q 1)) (btd-rep +1)))  ; n = 3q - 2  ==>  n = 3(q-1) + 1,  q-1 < 0
            ((= r +2)
             (string-append (btr-rep (+ q 1)) (btd-rep -1)))  ; n = 3q + 2  ==>  n = 3(q+1) - 1,  q+1 > 0
            ((= q  0)
             (btd-rep r))                                     ; n = r
            (else
             (string-append (btr-rep q) (btd-rep r)))         ; n = 3q + r,  q <> 0
            ))
    ))


;; Cifra ternaria bilanciata:

(define btd-rep                                               ; valore: stringa "-", ".", "+"
  (lambda (v)                                                 ; v: [-1, 0, 1]
    (cond ((= v -1) "-")
          ((= v  0) ".")
          ((= v +1) "+")
          )
    ))


;; Codifica in forma ternaria bilanciata (variante A):
;;
;; Calcolo a partire dalla cifra piu' significativa
;;
;; (define btr-rep      ; valore: stringa di -/./+
;;   (lambda (n)        ; n: intero
;;     (btr-pow n 3)
;;     ))
;;
;;
;; (define btr-pow      ; valore: stringa di -/./+
;;   (lambda (n p)      ; n, p: interi
;;     (if (< (* 2 (abs n)) p)
;;         (btr-rec n (/ p 3))
;;         (btr-pow n (* 3 p))
;;         )))
;;
;;
;; (define btr-rec      ; valore: stringa di -/./+
;;   (lambda (n p)      ; n, p: interi
;;     (cond ((< p 1)
;;            "")
;;           ((< (* 2 n) (- p))
;;            (string-append "-" (btr-rec (+ n p) (/ p 3))))
;;           ((> (* 2 n) p)
;;            (string-append "+" (btr-rec (- n p) (/ p 3))))
;;           (else
;;            (string-append "." (btr-rec n       (/ p 3))))
;;           )))


;; Codifica in forma ternaria bilanciata (variante B):
;;
;; Qual e' la logica del seguente programma?
;;
;; (define btr-rep      ; valore: stringa di -/./+
;;   (lambda (n)        ; n: intero
;;     (let ((u (shifted n))
;;           )
;;       (ter->btr (car u) (cadr u))
;;       )))            ;   cadr: secondo elemento di una lista
;;
;; (define ter->btr     ; valore: stringa di -/./+
;;   (lambda (n k)      ; n, k: interi
;;     (let ((r (remainder n 3)) (q (quotient n 3))
;;           )
;;       (if (= k 1)
;;           (shifted-btd r)
;;           (string-append (ter->btr q (- k 1)) (shifted-btd r))
;;           ))
;;     ))
;;
;;
;; (define shifted      ; valore: lista di due interi (coppia)
;;   (lambda (n)        ; n: intero
;;     (shifted-rec n 1 3)
;;     ))
;;
;; (define shifted-rec  ; valore: lista di due interi (coppia)
;;   (lambda (n k p)    ; n, k, p: interi
;;     (if (< (* 2 (abs n)) p)
;;         (list (+ n (quotient p 2)) k)
;;         (shifted-rec n (+ k 1) (* 3 p))
;;         )))
;;
;;
;; (define shifted-btd  ; valore: stringa "-", ".", "+"
;;   (lambda (v)        ; v: [-1, 0, 1]
;;     (cond ((= v 0) "-")
;;           ((= v 1) ".")
;;           ((= v 2) "+")
;;           )
;;     ))


;; Esercizio: Confronta i risultati dell'applicazione dei tre diversi
;; programmi per calcolare la rappresentazione BTR dei numeri interi.

