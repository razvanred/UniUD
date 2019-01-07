;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.ex1) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
; 1

; or
; (vh (string-ref v 0))
; (match (substring u 1) (substring v 1))
; char=?
; (substring u 0 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 2

; string
; (string-ref num (- digits 1))
; (increment (substring num 0 (- digits 1)) base)
; (next-digit dgt)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 3

; 0 0
; (string-ref u 0) (string-ref v 0)
; (list i j (string-ref u 0))
; (lcs-rec i u (+ j 1) (substring v 1))
; (lcs-rec (+ i 1) (substring u 1) j v)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 4

(define cyclic-string                 ; val: string
  (lambda (pattern n)                 ; pattern: string, n: int
    (if (= n 0)
        ""
        (let ((letp (modulo (- n 1) (string-length pattern))))
        (string-append
         (cyclic-string pattern (- n 1))
         (string (string-ref pattern letp)))))
    ))
"es 4"
(cyclic-string "abcd" 6)

; 5

(define av                            ; val: list
  (lambda (a)                         ; a: list
    (let ((b (cdr a)))
      (if (null? b)
          null
          (let ((1a (car a))
                (1b (car b)))
            (cons
             (cond ((> (+ 1a 1b) 0) 1)
                   ((< (+ 1a 1b) 0) -1)
                   (else 0))
             (av b))))
    )))
"es 5"
(av '(0 0 -1 -1 1 0 0 1 0))

; 6

(define r-val           ; val: razionale
  (lambda (bin)         ; bin: string
    (if (char=? (string-ref bin 0) #\.)
        (r-val-rec (substring bin 1) 0 0.5)
        0)
    ))

(define r-val-rec       ; val: razionale
  (lambda (bin v p)     ; bin: string, v: intero, p: razionale
    (if (> (string-length bin) 0)
        (if (char=? (string-ref bin 0) #\1)
            (r-val-rec (substring bin 1) (+ v p) (/ p 2))
            (r-val-rec (substring bin 1) v (/ p 2)))
        v)
    ))
"es 6"
(r-val ".1")
(r-val ".011")

; 7

(define shared       ; val: list
  (lambda (a b)      ; a,b: list
    (if (or (null? a) (null? b))
        null
        (let ((ca (car a))
              (cb (car b))
              (ad (cdr a))
              (bd (cdr b)))
          (cond ((> ca cb) (shared a bd))
                ((< ca cb) (shared ad b))
                (else (cons ca (shared ad bd))))))
    ))
"es 7"
(shared '(1 3 5 6 7 8 9 10) '(0 1 2 3 4 5 7 9))

; 8

(define parity-check-failures   ; val: list
  (lambda (a)                   ; a: list
    (parity-check-failures-rec a 0)
    ))

(define string-1-count          ; val: boolean
  (lambda (s)                   ; s: string
    (if (> (string-length s) 0)
        (if (char=? (string-ref s 0) #\1)
            (+ 1 (string-1-count (substring s 1)))
            (string-1-count (substring s 1)))
        0)
    ))

(define parity-check-failures-rec ; val: list
  (lambda (a n)                   ; a: list, n: intero
    (if (null? a)
        null
        (if (even? (string-1-count (car a)))
            (parity-check-failures-rec (cdr a) (+ n 1))
            (cons n (parity-check-failures-rec (cdr a) (+ n 1)))))
    ))
"es 8"
(parity-check-failures '("0111" "1001" "0000" "1010"))
(parity-check-failures '("0110" "1101" "0000" "1011"))
(parity-check-failures '("0111" "1011" "0100" "1110"))
(parity-check-failures '("0110" "1001" "0000" "1010"))

; 9

(define closest-pair            ; val: list
  (lambda (a)                   ; a: list
    (closest-pair-rec (cdr a) (list (car a) (car (cdr a))))
    ))

(define closest-pair-rec        ; val: list
  (lambda (a c)                 ; a,c: list
    (let ((b (cdr a)))
      (if (null? b)
        c
        (let ((a1 (car a))
              (b1 (car b))
              (c1 (car c))
              (c2 (car (cdr c))))
          (if (< (- b1 a1) (- c2 c1))
              (closest-pair-rec b (list a1 b1))
              (closest-pair-rec b c)))))
    ))
"es 9"
(closest-pair '(0.1 0.3 0.5 0.6 0.8 1.1))
(closest-pair '(0.1 0.6))

; 10

(define sorted-char-list       ; val: list
  (lambda (s)                  ; s: string
    (sorted-char-list-rec s null)
    ))

(define sorted-char-list-rec   ; val: list
  (lambda (s l)                ; s: string, l: list
    (if (> (string-length s) 0)
        (sorted-char-list-rec
         (substring s 1)
         (sorted-insert-char (string-ref s 0) l))
        l)
    ))

(define sorted-insert-char     ; val: list
  (lambda (c l)                ; c: char, l: list
    (if (null?  l)
        (list c)
        (cond ((char<? c (car l)) (cons c l))
              ((char>? c (car l)) (cons (car l) (sorted-insert-char c (cdr l))))
              (else l)))
    ))
"es 10"
(sorted-char-list "")
(sorted-char-list "abc")
(sorted-char-list "cba")
(sorted-char-list "list of chars that occur in this text")