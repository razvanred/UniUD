;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Mirolante) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define st-list ; val: lista di stringhe di cifre
  (lambda (n k) ; n, k: interi tali che 1 ≤ k ≤ n e k ≤ 9
    (cond ((and (= n 1) (= k 1))
           (list "1"));--
          ((= k 1)
           (list (string-append (car (st-list (- n 1) 1)) "1")));--
          ((= k n)
           (list (string-append (car (st-list (- n 1) (- k 1))) (number->string k))))
          (else
           (append
            (map (lambda (x) (string-append x (number->string k)))
                 (st-list (- n 1) (- k 1)))
            (comb-append (st-list (- n 1) k) k)));;;
          )))

(define comb-append
  (lambda (u i)
    (if (= i 0)
        null
        (append
         (comb-append u (- i 1))
         (map (lambda (x) (string-append x (number->string i) )) u));;;
        )))

(st-list 4 3)



;;;;;;;;;;;;;;;;;;;;;;;

(define word-count
  (lambda (s)
    (if (= (string-length s) 0)
        0
        (if (is-permitted? (string-ref s 0))
            (+ 1
               (word-skip (substring s 1)))
            (word-count (substring s 1))))
    ))

(define word-skip
  (lambda (s)
    (if (= (string-length s) 0)
        0
        (if (is-permitted? (string-ref s 0))
            (word-skip (substring s 1))
            (word-count (substring s 1))))
    ))



(define is-letter?
  (lambda (c)
    (or
     (and (char<=? #\a c) (char>=? #\z c))
     (and (char<=? #\A c) (char>=? #\Z c)))
    ))

(define is-number?
  (lambda (c)
    (and (char<=? #\0 c) (char>=? #\9 c))
    ))

(define is-permitted?
  (lambda (c)
    (or
     (is-letter? c)
     (is-number? c))
    ))



(define word-count-alt
  (lambda (s)
    (word-count-alt-rec #f 0 s)
    ))

(define word-count-alt-rec
  (lambda (p a s)
    (if (= (string-length s) 0)
        a
        (if (is-permitted? (string-ref s 0))
            (word-count-alt-rec #t (+ (if p 0 1) a) (substring s 1))
            (word-count-alt-rec #f a (substring s 1))))
    ))



(define word-count-2
  (lambda (s)
    (if (= (string-length s) 0)
        0
        (word-count-2-rec s #\.)
        )))

(define word-count-2-rec
  (lambda (s prec)
    (if (= (string-length s) 0)
        0
        (if (and (is-permitted? (string-ref s 0)) (not (is-permitted? prec)))
            (+ (word-count-2-rec (substring s 1) (string-ref s 0)) 1)
            (word-count-2-rec (substring s 1) (string-ref s 0))
     ))))


(word-count "")
(word-count " ... ?; ")
(word-count "3o esercizio della prova scritta di PROGRAMMAZIONE.")
(word-count " --- film: L'albero degli zoccoli (1978) / regista: E. Olmi ")

(word-count-alt "")
(word-count-alt " ... ?; ")
(word-count-alt "3o esercizio della prova scritta di PROGRAMMAZIONE.")
(word-count-alt " --- film: L'albero degli zoccoli (1978) / regista: E. Olmi ")

(word-count-2 "")
(word-count-2 " ... ?; ")
(word-count-2 "3o esercizio della prova scritta di PROGRAMMAZIONE.")
(word-count-2 " --- film: L'albero degli zoccoli (1978) / regista: E. Olmi ")
(word-count-2 "il batacchio di lord Pichenchester era assai gradito alla signora Rottermayer")