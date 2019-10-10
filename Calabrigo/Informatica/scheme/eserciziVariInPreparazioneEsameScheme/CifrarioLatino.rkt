;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname CifrarioLatino) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
;cifrario latino
;A, B, C, D, E, F, G, H, I, K, L, M, N, O, P, Q, R, S, T, V, X, Y, Z

;(list "a" "b" "c" "d" "e" "f" "g" "h" "i" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "v" "x" "y" "z")

(define encrypt
  (lambda (str rule)
    (if (= (string-length str) 0)
        ""
        (string-append (string (rule (string-ref str 0))) (encrypt (substring str 1) rule))
        )
    ))

(define rotate
  (lambda (rot)
    (lambda (letter)
      (if (> (+ (latin->integer letter) rot) 22)
          (integer->latin (- (+ (latin->integer letter) rot) 23))
          (integer->latin (+ (latin->integer letter) rot)))
      )
    ))


(define latin->integer
  (lambda (char)
    (cond ((char=? char #\a) 0)
          ((char=? char #\b) 1)
          ((char=? char #\c) 2)
          ((char=? char #\d) 3)
          ((char=? char #\e) 4)
          ((char=? char #\f) 5)
          ((char=? char #\g) 6)
          ((char=? char #\h) 7)
          ((char=? char #\i) 8)
          ((char=? char #\k) 9)
          ((char=? char #\l) 10)
          ((char=? char #\m) 11)
          ((char=? char #\n) 12)
          ((char=? char #\o) 13)
          ((char=? char #\p) 14)
          ((char=? char #\q) 15)
          ((char=? char #\r) 16)
          ((char=? char #\s) 17)
          ((char=? char #\t) 18)
          ((char=? char #\v) 19)
          ((char=? char #\x) 20)
          ((char=? char #\y) 21)
          ((char=? char #\z) 22))
    ))

(define integer->latin
  (lambda (n)
    (cond ((= n 0) #\a)
          ((= n 1) #\b)
          ((= n 2) #\c)
          ((= n 3) #\d)
          ((= n 4) #\e)
          ((= n 5) #\f)
          ((= n 6) #\g)
          ((= n 7) #\h)
          ((= n 8) #\i)
          ((= n 9) #\k)
          ((= n 10) #\l)
          ((= n 11) #\m)
          ((= n 12) #\n)
          ((= n 13) #\o)
          ((= n 14) #\p)
          ((= n 15) #\q)
          ((= n 16) #\r)
          ((= n 17) #\s)
          ((= n 18) #\t)
          ((= n 19) #\v)
          ((= n 20) #\x)
          ((= n 21) #\y)
          ((= n 22) #\z))
    ))


(define encRule (rotate 2))

(encrypt "givlivscaesar" encRule)
          

;(define encRule (rotateLetter 3))
;encRule
;(define encrypt encRule)
;(encrypt "add" 3)