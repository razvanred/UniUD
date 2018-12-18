;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1.9|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
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