;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.10.3) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
(define °        ; val: procedura
  (lambda (g f)  ; f,g: procedure
    (lambda (x)  ; x: reale
      (g (f x))
      )
    ))

(define iter     ; val: procedura
  (lambda (k f)  ; k: intero, f: procedura
    (lambda (x)
      (if (= k 0)
          x
          (f ((iter (- k 1) f) x))
          ))
    ))

(define iter2    ; val: procedura
  (lambda (k f)  ; k: intero, f: procedura
      (if (= k 0)
          (lambda (x) x)
          (° f (iter2 (- k 1) f))
          )
    ))


(define f
  (lambda (x)
    (* x 6)
    ))

(define g
  (lambda (x)
    (+ x 4)
    ))