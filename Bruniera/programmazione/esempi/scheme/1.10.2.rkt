;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.10.2) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
(define pol     ; val: procedura
  (lambda (cs)  ; cs: lista non vuota di reali
    (lambda (x) ; x: variabile reale del polnomio
      (if (= (length cs) 1)
          (car cs)
          (+ (car cs)
             (* x
                ((pol (cdr cs)) x)))
          )
      )
    ))

