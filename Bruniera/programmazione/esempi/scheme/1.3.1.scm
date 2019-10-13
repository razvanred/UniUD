;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 1.3.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define sup-tot-cil               ; val: misura
  (lambda (rb h)                  ; rb, h: misure
    (* 2 (* pi (* rb (+ h rb))))
    )
  )
