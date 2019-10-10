;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname decimaletoternariobalanced) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; 0 = .  --- 1 = + ---- 2 = |-
;#lang racket


;(define TerToBalancedTer
  ;(lambda (num)
    
; )
;)

(define DecToBalance
  (lambda(num)
    (if (and(=(quotient num 3)0)(=(remainder num 3)0))
    ""
    (string-append
     (cond ((=(remainder num 3)2)
                (DecToBalance (+(quotient num 3)1))
            )
           ((=(remainder num 3)-2)
                (DecToBalance (-(quotient num 3)1))
            )
           (else
                (DecToBalance (quotient num 3))
           )
      )
     (resto num)
    )
    )
  )
)   
(define resto
  (lambda (num)
    (cond ((= (remainder num 3) 0) ".")
          ((= (remainder num 3) 1) "+")
          ((= (remainder num 3) 2) "-")
          ((= (remainder num 3) -1) "-")
          ((= (remainder num 3) -2) "+")
    )
   )
 )

(DecToBalance 46)
