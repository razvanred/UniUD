;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname RicorsioneECond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Calcolo fogli AK-

;s(k)=1/2*s(k-2)   la  lunghezza del lato + lungo di un foglio akappa (es. a5) è uguale alla metà della lunghezza del lato + lungo di un foglio akappa - 2(es. a3)
;s(0) = s0 e s(1) = s1 (sono costanti, anche perchè la formula non worka per numeri minori di 2, visto che deve sottrarre 2)
;s0=/s1 = s1/s2 = s1/s0/2 (s0/2 = s(2))
;s1*s0 = 10000cm^2 = 1 mq^2

(define s
   (lambda(k)
     (if (>= k 2)
       (* 1/2 (s (- k 2)))
       (if(= k 0)
          s0
          s1
          )
      )
     )
  )
;cond = una specie di switch, che al posto di avere casi specifici ha delle preposizioni [cond(((k>2) "a") ((k<2) "b"))]
;cond([preposizione booleana]
;[cose fa eseguire]
;[preposizione booleana]
;[cose fa eseguire]
;else
;[cose da eseguire]
;)
(define sCond
   (lambda(k)
     (cond ((>= k 2)
           (* 1/2 (sCond (- k 2))))
           ((= k 0)
          s0)
           (else  ;caso default
          s1)
          )
       )
      )

(define s0 (* (expt 2 1/4) 100));trovata con formula mateatica su carta
(define s1 (* (expt 2 -1/4) 100));come sopra