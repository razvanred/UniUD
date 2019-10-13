;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1.5|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define manhattan-3d     ; val: intero (numero percorsi)
  (lambda (i j k)        ; i,j: interi (distanze dal punto, quindi dimensioe del reticolo)
    (cond ((or (and (= i 0) (= j 0))      ;se sono due a 0 allora c'è un solo percorso
               (and (= j 0) (= k 0))
               (and (= k 0) (= i 0)))
           1)
          ((= i 0)
           (+
            (manhattan-3d i (- j 1) k)    ;se uno solo è a zero si riduce al problema precedente
            (manhattan-3d i j (- k 1))))
          ((= j 0)
           (+
            (manhattan-3d (- i 1) j k)    ; """
            (manhattan-3d i j (- k 1))))
          ((= k 0)
           (+
            (manhattan-3d (- i 1) j k)    ; """
            (manhattan-3d i (- j 1) k)))
          (else
           (+
            (manhattan-3d (- i 1) j k)    ;se nessuno è a 0 allora sommo tutti i percorsi con una dimensione diminuita
            (manhattan-3d i (- j 1) k)
            (manhattan-3d i j (- k 1)))))
    ))

(manhattan-3d 0 0 7)
(manhattan-3d 2 0 2)
(manhattan-3d 1 1 1)
(manhattan-3d 1 1 5)
(manhattan-3d 2 3 1)
(manhattan-3d 2 3 3)