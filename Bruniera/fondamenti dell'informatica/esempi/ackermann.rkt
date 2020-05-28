;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ackermann) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss" "installed-teachpacks") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss" "installed-teachpacks") (lib "drawings.ss" "installed-teachpacks")) #f)))
(define ack
  (lambda (x y)
    (cond
      ;; forma classica
      ;((= x 0) (+ y 1))
      ;((= y 0) (ack (- x 1) 1))
      ;(else (ack (- x 1) (ack x (- y 1)))))
      ;; forma che si possa almeno provare a calcolare
      ((= x 0) (+ y 1))
      ((= x 1) (+ y 2))
      ((= x 2) (- (* (+ y 3) 2) 3))
      ((= x 3) (- (expt 2 (+ y 3)) 3))
      ((= y 0) (ack (- x 1) 1))
      (else (ack (- x 1) (ack x (- y 1)))))
    ))

(ack 0 4) ; ((4+3)+1)-3 = 5
(ack 1 4) ; (2+(4+3))-3 = 6
(ack 2 4) ; (2*(4+3))-3 = 11
(ack 3 4) ; (2^(4+3))-3 = 125
(ack 4 1) ; (2^^(1+3))-3 = (2^(2^(2^2)))-3 = 65533
(ack 4 2) ; (2^^(2+3))-3 = (2^(2^(2^(2^2))))-3 = 2*10^19728
;; Oltre non si riesce a calcolare
