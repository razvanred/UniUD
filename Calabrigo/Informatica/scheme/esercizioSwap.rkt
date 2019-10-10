;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname esercizioSwap) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define swap-rec
  (lambda (letters pos)
    (if (< pos (length letters))
        (append (if (= pos 0)
                    (append (list (car (cdr letters)))
                            (list (car letters))
                            (cdr (cdr letters)))
                    null)
                (swap (cdr letters) (- pos 1)))
        pos;else
        )
    ))

(define swap
  (lambda (letters pos)
    (cond ((< (length letters) 2) letters)
          ((= pos 0) (cons (cadr letters) (cons (car letters) (cddr letters))))
          (else (cons (car letters) (swap (cdr letters) (- pos 1))))
          )))
(swap (list 1 2 3) 1)