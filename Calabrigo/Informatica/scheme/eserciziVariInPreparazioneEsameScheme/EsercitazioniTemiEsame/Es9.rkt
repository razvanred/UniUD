;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Es9) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define closest-pair;restituisce una lista contentente i 2 numeri la cui differenza è minima
 (lambda (lis);lista di numeri
   (reverse (closest-pair-rec lis (list (car lis) (car (cdr lis)))))
   ))


(define closest-pair-rec
  (lambda (lis lisF)
    (if (<= (length lis) 2)
        lisF
        (if (<
             (- (car (cdr lis)) (car lis))
             (- (car (cdr lisF)) (car lisF))
             )
            
            (closest-pair-rec (cdr lis) (list (car (cdr lis)) (car lis)))
            (closest-pair-rec (cdr lis) (list (car (cdr lisF)) (car lisF))))
        )
    ))

(closest-pair (list 2 4 5 6 8))