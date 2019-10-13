;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Es5) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define av; lista di numeri [-1,1]
  (lambda (num);num-> lista di numeri [-1,1]
    (if (= (length num) 1)
           (list )
           (let ((sumX (+ (car num) (car (cdr num))) ))
             (cons 
              (cond ((< sumX 0)
                     -1)
                    ((= sumX 0)
                     0)
                    (else
                     1))
              (av (cdr num)))
             ))
    ))

(av (list 0 0 -1 -1 1 0 0 1 0)); → (0 -1 -1 0 1 0 1 1)