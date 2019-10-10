;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |(1.7 vecchio)|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define prob-3d    ; val: reale
  (lambda (l r b) ; l: intero, r: intero, b: intero
    (/
     (right-path r b (- l (+ r b)))
     (expt 3 l)) ;(all-path l)
    ))

(define all-path  ; val: intero
  (lambda (l)     ; l: intero
    (if (> l 1)
          (* 3
             (all-path (- l 1)))
        3)
    ))

(define right-path  ; val: intero
  (lambda (r b g)       ; r: intero, b: intero, g: intero
    (cond ((or
            (and (= r 0) (= g 0))
            (and (= g 0) (= b 0))
            (and (= b 0) (= r 0))) 1)
          ((= r 0) (+
                    (right-path 0 (- b 1) g)
                    (right-path 0 b (- g 1))))
          ((= b 0) (+
                    (right-path (- r 1) 0 g)
                    (right-path r 0 (- g 1))))
          ((= g 0) (+
                    (right-path (- r 1) b 0)
                    (right-path r (- b 1) 0)))
          (else (+
                 (right-path (- r 1) b g)
                 (right-path r (- b 1) g)
                 (right-path r b (- g 1)))))
    ))

(prob-3d 1 0 1)
(prob-3d 2 1 0) 
(prob-3d 3 1 1)
(prob-3d 4 1 2)
(prob-3d 6 2 2)
(prob-3d 15 5 5)