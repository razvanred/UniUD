;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname principale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Razvan Rosu
; Manhattan 3D

(define manhattan-3d ; int
  (lambda (i j k) ; int, int, int
    (cond
      [(= i 0)
       (if (or (= j 0) (= k 0))
           1
           (+ (manhattan-3d i (sub1 j) k) (manhattan-3d i j (sub1 k))))]
      [(= j 0)
       (if (or (= i 0)  (= k 0))
           1
           (+ (manhattan-3d (sub1 i) j k) (manhattan-3d i j (sub1 k))))]
      [(= k 0)
       (if (or (= i 0) (= j 0))
           1
           (+ (manhattan-3d (sub1 i) j k) (manhattan-3d i (sub1 j) k)))]
      [else
       (+ (manhattan-3d (sub1 i) j k) (manhattan-3d i (sub1 j) k) (manhattan-3d i j (sub1 k)))
       ]
      )
    ))

; testing
(= (manhattan-3d 0 0 7) 1)
(= (manhattan-3d 2 0 2) 6)
(= (manhattan-3d 1 1 1) 6)
(= (manhattan-3d 1 1 5) 42)
(= (manhattan-3d 2 3 1) 60)
(= (manhattan-3d 2 3 3) 560)