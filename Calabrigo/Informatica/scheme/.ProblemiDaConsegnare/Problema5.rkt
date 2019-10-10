;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Problema5) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))

; (mahn3D 0 0 k) -> 1
; (mahn3D i 0 k) -> (+ (mahn3D (- i 1) 0 k) (mahn3D i 0 (- k 1)))
; (mahn3D i j k) -> (+ (mahn3D (- i 1) j k) (mahn3D i (- j 1) k) (mahn3D i j (- k 1)))

(define mahn3D    ; val: intero positivo
  (lambda(i j k)  ; in: i,j,k : interi positivi
    (cond ( (or(and(= i 0)(= j 0)) (and(= i 0)(= k 0)) (and(= j 0)(= k 0)))
           1
           )
          ((= i 0)
           (+ (mahn3D 0 (- j 1) k) (mahn3D 0 j (- k 1)))
           )
          ((= j 0)
           (+ (mahn3D (- i 1) 0 k) (mahn3D i 0 (- k 1)))
           )
          ((= k 0)
           (+ (mahn3D (- i 1) j 0) (mahn3D i (- j 1) 0))
           )
          (else
            (+ (mahn3D (- i 1) j k) (mahn3D i (- j 1) k) (mahn3D i j (- k 1)))
           )
    )
  )
)

(mahn3D 0 0 7)
;(define mahn3DD    ; val: intero positivo
 ; (lambda(i j k)  ; in: i,j,k : interi positivi
   ; (if(or(and(= i 0)(= j 0)) (and(= i 0)(= k 0)) (and(= j 0)(= k 0)))
     ;  1
      ; (if (or(and(= i 0)(= j 0)) (and(= i 0)(= k 0)) (and(= j 0)(= k 0)))
         ;  1
          ; ()
           
    
