;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ninarioToAltreBasi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(let (<lista associazioni) <espressione>)  è una specie di define
;esempio: (let ((lsb (if (even? n) "0" "1") ))<uso lsb> )      -->      

(define f
  (lambda(n)
    (let ((lsb (if (even? n) "0" "1")))
      (if(even? n)
         lsb
         (string-append lsb " cose")
       )
     )
   )
 )



;da binario a qualsiasi base
(define binToQualsiasiBase
  (lambda(bin base)
    (if (> (string-length bin) 0)
     (let ((binLen(string-length bin) ))
        (let ((bin-val(if (string=? (substring bin (- binLen 1) binLen) "1") 1 0) ))
              (+
                 (* base bin-val)
                 (binToQualsiasiBase (substring bin 0 (- binLen 1)) base)
              )
        )
     )
     0
   )
  )
)
                    