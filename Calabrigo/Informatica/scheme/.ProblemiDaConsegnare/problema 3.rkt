;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problema 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;input: numero binario output: numero decimale
;il numero binario può essere negativo o con la virgola, in questi format: 101010 101010.1010 -010101010 -10101001.1010

;risoluzione:
;verifica il primo simbolo, se è = "-" allora il num è negativo, altrimenti è positivo
(define isNegative?
  (lambda(bin)
    (if (char=? (string-ref bin 0) #\-)
        "-"
        "+"
     )
    )
  )
;traforma carattere binario in numero
(define binToInt
  (lambda(c)
    (if (string=? c "1")
        1
        0
     )
    )
  )
;funzione che calcola il numero decimale
(define intero
  (lambda(bin)
    (if (> (string-length bin) 0)
    (+
    (* (binToInt (substring bin 0 1))
       (expt 2 (-(string-length bin)1)))
    (intero (substring bin 1 (string-length bin)))
    )
    0
    )
  )
)
;funzione che calcola il numero dopo il punto
(define decimale
  (lambda(bin)
    (if (> (string-length bin) 0)
    (+
    (* (binToInt (substring bin (-(string-length bin)1) (string-length bin)))
       (expt 2 (*(string-length bin)-1)))
    (decimale (substring bin 0 (-(string-length bin)1)))
    )
    0
    )
  )
)
   

;funzione che, presa in input la stringa binaria, ritorna la posizione (partendo da 0) in cui si trova il ".", se il "." non è presente, allora la funzione ritorna -1
(define findPoint
  (lambda(bin)
    (if (> (string-length bin)0)
        (if (not(char=? (string-ref bin 0) #\.))
            (+ (findPoint (substring bin 1 (string-length bin))) 1)
            0
        )
        (* (string-length bin) -1)
    )
   )
)

;funzione che calcola il numero in base 10, dato un numero in base 2 che può essere positivo, negativo o decimale
(define converterBinToDec
  (lambda(bin)
    (let (( pointPos (findPoint bin) ))
    (string-append
    (isNegative? bin)
    (number->string (intero (substring bin 0  pointPos)))
    (if (< (+ pointPos 1) (string-length bin))
        "."
        ""
    )
    (if (< (+ pointPos 1) (string-length bin))
        (number->string (decimale (substring bin (+ pointPos 1) (string-length bin))))
        ""
    )
    )
    ) 
  )
)
(converterBinToDec "11101.101")
    
      