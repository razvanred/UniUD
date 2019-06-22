;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname conversioneternariabilanciata-decimale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;data una stringa ternaria bilanciata, con + = 1, . = 0, e - = -1 trasformala in una cifra decimale

(define TerToDec
  (lambda (ter)                                        
     (let(( terC (if (>(string-length ter)0)(substring ter (-(string-length ter)1) (string-length ter))"")))
      (let ((terN (cond
                    ((string=? terC "+") 1)
                    ((string=? terC "-") -1)
                    ((string=? terC ".") 0)
                    )))
        (if (> (string-length ter) 1) 
        (+ terN (*(TerToDec (substring ter 0 (-(string-length ter)1)))3) ) ;if
        terN              ;else
        )
      )
    )                                                
    )
  )