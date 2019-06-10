;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname principale) (read-case-sensitive #t) (teachpacks ((lib "teachpack.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "teachpack.rkt" "installed-teachpacks")) #f)))
;; Razvan Rosu
;; Esercitazione su codifica Java
;; Rappresentazione del numero intero successivo scritto in notazione BTR

(define btr-succ ; val: stringa di -/./+
  (lambda (btr) ; btr: stringa di -/./+
    (let ((n (string-length btr))) ; (brt = "." oppure inizia con "+")
      (let ((lsb (string-ref btr (- n 1))))
        (if (= n 1)
            (if (char=? lsb #\+)
                "+-"
                "+")
            (let ((pre (substring btr 0 (- n 1))))
              (if (char=? lsb #\+)
                  (string-append (btr-succ pre) "-")
                  (string-append pre (if (char=? lsb #\-) "." "+"))
                  ))
            )))
    ))