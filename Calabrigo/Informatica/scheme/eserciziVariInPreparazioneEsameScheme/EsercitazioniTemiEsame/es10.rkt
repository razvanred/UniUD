;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es10) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "drawings.ss" "installed-teachpacks")))))
(define sorted-char-list;lista dei caratteri che compaiono nella stringa, ripetuti una sola volta
 (lambda (str);stringa
    (sorted-char-list-rec str "")
   ))

(define isCharDifferent?;
  (lambda (str char);lis 
    (if (= (string-length str) 0)
        0
        (if (char=? (string-ref str 0) char)
            (+ (isCharDifferent? (substring str 1) char) 1)
            (isCharDifferent? (substring str 1) char))
        )
    ))

(orderStringChar (substring str 1) cha)
(define orderStringChar
  (lambda (str cha)
    
    (if (< (char->integer cha)(char->integer (string-ref str 0)))
        (string-append (string cha) 
        

(define orderStringChar-rec
  (lambda (str char pos)
    ((if (< (char->integer cha)(char->integer (string-ref str 0)))
        
        

(define sorted-char-list-rec
  (lambda (str TrueStr);str -> stringa passata , TrueStr -> stringa di caratteri comuni non ripetuti
    (if (= (string-length str) 0)
        TrueStr
        (let ((charDifferent (if (= (isCharDifferent? TrueStr (string-ref str 0)) 0) #t #f) ))
          (if charDifferent
              (sorted-char-list-rec (substring str 1) (string-append (string (string-ref str 0)) TrueStr))
              (sorted-char-list-rec (substring str 1) TrueStr))
          )
        )
    ))

(sorted-char-list "una frase ciao")