
;; Questo file contiene esempi di codice Scheme:
;; Procedure ricorsive che operano su stringhe
;; Ultimo aggiornamento: 4/11/16


;; Rieleaborazione in lettere maiuscole di una parola:

(define word-uppercase  ; val: stringa
  (lambda (word)        ; word: stringa
    (if (string=? word "")
        ""
        (string-append
         (string (lowercase->uppercase (string-ref word 0)))
         (word-uppercase (substring word 1)))
        )))

(define lowercase->uppercase  ; val: carattere
  (lambda (c)                 ; c: carattere
    (if (lowercase? c)
        (integer->char (- (char->integer c) displ))
        c
        )))

(define lowercase?  ; val: booleano
  (lambda (c)       ; c: carattere
    (and (char<=? #\a c) (char<=? c #\z))
    ))

(define displ (- (char->integer #\a) (char->integer #\A)))


;; Complemento a uno:

(define bit-complement   ; val: stringa
  (lambda (bit)          ; bit: stringa
    (if (string=? bit "0")
        "1"
        "0"
        )))

(define ones-complement  ; val: stringa di 0/1
  (lambda (bin)          ; bin: stringa di 0/1
    (if (string=? bin "")
        ""
        (string-append
         (ones-complement (substring bin 0 (- (string-length bin) 1)))
         (bit-complement (substring bin (- (string-length bin) 1)))
         ))
    ))


;; Complemento a due e riutilizzo del codice (ones-complement):

   (define twos-complement  ; val: stringa di 0/1
     (lambda (bin)          ; bin: stringa di 0/1
       (let ((k (- (string-length bin) 1))
             )
         (cond ((= k 0)
                bin)
               ((char=? (string-ref bin k) #\0)
                (string-append (twos-complement (substring bin 0 k)) "0"))
               (else  ; #\1
                (string-append (ones-complement (substring bin 0 k)) "1"))
               ))
       ))


;; Rappresentazione binaria successiva:

(define bin-succ
  (lambda (bin)
    (let ((k (- (string-length bin) 1))
          )
      (if (= k 0)
          (if (string=? bin "0")
              "1"                                 ; bin = "0"
              "10"                                ; bin = "1"
              )
          (let ((lsb (substring bin k))
                (pre (substring bin 0 k))
                )
            (if (string=? lsb "0")
              (string-append pre "1")             ; lsb = "0"
              (string-append (bin-succ pre) "0")  ; lsb = "1"
              ))
          ))
    ))


;; Controllo di parita':
;; Verifica se il numero di occorrenze del simbolo '1'
;; in una sequenza binaria e' pari;
;; Si applica per rilevare errori di trasmissione.

;; Algoritmo non deterministico:

(define parity-check?  ; valore: booleano
  (lambda (seq)        ; seq: stringa non vuota di 0/1
    (let ((n (string-length seq))
          )
      (if (= n 1)
          (string=? seq "0")
          (let ((k (+ (random (- n 1)) 1))
                )
            (equal? (parity-check? (substring seq 0 k))
                    (parity-check? (substring seq k)))
            )
          ))
    ))


;; Calcolo delle occorrenze di una parola in un testo
;; ("Find all"):

(define how-many       ; valore: intero
  (lambda (word text)  ; word, text: stringhe
    (let ((n (string-length word))
          )
      (cond ((> n (string-length text))
             0
             )
            ((string=? (substring text 0 n) word)
             (+ (how-many word (substring text n)) 1)
             )
            (else
             (how-many word (substring text 1))
             ))
      )))


;; Rimpiazzamento delle occorrenze di una parola (word)
;; con un'altra (subs) in un testo ("Replace all"):

(define replacement         ; valore: stringa
  (lambda (word subs text)  ; word, subs, text: stringhe
    (let ((n (string-length word))
          )
      (cond ((> n (string-length text))
             text
             )
            ((string=? (substring text 0 n) word)
             (string-append subs (replacement word subs (substring text n)))
             )
            (else
             (string-append (substring text 0 1) (replacement word subs (substring text 1)))
             ))
      )))

