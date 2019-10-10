;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname principale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Razvan Rosu
; 01/05/2019

; definire un metodo in grado di generare una frase in base a 3 argomenti: soggetto, predicato verbale e complemento oggetto
; ritorna una stringa
;
; il predicato verbale sarà all'infinito
; -are: -a/-ano, -ere: -e/-ono, -ire: -e/-ono
;
; il sostantivo sarò preceduto da un articolo determinativo
;
; se il soggetto è maschile e singolare, finirà con la lettera o, se plurale con i
; i rispettivi articoli saranno il e i
;
; se il soggetto è femminile e singolare terminerà con a, se plurale con e
; i rispettivi articoli saranno la e le

; metodo che ritorna un articolo determinativo maschile
(define articolo-maschile ; string
  (lambda (is-singolare) ; boolean
    (if is-singolare "il" "i")
    ))

; metodo che ritorna un articolo determinativo femminile
(define articolo-femminile ; string
  (lambda (is-singolare) ; boolean
    (if is-singolare "la" "le")
    ))

; metodo che permette di identificare se il soggetto è singolare o plurale
; NOTA: ho preferito usare il cond per escludere qualsiasi altro caso non descritto nel problema (il programma lancierà un'eccezione nel caso l'utente inserisse un caso non previsto)
(define singolare? ; boolean
  (lambda (soggetto) ; string
    (let ([last-char (string-ref soggetto (- (string-length soggetto) 1))])
      (cond
        ((or (char=? last-char #\a) (char=? last-char #\o)) #t)
        ((or (char=? last-char #\i) (char=? last-char #\e)) #f)
        )  
      )
    ))

; meotodo che ritorna il predicato verbale coniugato
(define coniuga ; string
  (lambda (pred-verb is-singular) ; string, boolean
    (let ([str-length (string-length pred-verb)])
      (let ([con (substring pred-verb (- str-length 3) str-length)]
            [suffix (substring pred-verb 0 (- str-length 3))])       
        (string-append suffix (cond
          ((string=? "are" con)
           (if is-singular "a" "ano"))
          ((string=? "ere" con)
           (if is-singular "e" "ono"))
          ((string=? "ire" con)
           (if is-singular "e" "ono"))
          ))
        )
      )
    ))

; metodo che ritorna un articolo in base al genere del soggetto
(define articolo-determinativo ; string
  (lambda (soggetto is-singular) ; string, boolean
    (let ([last-char (string-ref soggetto (- (string-length soggetto) 1))])
      (cond
        ((or (char=? last-char #\o) (char=? last-char #\i)) (articolo-maschile is-singular))
        ((or (char=? last-char #\a) (char=? last-char #\e)) (articolo-femminile is-singular))
        )
      )
    ))
             

(define frase ; string
  (lambda (soggetto pred-verb compl-ogg) ; string, string, string
    (let ([is-singular-soggetto (singolare? soggetto)])
      (string-append
       (articolo-determinativo soggetto is-singular-soggetto)
       " "
       soggetto
       " "
       (coniuga pred-verb is-singular-soggetto)
       " "
       (articolo-determinativo compl-ogg (singolare? compl-ogg))
       " "
       compl-ogg)
      )))



; testing

(string=? "il gatto caccia i topi"
          (frase "gatto" "cacciare" "topi"))

(string=? "la mucca mangia il fieno"
          (frase "mucca" "mangiare" "fieno"))

(string=? "le sorelle leggono la novella"
          (frase "sorelle" "leggere" "novella"))

(string=? "i bambini amano le favole"
          (frase "bambini" "amare" "favole"))

(string=? "i musicisti suonano i pianoforti"
          (frase "musicisti" "suonare" "pianoforti"))

(string=? "il cuoco frigge le patate"
          (frase "cuoco" "friggere" "patate"))

(string=? "i camerieri servono i clienti"
          (frase  "camerieri" "servire" "clienti"))

(string=? "la mamma chiama le figlie"
          (frase "mamma" "chiamare" "figlie"))