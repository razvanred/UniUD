#lang racket

(define con-pp-1
  (lambda (v)
    (string-append 
     (substring
      v
      0
      (- (string-length v) 3)
      )
     "ato")
    )
  )

(define con-pp-13
  (lambda (v)
    (string-append 
     (substring
      v
      0
      (- (string-length v) 2)
      )
     "to")
    )
  )

(define con-pp-2
  (lambda (v)
    (string-append 
     (substring
      v
      0
      (- (string-length v) 3)
      )
     "uto")
    )
  )

(define seconda?
  (lambda (v)
    (char=?
     (string-ref v 
                 (- (string-length) 3)
                 )
     #\e)
    )
  )

(define con-pp
  (lambda (v)
    (if (seconda? v)
        (con-pp-2 v)
        (con-pp-13 v)
     )
    )
  )

(define setCharacter
  (lambda (word character position) 
    (string-set! word position character)
    word
    ))

(define stringa (string-append "hello"))