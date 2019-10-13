#lang racket
(define comp-a-1
  (lambda (string)
    (if (> (string-length string) 1)
        (string-append
         (compl (substring string 0 1))
         (comp-a-1 (substring string 1))
         )
        (compl string)
        )
    )
  )

(define comp-a-1-alt
  (lambda (string)
    (if (> (string-length string) 0)
        (string-append
         (compl (substring string 0 1))
         (comp-a-1 (substring string 1))
         )
        ""
        )
    )
  )


(define compl
  (lambda (bit)
    (if (string=? bit "1")
        "0"
        "1"
        )
    )
  )