;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1.8|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
(define hanoi-disks
  (lambda (n k)
    (hanoi-disks-rec n k 1 2 3 0 0 0)
    ))

(define hanoi-disks-rec
  (lambda (n k s d t a b c)
    (if (= n 0)
        (list (list s a) (list d b) (list t c))
        (let ((nn (- n 1)))
          (if (< k (expt 2 nn))
              (hanoi-disks-rec nn k s d t (+ a 1) b c)
              (hanoi-disks-rec nn k t d s c (+ b 1) a))))
    ))

(hanoi-disks 3 0)
(hanoi-disks 3 1)
(hanoi-disks 3 2)
(hanoi-disks 3 3)
(hanoi-disks 3 4)
(hanoi-disks 3 5) 
(hanoi-disks 3 6)
(hanoi-disks 3 7)
(hanoi-disks 5 13)
(hanoi-disks 15 19705)
(hanoi-disks 15 32767)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hanoi-picture
  (lambda (n k)
    (hanoi-disks-rec n k 1 2 3 0 0 0)
    ))

(define hanoi-picture-rec
  (lambda (n k s d t a b c)
    (if (= n 0)
        (list (list s (+ n a)) (list d b) (list t c))
        (let ((nn (- n 1)))
          (if (< k (expt 2 nn))
              (hanoi-picture-rec nn k s t d (+ a 1) c b)
              (hanoi-picture-rec nn k t d s c (+ b 1) a))))
    ))

(hanoi-picture 3 0)
(hanoi-picture 3 1)
(hanoi-picture 3 2)
(hanoi-picture 3 3)
(hanoi-picture 3 4)
(hanoi-picture 3 5) 
(hanoi-picture 3 6)
(hanoi-picture 3 7)
(hanoi-picture 5 13)
(hanoi-picture 15 19705)
(hanoi-picture 15 32767)