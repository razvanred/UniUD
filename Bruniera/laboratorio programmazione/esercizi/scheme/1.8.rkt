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
              (hanoi-disks-rec nn k s t d (+ a 1) c b)
              (hanoi-disks-rec nn (- k (expt 2 nn)) t d s c (+ b 1) a))))
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
    (hanoi-picture-rec n k 1 2 3 0 0 0 (towers-background n))
    ))

(define hanoi-picture-rec
  (lambda (n k s d t a b c i)
    (if (= n 0)
        i
        (let ((nn (- n 1))
              (all (+ a (+ b (+ c n)))))
          (if (< k (expt 2 nn))
              (hanoi-picture-rec nn k s t d (+ a 1) c b
                                 (above (disk-image n all s a) i))
              (hanoi-picture-rec nn (- k (expt 2 nn)) t d s c (+ b 1) a
                                 (above (disk-image n all d b) i)))))
    ))

(hanoi-picture 5 0)
(hanoi-picture 5 13)
(hanoi-picture 5 22)
(hanoi-picture 5 31)
(hanoi-picture 15 19705)