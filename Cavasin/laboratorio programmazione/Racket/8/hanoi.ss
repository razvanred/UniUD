
;; Arranged by Claudio Mirolo (5th December 2017)
;; in order to provide visualization tools for Hanoi towers.


(module drawings mzscheme
  (provide 
           disk-image
           towers-background
           above
           )
  
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  
  (define default-image-size 120)
  
  (define get-default-image-size
    (lambda ()
      default-image-size))
  
  (define set-default-image-size!
    (lambda (new)
      (set! default-image-size new)))
  
  (define overlay
    (lambda (top bottom)
      (let* ((top-bm (send top get-bitmap))
             (bottom-bm (send bottom get-bitmap))
             (top-w (send top-bm get-width))
             (top-h (send top-bm get-height))
             (w (if (= (send bottom-bm get-width) top-w)
                    top-w
                    (error "can't overlay images of different widths" top bottom)))
             (bottom-h (send bottom-bm get-height))
             (h (if (= top-h bottom-h)
                    top-h
                    (error "can't overlay images of different heights" top bottom)))
             (bm (make-object bitmap% w h (not (or (send top-bm is-color?)
                                                   (send bottom-bm is-color?))) #t))
             (dc (new bitmap-dc% (bitmap bm))))
        (send dc erase)
        (send dc draw-bitmap bottom-bm 0 0)
        (send dc draw-bitmap top-bm 0 0)
        (send dc set-bitmap #f)
        (make-object image-snip% bm))))

  (define image-width
    (lambda (img)
      (let* ( (bm (send img get-bitmap)) )
        (send bm get-width)
        )))

  (define image-height
    (lambda (img)
      (let* ( (bm (send img get-bitmap)) )
        (send bm get-height)
        )))


;; Translating & rotating images
  
  (define default-shift-step 30)
  
  
  (define shift-down
    (lambda (src steps)
      (let* ((src-bm (send src get-bitmap))
             (src-w (send src-bm get-width))
             (src-h (send src-bm get-height))
             (delta (* steps default-shift-step))
             (w src-w)
             (h (+ src-h delta))
             (bm (make-bitmap w h))
             (dc (new bitmap-dc% (bitmap bm)))
             )
        (send dc erase)
        (send dc draw-bitmap src-bm 0 delta)
        (send dc set-bitmap #f)
        (make-object image-snip% bm)
        )))
  
  (define shift-right
    (lambda (src steps)
      (let* ((src-bm (send src get-bitmap))
             (src-w (send src-bm get-width))
             (src-h (send src-bm get-height))
             (delta (* steps default-shift-step))
             (w (+ src-w delta))
             (h src-h)
             (bm (make-bitmap w h))
             (dc (new bitmap-dc% (bitmap bm)))
             )
        (send dc erase)
        (send dc draw-bitmap src-bm delta 0)
        (send dc set-bitmap #f)
        (make-object image-snip% bm)
        )))

  
;; Overlapping images
  
  (define overlap-images
    (lambda (top bottom)
      (let* ((top-bm (send top get-bitmap))
             (bottom-bm (send bottom get-bitmap))
             (top-w (send top-bm get-width))
             (top-h (send top-bm get-height))
             (bottom-w (send bottom-bm get-width))
             (bottom-h (send bottom-bm get-height))
             (w (max top-w bottom-w))
             (h (max top-h bottom-h))
             (bm (make-bitmap w h))
             (dc (new bitmap-dc% (bitmap bm)))
             )
        (send dc erase)
        (send dc draw-bitmap bottom-bm 0 0)
        (send dc draw-bitmap top-bm 0 0)
        (send dc set-bitmap #f)
        (make-object image-snip% bm)
        )))
  
  
  (define glue-tiles overlap-images)


;; Puzzle
  
  (define set-puzzle-shift-step!
    (lambda ()
      (set! default-shift-step (- (image-height smaller-tile) 1))
      ))
  
  
  (define larger-tile
    (let* ((w (quotient (* 4 default-image-size) 5))
           (h default-image-size)
           (bm (make-bitmap (+ w 1) (+ h 1)))
           (dc (new bitmap-dc% (bitmap bm)))
           )
      (send dc erase)
      (send dc set-pen "DarkRed" 1 'solid)
      (send dc set-brush "yellow" 'solid)
      (send dc draw-polygon
            (list
             (cons (quotient w 2) h)
             (cons w (quotient h 5))
             (cons (quotient w 2) 0)
             (cons (quotient w 2) (quotient (* 2 h) 5))
             (cons 0 (quotient (* 2 h) 5))
             (cons 0 (quotient (* 4 h) 5))
             (cons (quotient w 2) (quotient (* 4 h) 5))
             ))
      (send dc set-bitmap #f)
      (make-object image-snip% bm)
      ))
  
  (define smaller-tile
    (let* ((w (quotient (* 2 default-image-size) 5))
           (h (quotient default-image-size 5))
           (bm (make-bitmap (+ w 1) (+ h 1)))
           (dc (new bitmap-dc% (bitmap bm)))
           )
      (send dc erase)
      (send dc set-pen "DarkRed" 1 'solid)
      (send dc set-brush "yellow" 'solid)
      (send dc draw-polygon
            (list
             (cons 0 0)
             (cons w h)
             (cons w 0)
             ))
      (send dc set-bitmap #f)
      (make-object image-snip% bm)
      ))
  
  
;; Hanoi
  
  (define disk-image
    (lambda (s n p t)
      (let* ((w default-image-size)
             (h (quotient w 2))
             (dx (* w .4 (/ (- n s) n)))
             (dy (* dx .375))
             (dw (* w (- p 1)))
             (dh (quotient (* h (- n (+ t 1))) 4))
             (bm (make-bitmap (+ w dw 20) (+ h dh 20)))
             (dc (new bitmap-dc% (bitmap bm)))
             )
        (send dc erase)
        (send dc set-pen "Goldenrod" 1 'solid)
        (send dc set-brush "Goldenrod" 'solid)
        (send dc draw-ellipse (+ dw dx 10) (+ dh (* h .25) dy 10) (- w (* 2 dx)) (- (* h .75) (* 2 dy)))
        (send dc draw-rectangle (+ dw dx 10) (+ dh (* h .375) 10) (- w (* 2 dx)) (* h .25))
        (send dc set-pen "Gold" 1 'solid)
        (send dc set-brush "Gold" 'solid)
        (send dc draw-ellipse (+ dw dx 10) (+ dh dy 10) (- w (* 2 dx)) (- (* h .75) (* 2 dy)))
        (send dc set-pen "DarkBlue" 1 'solid)
        (send dc set-brush "DarkBlue" 'solid)
        (send dc draw-rounded-rectangle (+ (/ w 2) dw 6) 10 7 (+ dh (* h .375)) 3)
        (send dc set-bitmap #f)
        (make-object image-snip% bm)
        )))


  (define towers-background
    (lambda (n)
      (let* ((w default-image-size)
             (h (quotient w 2))
             (dw (* w 2))
             (dh (quotient (* h (- n 1)) 4))
             (bm (make-bitmap (+ w dw 20) (+ h dh 20)))
             (dc (new bitmap-dc% (bitmap bm)))
             )
        (send dc erase)
        (send dc set-pen "RoyalBlue" 1 'solid)
        (send dc set-brush "RoyalBlue" 'solid)
        (send dc draw-rounded-rectangle 0 0 (+ w dw 20) (+ h dh 20) 20)
        (send dc set-pen "DarkBlue" 1 'solid)
        (send dc set-brush "DarkBlue" 'solid)
        (send dc draw-rounded-rectangle (+ (/ w 2) 6) 10 7 (+ (* h .625) dh) 3)
        (send dc draw-rounded-rectangle (+ (/ w 2) w 6) 10 7 (+ (* h .625) dh) 3)
        (send dc draw-rounded-rectangle (+ (/ w 2) dw 6) 10 7 (+ (* h .625) dh) 3)
        (send dc set-bitmap #f)
        (make-object image-snip% bm)
        )))


  (define above overlap-images)

)

