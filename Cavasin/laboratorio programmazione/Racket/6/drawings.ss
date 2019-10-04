
;; This file contains a DrScheme library version of the functional graphics
;; system for use with Concrete Abstractions: An Introduction to Computer
;; Science Using Scheme, by Max Hailperin, Barbara Kaiser, and Karl Knight.
;;
;; This version (tested w/DrScheme v370), unlike the previously distributed
;; version, represents images directly as DrScheme's image snips, which has the
;; virtue that "Insert Image ..." can be used to bring in an image file (e.g.
;; a photo) and then use it for things like pinwheeling.
;;
;; At the moment, color images are treated as opaque, whereas monochrome images
;; are treated as transparent (the white regions are transparent, that is).  This
;; is relevant for the overlay operation.  If the first argument to overlay is a
;; color image, it will totally obscure the second argument.  However, if the
;; first argument to overlay is a monochrome image (including those produced with
;; filled-triangle and line, or by operating on monochrome images), then it will
;; overlay nicely onto the second argument, regardless of whether the second is
;; monochrome or color.
;;
;; To get around this limitation of color (or grayscale) images, they can be
;; converted to monochrome using a newly added operation, threshold.  For
;; example, if photo is a color image, then
;;  (threshold photo 200)
;; is a monochrome image formed by translating all pixels where the sum of
;; the red, green, and blue components (each on a scale of 0 to 255) exceeds
;; 200 into white and the rest of the pixels into black.  Choice of a suitable
;; threshold is left to experimentation.
;;
;; The invert operation can be applied even to color images, though the result
;; isn't all that useful.

;; Extended by Claudio Mirolo (6th November 2014) in order to provide tools
;; for a variety of problems:  With this version also color images may have
;; transparent background and overlap properly.


(module fungraph mzscheme
  (provide 
           get-default-image-size
           set-default-image-size!
           white-box                     ; added 5/11/14
           image-width                   ; added 5/11/14
           image-height                  ; added 5/11/14
           line
           quarter-turn-right
           mirror-image
           invert
           threshold
           overlay
           resize-image
           stack
           filled-triangle
           write-image-as-epsf
           shift-down                    ; added 6/11/14
           shift-right                   ; added 6/11/14
           quarter-turn-left             ; added 6/11/14
           half-turn                     ; added 6/11/14
           overlap-images                ; added 6/11/14
           glue-tiles                    ; added 6/11/14
           set-puzzle-shift-step!        ; added 6/11/14
           larger-tile                   ; added 6/11/14
           smaller-tile                  ; added 6/11/14
           set-tessellation-shift-step!  ; added 6/11/14
           L-tile                        ; added 6/11/14
           chessboard-image              ; added 6/11/14
           add-queen-image               ; added 6/11/14
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
  
  (define mirror-image
    (lambda (i)
      (let* ((bm (send i get-bitmap))
             (w (send bm get-width))
             (h (send bm get-height))
             (sz (* w h 4))
             (bs (make-bytes sz))
             (bs2 (make-bytes sz)) ; could reuse bs
             (bm2 (make-object bitmap% w h (not (send bm is-color?)) #t))
             (dc (new bitmap-dc% (bitmap bm2))))
        (send bm get-argb-pixels 0 0 w h bs)
        (let loop1 ((y 0))
          (if (= y h)
              (begin (send dc set-argb-pixels 0 0 w h bs2)
                     (send dc set-bitmap #f)
                     (make-object image-snip% bm2))
              (let loop2 ((x 0))
                (if (= x w)
                    (loop1 (+ y 1))
                    (let ((x2 (- w x 1)))
                      (let loop3 ((c 0))
                        (if (= c 4)
                            (loop2 (+ x 1))
                            (let ((pos (+ c (* 4 (+ x (* y w)))))
                                  (pos2 (+ c (* 4 (+ x2 (* y w))))))
                              (bytes-set! bs2 pos2 (bytes-ref bs pos))
                              (loop3 (+ c 1)))))))))))))
  (define invert
    (lambda (i)
      (let* ((bm (send i get-bitmap))
             (w (send bm get-width))
             (h (send bm get-height))
             (sz (* w h 4))
             (bs (make-bytes sz))
             (bm2 (make-object bitmap% w h (not (send bm is-color?)) #t))
             (dc (new bitmap-dc% (bitmap bm2))))
        (send bm get-argb-pixels 0 0 w h bs)
        (let loop1 ((y 0))
          (if (= y h)
              (begin (send dc set-argb-pixels 0 0 w h bs)
                     (send dc set-bitmap #f)
                     (make-object image-snip% bm2))
              (let loop2 ((x 0))
                (if (= x w)
                    (loop1 (+ y 1))
                    (let loop3 ((c 1))
                      (if (= c 4)
                          (loop2 (+ x 1))
                          (let ((pos (+ c (* 4 (+ x (* y w))))))
                            (bytes-set! bs pos (- 255 (bytes-ref bs pos)))
                            (loop3 (+ c 1))))))))))))
  
  (define threshold
    (lambda (i t)
      (let* ((bm (send i get-bitmap))
             (w (send bm get-width))
             (h (send bm get-height))
             (sz (* w h 4))
             (bs (make-bytes sz))
             (bm2 (make-object bitmap% w h #f #t))
             (dc (new bitmap-dc% (bitmap bm2))))
        (send bm get-argb-pixels 0 0 w h bs)
        (let loop1 ((y 0))
          (if (= y h)
              (begin (send dc set-argb-pixels 0 0 w h bs)
                     (send dc set-bitmap #f)
                     (make-object image-snip% bm2))
              (let loop2 ((x 0))
                (if (= x w)
                    (loop1 (+ y 1))
                    (let* ((p (lambda (c)
                                (+ c (* 4 (+ x (* y w))))))
                           (new (if (> (+ (bytes-ref bs (p 1))
                                          (bytes-ref bs (p 2))
                                          (bytes-ref bs (p 3)))
                                       t)
                                    255
                                    0)))
                      (let loop3 ((c 1))
                        (if (= c 4)
                            (loop2 (+ x 1))
                            (begin
                              (bytes-set! bs (p c) new)
                              (loop3 (+ c 1)))))))))))))
  
  (define quarter-turn-right
    (lambda (i)
      (let* ((bm (send i get-bitmap))
             (w (send bm get-width))
             (h (send bm get-height))
             (sz (* w h 4))
             (bs (make-bytes sz))
             (bs2 (make-bytes sz))
             (bm2 (make-object bitmap% h w (not (send bm is-color?)) #t))
             (dc (new bitmap-dc% (bitmap bm2))))
        (send bm get-argb-pixels 0 0 w h bs)
        (let loop1 ((y 0))
          (if (= y h)
              (begin (send dc set-argb-pixels 0 0 h w bs2)
                     (send dc set-bitmap #f)
                     (make-object image-snip% bm2))
              (let loop2 ((x 0))
                (if (= x w)
                    (loop1 (+ y 1))
                    (let loop3 ((c 0))
                      (if (= c 4)
                          (loop2 (+ x 1))
                          (let ((pos (+ c (* 4 (+ x (* y w)))))
                                (pos2 (+ c (* 4 (+ (- h y 1) (* x h))))))
                            (bytes-set! bs2 pos2 (bytes-ref bs pos))
                            (loop3 (+ c 1))))))))))))
  
  (define stack
    (lambda (top bottom)
      (let* ((top-bm (send top get-bitmap))
             (bottom-bm (send bottom get-bitmap))
             (top-w (send top-bm get-width))
             (top-h (send top-bm get-height))
             (w (if (= (send bottom-bm get-width) top-w)
                    top-w
                    (error "can't stack images of different widths" top bottom)))
             (bottom-h (send bottom-bm get-height))
             (h (+ top-h bottom-h))
             (bm (make-object bitmap% w h (not (or (send top-bm is-color?)
                                                   (send bottom-bm is-color?))) #t))
             (dc (new bitmap-dc% (bitmap bm))))
        (send dc erase)
        (send dc draw-bitmap bottom-bm 0 top-h)
        (send dc draw-bitmap top-bm 0 0)
        (send dc set-bitmap #f)
        (make-object image-snip% bm))))
  
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
  
  (define filled-triangle
    (lambda (x0 y0 x1 y1 x2 y2)
      (let* ((w default-image-size)
             (h default-image-size)
             (pt (lambda (x y)
                   (make-object point%
                     (* (+ x 1) w .5)
                     (* (- 1 y) h .5))))
             (bm (make-object bitmap% w h #f #t))
             (dc (new bitmap-dc% (bitmap bm))))
        (send dc erase)
        (send dc set-pen "yellow" 1 'solid)
        (send dc set-brush "yellow" 'solid)
        (send dc draw-polygon (list (pt x0 y0) (pt x1 y1) (pt x2 y2)))
        (send dc set-bitmap #f)
        (make-object image-snip% bm))))
  
  (define line
    (lambda (x0 y0 x1 y1)
      (let* ((w default-image-size)
             (h default-image-size)
             (tx (lambda (x)
                   (* (+ x 1) w .5)))
             (ty (lambda (y)
                   (* (- 1 y) h .5)))
             (bm (make-object bitmap% w h #f #t))
             (dc (new bitmap-dc% (bitmap bm))))
        (send dc erase)
        (send dc set-pen "blue" 1 'solid)
        (send dc draw-line (tx x0) (ty y0) (tx x1) (ty y1))
        (send dc set-bitmap #f)
        (make-object image-snip% bm))))
  
  (define (resize-image image . wh)
    (let ((width default-image-size)
          (height default-image-size))
      (if (not (null? wh))
          (begin (set! width (car wh))
                 (if (not (null? (cdr wh)))
                     (begin (set! height (cadr wh))
                            (if (not (null? (cddr wh)))
                                (error "too many argument to resize-image")))
                     (set! height width))))
      (let* ((old-bm (send image get-bitmap))
             (old-w (send old-bm get-width))
             (old-h (send old-bm get-height))
             (bm (make-object bitmap% width height (not (send old-bm is-color?)) #t))
             (dc (new bitmap-dc% (bitmap bm))))
        (send dc erase)
        (send dc draw-bitmap-section-smooth old-bm 0 0 width height 0 0 old-w old-h)
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

  (define white-box
    (lambda  wh
      (let ((width default-image-size)
            (height default-image-size)
            )
        (if (not (null? wh))
            (begin (set! width (car wh))
                   (if (not (null? (cdr wh)))
                       (begin (set! height (cadr wh))
                              (if (not (null? (cddr wh)))
                                  (error "too many argument to white-box")))
                       (set! height width))))
        (if (not (and (integer? height)
                      (integer? width)
                      (exact? height)
                      (exact? width)
                      (> height 0)
                      (> width 0)))
            (error "illegal size specification in white-box" wh)
            )
        (let* ((bm (make-object bitmap% width height #f #t))
               (dc (new bitmap-dc% (bitmap bm)))
               )
          (send dc erase)
          (send dc set-bitmap #f)
          (make-object image-snip% bm)
          ))))

  (define write-image-as-epsf
    (let ((margin 72.0))
      (lambda (snip filename)
        (let ((pss (current-ps-setup)))
          (send pss set-file filename)
          (send pss set-mode 'file)
          (current-ps-setup pss))
        (let ((dc (make-object post-script-dc% #f)))
          (if (send dc ok?)
              (begin
                (send dc start-doc filename)
                (send dc start-page)
                (let ((wbox (box 0.0))
                      (hbox (box 0.0)))
                  (send snip get-extent dc margin margin wbox hbox #f #f #f #f)
                  (send snip draw dc margin margin margin margin
                        (+ margin (unbox wbox))
                        (+ margin (unbox hbox))
                        0.0 0.0
                        'no-caret))
                (send dc end-page)
                (send dc end-doc)))))))


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
  
  
  (define quarter-turn-left
    (lambda (img)
      (quarter-turn-right
       (quarter-turn-right
        (quarter-turn-right img)))
      ))

  (define half-turn
    (lambda (img)
      (quarter-turn-right
       (quarter-turn-right img))
      ))
  
  
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


;; Tesselation
  
  (define set-tessellation-shift-step!
    (lambda ()
      (set! default-shift-step (quotient (- (image-width L-tile) 1) 2))
      ))
  
  
  (define L-tile
    (let* ((w (quotient default-image-size 4))
           (bm (make-bitmap (+ w 1) (+ w 1)))
           (dc (new bitmap-dc% (bitmap bm)))
           )
      (send dc erase)
      (send dc set-pen "blue" 1 'solid)
      (send dc set-brush "orange" 'solid)
      (send dc draw-polygon
            (list
             (cons 0 0)
             (cons 0 w)
             (cons (quotient w 2) w)
             (cons (quotient w 2) (quotient w 2))
             (cons w (quotient w 2))
             (cons w 0)
             ))
      (send dc set-bitmap #f)
      (make-object image-snip% bm)
      ))
  
  
;; Chessboard
  
  (define chessboard-image
    (lambda (n)
      (let* ((w default-image-size)
             (h w)
             (bm (make-bitmap (+ w 1) (+ h 1)))
             (dc (new bitmap-dc% (bitmap bm)))
             (s (quotient w n))
             )
        (send dc erase)
        (send dc set-pen "Khaki" 1 'solid)
        (send dc set-brush "Khaki" 'solid)
        (do
            ((i 0 (+ i 1))) ((>= i n))
          (do
              ((j 0 (+ j 1))) ((>= j n))
            (if (even? (+ i j))
                (send dc draw-rectangle (* i s) (* j s) s s)
                )))
        (send dc set-pen "Brown" 1 'solid)
        (send dc draw-line 0 w  w w)
        (send dc draw-line w w  w 0)
        (send dc draw-line w 0  0 0)
        (send dc draw-line 0 0  0 w)
        (send dc set-bitmap #f)
        (make-object image-snip% bm)
        )))
  
  (define add-queen-image
    (lambda (i j n board)
      (let* ((board-bm (send board get-bitmap))
             (w default-image-size)
             (h w)
             (bm (make-bitmap (+ w 1) (+ h 1)))
             (dc (new bitmap-dc% (bitmap bm)))
             (s (quotient w n))
             (dx (* (- j 1) s))
             (dy (* (- i 1) s))
             )
        (send dc erase)
        (send dc draw-bitmap board-bm 0 0)
        (send dc set-pen "MidnightBlue" 1 'solid)
        (send dc set-brush "DarkSlateBlue" 'solid)
        (send dc draw-polygon
              (list
               (cons (+ dx (quotient s 4))  (+ dy (quotient (* 5 s) 6)))
               (cons (+ dx (quotient (* 3 s) 4))  (+ dy (quotient (* 5 s) 6)))
               (cons (+ dx (quotient (* 5 s) 6))  (+ dy (quotient s 2)))
               (cons (+ dx (quotient (* 5 s) 6))  (+ dy (quotient s 6)))
               (cons (+ dx (quotient (* 2 s) 3))  (+ dy (quotient s 2)))
               (cons (+ dx (quotient s 2))  (+ dy (quotient s 6)))
               (cons (+ dx (quotient s 3))  (+ dy (quotient s 2)))
               (cons (+ dx (quotient s 6))  (+ dy (quotient s 6)))
               (cons (+ dx (quotient s 6))  (+ dy (quotient s 2)))
               ))
        (send dc set-bitmap #f)
        (make-object image-snip% bm)
        )))

)

