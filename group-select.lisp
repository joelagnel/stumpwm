(in-package :stumpwm)
(export '(make-rect-window))

(defparameter *rects* (make-array 1 :fill-pointer 0 :adjustable t))

(defun make-rects ()
  (dotimes (n 10)
    (vector-push-extend (make-rect-window (- (* (+ n 1) 14) 2) n)
			*rects*)))

(defun destroy-rects ()
  (map 'vector (lambda (win)
		 (xlib:destroy-window win)) 
       *rects*)
  (setf *rects* (make-array 1 :fill-pointer 0 :adjustable t))
  (xlib:display-finish-output *display*))

;; (defun make-rect-appear-focused (num)
;;   (let* (win (aref *rects num))
;;     (setf (xlib:window-background win)

(defun make-rect-window (x &optional (num 0))
  (let* ((screen (first (xlib:display-roots *display*)))
	 (font (screen-font (current-screen)))
	 (string (write-to-string num))
	 (height (font-height font))
	 (width (xlib:text-width font string))
	 (parent (xlib:screen-root screen)) ;(mode-line-window mode-line))
	 (rect (xlib:create-window
		:parent parent
		;; (mode-line-window (current-mode-line))
		:x x
		:y 0
		:width 13
		:height 13
		:background (xlib:screen-black-pixel screen)
		:border (alloc-color (current-screen) "Green")
		:border-width 1
		:event-mask (xlib:make-event-mask :exposure)))
         (gcontext (xlib:create-gcontext :drawable rect
                                         :font font
                                         :foreground (screen-bg-color (current-screen))
                                         :background (screen-fg-color (current-screen)))))
    (xlib:map-window rect)
    (draw-font-with-gcon rect font gcontext string)
    (xlib:display-finish-output *display*)
    rect))

 (defun draw-font-with-gcon (win font gcontext string)
    (xlib:clear-area win)
    (xlib:draw-image-glyphs  win gcontext 4
			     (+ (xlib:font-ascent font) 1)
			     string 
			     :translate #'translate-id 
			     :size 16))

;; (xlib:event-case (display :force-output-p t
;; 			      :discard-p t)
;;   (:exposure (count)
;; 		 (xlib:draw-rectangle
;; 		  window
;; 		  (get (rect-c rect) 'grackon)
;; 		  (rect-x rect)
;; 		  (rect-y rect)
;; 		  (rect-w rect)
;; 		  (rect-h rect)
;; 		  'fill)))
;; nil))))
	

;; (defun show-rectangles (window width height)
;;   (let* ((display stumpwm:*display*)
;; 	 (screen (first (xlib:display-roots display)))
;; 	 (black (xlib:screen-black-pixel screen))
;; 	 (white (xlib:screen-white-pixel screen))
;; 	 (my-window (xlib:create-window
;; 		     :parent window
;; 		     :x 0
;; 		     :y 0
;; 		     :width width
;; 		     :height height
;; 		     :background black
;; 		     :event-mask (xlib:make-event-mask :exposure))))
;;     (dolist (colour-symbol *colour-list*)
;;       (setf (get colour-symbol 'grackon)
;; 	    (xlib:create-gcontext
;; 	     :drawable root-window
;; 	     :foreground (xlib:alloc-color
;; 			  (xlib:window-colormap root-window)
;; 			  (symbol-name colour-symbol))
;; 	     :background black)))
;;     (xlib:map-window my-window)
;;     (xlib:event-case (display :force-output-p t
;; 			      :discard-p t)
;;       (:exposure (count)
;; 		 (when (zerop count)
;; 		   (dolist (rect *rect-list*)
;; 		     (xlib:draw-rectangle
;; 		      window
;; 		      (get (rect-c rect) 'grackon)
;; 		      (rect-x rect)
;; 		      (rect-y rect)
;; 		      (rect-w rect)
;; 		      (rect-h rect)
;; 		      'fill)))
;; 		 nil)
;;       (:button-press () t))
;;     my-window))


;; (defstruct rect x y w h c)

;; (defun random-choice (item-list)
;;   (let ((options (length item-list)))
;;     (elt item-list (random options))))

;; (defun cons-up-rect-list (n)
;;   (cons-up n #'(lambda()
;; 		 (make-rect :x (random 300)
;; 			    :y (random 300)
;; 			    :w (+ 20 (random 30))
;; 			    :h (+ 20 (random 30))
;; 			    :c (random-choice *colour-list*)))))

;; (defun cons-up (count constructor)
;;   (let (accumulator)
;;     (dotimes (index count accumulator)
;;       (push (funcall constructor) accumulator))))


;; (defparameter *default-rect-list*
;;   (cons-up-rect-list 10))

;; (defvar *rect-list* *default-rect-list*)
;; ; The program can be loaded and run, and will display
;; ; some rectangles. If you have already created some 
;; ; rectangles, it will not clobber them.
;; ; In particular you can edit the source, reload the 
;; ; the file, and you still have your own rectangle list.
;; ; At any time, you can get back to the default with
;; ; (setf *rect-list* *default-rect-list*)
    ;; (xlib:create-gcontext :drawable win
    ;; 					 :font font
    ;; 					 :foreground fg
    ;; 					 :background bg)))

;; (xlib:draw-glyphs *rect* grackon 5 5 "hello"
;; 		      :translate #'translate-id
;; 		      :size 16)

;; (grackon (xlib:create-gcontext
;; 	   :drawable *rect*
;; 	   :foreground black
;; 	   :background white)))
