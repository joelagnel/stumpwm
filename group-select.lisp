(in-package :stumpwm)
(export '(cube-test))

(defparameter *rects* (make-array 1 :fill-pointer 0 :adjustable t))

(defstruct cube
   state
   number
   window
   gcontext-normal
   gcontext-toggled)

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

(defun create-cube (x &optional (num 0))
  (let* ((screen (first (xlib:display-roots *display*)))
	 (font (screen-font (current-screen)))
	 (parent (xlib:screen-root screen)) ;(mode-line-window mode-line))
	 (win (xlib:create-window
	       :parent parent
	       ;; (mode-line-window (current-mode-line))
	       :x x
	       :y 0
	       :width 13
	       :height 13
	       :background (screen-bg-color (current-screen))
	       :border (alloc-color (current-screen) "Blue")
	       :border-width 1
	       :event-mask (xlib:make-event-mask :exposure)))
         (gcontext-normal (xlib:create-gcontext :drawable win
						:font font
						:foreground (screen-fg-color (current-screen))
						:background (screen-bg-color (current-screen))))
         (gcontext-toggled (xlib:create-gcontext :drawable win
						 :font font
						 :foreground (screen-bg-color (current-screen))
						 :background (screen-fg-color (current-screen))))
	 (cube (make-cube :state :normal
			  :number num
			  :window win
			  :gcontext-normal gcontext-normal
			  :gcontext-toggled gcontext-normal)))
    cube))

(defun draw-cube (cube)
  (let* ((win (cube-window cube))
	 (gc  (cube-gcontext-normal cube))
	 (font (xlib:gcontext-font gc))
	 (string (write-to-string (cube-number cube))))
    ;; sync window background with gc background
    (setf (xlib:window-background win) (xlib:gcontext-background gc))
    (xlib:map-window win)
    ;; draw font
    (xlib:clear-area win)
    (xlib:draw-image-glyphs  win gc 4
			     (+ (xlib:font-ascent font) 1)
			     string
			     :translate #'translate-id 
			     :size 16))
  (xlib:display-finish-output *display*)
  t)

(defun cube-test ()
  (let* (cube (create-cube 10))
    (draw-cube cube)))

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
