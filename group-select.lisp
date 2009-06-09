(in-package :stumpwm)
(export '(create-cube create-cubes destroy-cubes find-cube-window cube-clicked))

(defparameter *cubes* (make-array 1 :fill-pointer 0 :adjustable t))

(defstruct cube
  state
  number
  window
  gcontext-normal
  gcontext-toggled)

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
	       :event-mask (xlib:make-event-mask :exposure :button-press)))
         (gcontext-normal (xlib:create-gcontext :drawable win
						:font font
						:foreground (screen-fg-color (current-screen))
						:background (screen-bg-color (current-screen))))
         (gcontext-toggled (xlib:create-gcontext :drawable win
						 :font font
						 :foreground (screen-bg-color (current-screen))
						 :background (screen-fg-color (current-screen)))))
    (make-cube :state :normal
	       :number num
	       :window win
	       :gcontext-normal gcontext-normal
	       :gcontext-toggled gcontext-toggled)))

(defun toggle-cube (cube)
  (cond ((eq (cube-state cube) :normal)
	 (setf (cube-state cube) :toggled))
	((eq (cube-state cube) :toggled)
	 (setf (cube-state cube) :normal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cube events 			      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; click
(defun cube-clicked (cube)
  (dformat 0 "cube ~a clicked~%" (cube-number cube))
  (toggle-cube cube)
  (draw-cube cube))

;; exposure
(defun draw-cube (cube)
  (let* ((win (cube-window cube))
	 (gc  (or (and (eq (cube-state cube) :toggled) (cube-gcontext-toggled cube))
		  (cube-gcontext-normal cube)))
	 (font (xlib:gcontext-font gc))
	 (string (write-to-string (cube-number cube))))
    ;; sync window background with gc background
    (setf (xlib:window-background win) (xlib:gcontext-background gc))
    (xlib:map-window win)
    ;; draw text
    (xlib:clear-area win)
    (xlib:draw-image-glyphs  win gc 4
			     (+ (xlib:font-ascent font) 1)
			     string
			     :translate #'translate-id 
			     :size 16)
    (xlib:display-finish-output *display*)
    gc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cube management 			      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-cubes ()
  (dotimes (n 10)
    (let ((cube (create-cube (+ (* (+ n 1) 14) 200) n)))
      (vector-push-extend cube *cubes*)
      (draw-cube cube))))

(defun destroy-cubes ()
  (map 'vector (lambda (cube)
		 (xlib:destroy-window (cube-window cube))) 
       *cubes*)
  (setf *cubes* (make-array 1 :fill-pointer 0 :adjustable t))
  (xlib:display-finish-output *display*))

(defun find-cube-window (win)
  (find-if (lambda (cube)
	     (eq (cube-window cube) win))
	   *cubes*))

(defun find-cube-number (num)
  (find-if (lambda (cube)
	     (eq (cube-number cube) num))
	   *cubes*))

;; Delete a cube window and remove it from *cubes*
(defun delete-cube-number (num)
  (setf *cubes* (remove-if (lambda (cube)
			     (if (eq (cube-number cube) num)
				 (progn (xlib:destroy-window (cube-window cube)) t)))
			   *cubes*))
  (xlib:display-finish-output *display*))
