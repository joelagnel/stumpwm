(in-package :stumpwm)
(export '(create-cube create-cubes destroy-cubes find-cube-window cube-clicked))

(defparameter *cubes* '())
(defparameter *cube-border-width* 1)
(defparameter *cube-border-color* "Black")
(defparameter *cube-background* "Gray")
(defparameter *cube-background-toggled* "Orange")
(defparameter *cube-foreground* "Black")
(defparameter *cube-foreground-toggled* "Black")

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
	       :height 12
	       :border (alloc-color (current-screen) *cube-border-color*)
	       :border-width *cube-border-width*
	       :event-mask (xlib:make-event-mask :exposure :button-press)))
	 (fg (alloc-color (current-screen) *cube-foreground*))
	 (bg (alloc-color (current-screen) *cube-background*))
	 (fg-toggled (alloc-color (current-screen) *cube-foreground-toggled*))
	 (bg-toggled (alloc-color (current-screen) *cube-background-toggled*))
	 (gcontext-normal (xlib:create-gcontext :drawable win
						:font font
						:foreground fg
						:background bg))
	 (gcontext-toggled (xlib:create-gcontext :drawable win
						 :font font
						 :foreground fg-toggled
						 :background bg-toggled)))
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

(defun add-cube-number (num)
  ;; (let* ((last-cube (car (last *cubes*)))
  ;; (new-x (or (and last-cube (+ (xlib:drawable-x (cube-window last-cube))
  ;; 			      (xlib:drawable-x (cube-window last-cube)))
  ;; 		 0))
  ;;   (cube (create-cube new-x num)))
  (setf *cubes* (append *cubes* (list (create-cube 0 num))))
  (rearrange-cubes)
  (redraw-cubes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cube events 			      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; click
(defun cube-clicked (cube)
  (dformat 0 "cube ~a clicked~%" (cube-number cube))
  (let ((new-group (find (cube-number cube) (screen-groups (current-screen)) :key 'group-number)))
    (and new-group (switch-to-group new-group))))

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
			     (xlib:font-ascent font)
			     string
			     :translate #'translate-id 
			     :size 16)
    (xlib:display-finish-output *display*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cube management 			      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-cubes (offset)
  (dotimes (n 2)
    (let ((cube (create-cube (+ (* n 14) offset) n)))
      (setf *cubes* (append *cubes* (list cube)))
      (draw-cube cube))))

(defun destroy-cubes ()
  (setf *cubes* (remove-if (lambda (cube)
			     (xlib:destroy-window (cube-window cube)) t)
			   *cubes*))
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
  (unless (zerop (length *cubes*)) (rearrange-cubes))
  (xlib:display-finish-output *display*))

(defun rearrange-cubes ()
  (reduce (lambda (cube1 cube2)
	    (let* ((cube1-win (cube-window cube1))
		   (cube1-width (xlib:drawable-width cube1-win))
		   (cube2-x (+ (xlib:drawable-x cube1-win) cube1-width)))
	      (setf (xlib:drawable-x (cube-window cube2)) cube2-x))
	    cube2)
	  *cubes*)
  (xlib:display-finish-output *display*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stumpwm environment   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-mode-line ()
  (dolist (h (screen-heads (current-screen)))
    (let ((mode-line (head-mode-line h)))
      (when mode-line
	(return-from current-mode-line mode-line)))))

(defun create-cubes-group ()
  (setf *cubes* (mapcar (lambda (w) (let* ((cube (create-cube (* (group-number w) 14) (group-number w))))
				      (and (eq w (current-group)) (toggle-cube cube))
				      (draw-cube cube)
				      cube))
			(sort-groups (group-screen (mode-line-current-group (current-mode-line)))))))

;; redraw cube windows
(defun redraw-cubes ()
  (mapcar (lambda (cube)
	    (setf (cube-state cube) 
		  (if (eq (cube-number cube) (group-number (current-group)))
		      :toggled
		      :normal))
	    (draw-cube cube))
	  *cubes*))

(defun init-cubes ()
  ;; Group Switch hook
  ;; To be moved to switch-to-group in group.lisp or update-mode-line
  (add-hook *focus-group-hook* (lambda (new old) 
				 (if (not (find-cube-number (group-number new)))
				     (add-cube-number (group-number new)))
				 (redraw-cubes)))
  ;; Create cubes on init
  (create-cubes-group))

