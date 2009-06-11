;;; Modeline cubes - A group switcher widget for the mode-line

(in-package :stumpwm)
(export '(create-cube create-cubes destroy-cubes find-cube-window cube-clicked))

(defparameter *cubes* '())

;; Show Group numbers or Group formatted names?
(defparameter *cube-display-number* nil)

;; border
(defparameter *cube-border-width* 1)
(defparameter *cube-border-color* "Black")
;; colors
(defparameter *cube-background* "Gray")
(defparameter *cube-background-toggled* "Orange")
(defparameter *cube-foreground* "Black")
(defparameter *cube-foreground-toggled* "Black")

(defstruct cube
  state
  number
  group
  window
  gcontext-normal
  gcontext-toggled)

(defun create-cube (ml group &optional (x 0))
  "Create cube numer num at position x on mode-line ml"
  (let* ((screen (mode-line-screen ml))
         (font (screen-font screen))
         (parent (mode-line-window ml))
         (win (xlib:create-window
               :parent parent
               :x x
               :y 0
               :width  (* (xlib:char-width (screen-font screen) 0) 2)
               :height (mode-line-height ml)
               :border (alloc-color screen *cube-border-color*)
               :border-width *cube-border-width*
               :event-mask (xlib:make-event-mask :exposure :button-press)))
         (fg (alloc-color screen *cube-foreground*))
         (bg (alloc-color screen *cube-background*))
         (fg-toggled (alloc-color screen *cube-foreground-toggled*))
         (bg-toggled (alloc-color screen *cube-background-toggled*))
         (gcontext-normal (xlib:create-gcontext :drawable win
                                                :font font
                                                :foreground fg
                                                :background bg))
         (gcontext-toggled (xlib:create-gcontext :drawable win
                                                 :font font
                                                 :foreground fg-toggled
                                                 :background bg-toggled))
         (cube (make-cube :state :normal
                          :number (group-number group)
                          :group group
                          :window win
                                        ;     :mode-line ml
                          :gcontext-normal gcontext-normal
                          :gcontext-toggled gcontext-toggled)))
    (setf (xlib:window-plist win) (list 'cube cube))
    cube))

(defun toggle-cube (cube)
  (cond ((eq (cube-state cube) :normal)
         (setf (cube-state cube) :toggled))
        ((eq (cube-state cube) :toggled)
         (setf (cube-state cube) :normal))))

(defun add-cube-group (ml group)
  (setf (mode-line-cubes ml)
        (sort (append (mode-line-cubes ml) (list (create-cube ml group)))
              #'< :key 'cube-number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cube events          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; click
(defun cube-clicked (cube)
  (let ((new-group (find (cube-number cube) (screen-groups (current-screen)) :key 'group-number)))
    (and new-group (switch-to-group new-group))))

;; exposure
(defun draw-cube (cube)
  (let* ((win (cube-window cube))
         (gc  (or (and (eq (cube-state cube) :toggled) (cube-gcontext-toggled cube))
                  (cube-gcontext-normal cube)))
         (font (xlib:gcontext-font gc))
                                        ;(xlib:char-width font 0))
         (string (cube-string cube))
         (char-width (xlib:char-width font 0))
         (text-width (xlib:text-width font string))
         (window-width (+ text-width
                          char-width)))
    ;; change window width if different
    (unless (eq (xlib:drawable-width win) window-width)
      (setf (xlib:drawable-width win) window-width))
    ;; sync window background with gc background
    (setf (xlib:window-background win) (xlib:gcontext-background gc))
    (xlib:map-window win)
    ;; draw text
    (xlib:clear-area win)
    (xlib:draw-image-glyphs  win gc (round (/ char-width 2)) ;; char-width / 2 draws at center
                             (xlib:font-ascent font)
                             string
                             :translate #'translate-id
                             :size 16)
    (xlib:display-finish-output *display*)))

(defun cube-string (cube)
  (if *cube-display-number*
      (write-to-string (group-number (cube-group cube)))
      (format-expand *group-formatters* *group-format* (cube-group cube))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cube management          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun destroy-cubes (ml)
  (setf (mode-line-cubes ml) (remove-if (lambda (cube)
                                          (xlib:destroy-window (cube-window cube)) t)
                                        (mode-line-cubes ml)))
  (xlib:display-finish-output *display*))

(defun find-cube-window (win)
  (second (xlib:window-plist win)))

(defun find-cube-number (ml num)
  (find-if (lambda (cube)
             (eq (cube-number cube) num))
           (mode-line-cubes ml)))

;; Delete a cube window and remove it from *cubes*
;; Apply key on each cube and delete if = arg
(defun delete-cube (ml arg key)
  (setf (mode-line-cubes ml) (remove-if (lambda (cube)
                                          (if (eq (funcall (symbol-function key) cube) arg)
                                              (progn (xlib:destroy-window (cube-window cube)) t)))
                                        (mode-line-cubes ml)))
  ;;  (unless (zerop (length (mode-line-cubes ml))) (rearrange-cubes ml))
  (xlib:display-finish-output *display*))

(defun rearrange-cubes (ml &optional (x 0))
  (and
   (mode-line-cubes ml)
   (progn (setf (xlib:drawable-x (cube-window (first (mode-line-cubes ml))))
                x)
          (reduce (lambda (cube1 cube2)
                    (let* ((cube1-win (cube-window cube1))
                           (cube1-width (xlib:drawable-width cube1-win))
                           (cube2-x (+ (xlib:drawable-x cube1-win) cube1-width)))
                      (setf (xlib:drawable-x (cube-window cube2)) cube2-x))
                    cube2)
                  (mode-line-cubes ml))
          (redraw-cubes ml)
          (xlib:display-finish-output *display*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stumpwm environment   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun group-mode-lines (group)
  (mapcar (lambda (head) (head-mode-line head))
          (screen-heads (group-screen group))))

(defun group-exists-p (group)
  (and (find group (screen-groups (group-screen group))) t))

(defun create-mode-line-cubes (ml)
  (destroy-cubes ml)
  (dolist (w (sort-groups (group-screen (mode-line-current-group ml))))
    (add-cube-group ml w)))

;; redraw cube windows
(defun redraw-cubes (ml)
  (mapcar (lambda (cube)
            (setf (cube-state cube)
                  (if (eq (cube-number cube) (group-number (current-group)))
                      :toggled
                      :normal))
            (draw-cube cube))
          (mode-line-cubes ml)))

(defun cube-switch (new old)
  (let ((old-group-exists (group-exists-p old)))
    (mapcar (lambda (ml)
              ;; FIXME: cache group number
              (if (not (find-cube-number ml (group-number new)))
                  (add-cube-group ml new)
                  (redraw-cubes ml))
              (if (not old-group-exists)
                  (delete-cube ml old 'cube-group)))
            (group-mode-lines new))))

(defun add-cube-switch-hook ()
  ;; Group Switch hook
  ;; To be moved to switch-to-group in group.lisp or update-mode-line
  (add-hook *focus-group-hook* (lambda (new old) (cube-switch new old))))

