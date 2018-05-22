(require 'asdf)
(require 'cl-opengl)
(require 'cl-glut)
(require 'cl-glu)

(load "mk-rkstep.lisp")
(load "func.lisp")

(defvar *width* 800)
(defvar *height* 800)

;; class : Window
(defclass Window (glut:window)
  ()
  (:default-initargs
   :width *width* :height *height*
   :pos-x (- (/ 1920 2) (/ *width* 2)) :pos-y (/ 1920 2)
   :mode '(:rgba) :title "Chaos-Leader"))

;; init
(defmethod glut:display-window :before ((window Window))
  (gl:clear-color 1 1 1 1)
  (gl:clear :color-buffer-bit)) ; clear buffer

;; view port
(defmethod glut:reshape ((window Window) width height)
  (gl:viewport 0 0 width height)
  (gl:load-identity)
  (gl:ortho (- (/ width *width*)) (/ width *width*) (- (/ height *height*)) (/ height *height*) -1 1))
                                        ;(gl:translate (/ *width* 2) (/ *height* 2) 0))

;; keyboard
(defmethod glut:keyboard ((window Window) key x y)
  (declare (ignore x y))
  (case key
    ((#\q)
     (glut:destroy-current-window))))

;; idle
(defmethod glut:idle ((window Window))
  (sleep (/ 1.0 1000.0))
  (glut:post-redisplay))

;; display
(let ((rkstep (mk-rkstep `(,#'f ,#'g) '(2 2) 0 0.01)))
  (defmethod glut:display ((window Window))
    (let ((rkvalue (funcall rkstep)))
      (%gl:color-3d 0.4 0.6 0.8) ; line color

      (gl:begin :line-strip) ; line-strip : line with no finishing clozure
      (%gl:vertex-2d (* 0.15 (caar rkvalue)) (* 0.15 (cadar rkvalue)))
      (%gl:vertex-2d (* 0.15 (caadr rkvalue)) (* 0.15 (cadadr rkvalue)))
      (gl:end)

      (gl:flush)))) ; show window

;; main
(defun main ()
  (glut:display-window (make-instance 'Window)))
