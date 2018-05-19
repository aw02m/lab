(require 'asdf)
(require 'cl-opengl)
(require 'cl-glut)
(require 'cl-glu)

(load "runge-kutta.lisp")
(load "func.lisp")

(defvar *width* 800)
(defvar *height* 800)

(defclass Window (glut:window)
  ()
  (:default-initargs
   :width *width* :height *height*
   :pos-x (- (/ 1920 2) (/ *width* 2)) :pos-y (/ 1920 2)
   :mode '(:rgba) :title "Chaos-Leader"))

;; init
(defmethod glut:display-window :before ((window Window))
  (gl:clear-color 1 1 1 1))

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

;; display
(defmethod glut:display ((window Window))
  (gl:clear :color-buffer-bit) ; clear buffer

  (%gl:color-3d 0.4 0.6 0.8) ; line color
  
  (gl:begin :line-strip) ; line-strip : line with no finishing clozure
  ;; runge-kutta loop with lines
  (loop for r in (runge-kutta (list f g) ; list of function
                              (list 0 1) ; list of parameter
                              0 0.01 10000) ; t0 h loops
     do (%gl:vertex-2d (* 0.15 (cadr r)) (* 0.15 (caddr r)))) ; pick x y from r
  (gl:end)
  
  (gl:flush)) ; show window

(defun main ()
  (glut:display-window (make-instance 'Window)))
