(require 'asdf)
(require 'cl-opengl)
(require 'cl-glut)
(require 'cl-glu)

(load "mk-rkstep.lisp")
(load "func.lisp")
(load "color.lisp")
(load "window-obj.lisp")

;; main
(defun main ()
  (glut:display-window (make-instance 'Window)))
