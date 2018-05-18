(defparameter *B* 1.200000000000d0)

(defun f (ti x)
  (+ (* 0 ti) (aref x 1)))

(defun g (ti x)
  (+ (* *B* (cos ti)) (- (sin (aref x 0))) (- (* 0.1 (aref x 1)))))
