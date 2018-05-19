(defun f (ti x)
  (+ (* 0 ti) (aref x 1)))

(defun g (ti x)
  (+ (- (* 0.2 (aref x 1)))
     (- (aref x 0))
     (- (* (aref x 0) (aref x 0) (aref x 0)))
     (* 10 (cos ti))))
