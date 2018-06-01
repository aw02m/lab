(defun pen-f (ti x)
  (+ (* 0 ti) (aref x 1)))

(defun pen-g (ti x)
  (+ (* 1.2 (cos ti)) (- (sin (aref x 0))) (- (* 0.1 (aref x 1)))))

(defun duf-f (ti x)
  (+ (* 0 ti) (aref x 1)))

(defun duf-g (ti x)
  (+ (- (* 0.1 (aref x 1)))
     (- (aref x 0))
     (- (* (aref x 0) (aref x 0) (aref x 0)))
     (* 10 (cos ti))))

(defparameter pen-f `(,#'pen-f ,#'pen-g))
(defparameter duf-f `(,#'duf-f ,#'duf-g))
(defvar init-x '(0 1))
(defvar t0 0)
