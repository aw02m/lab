;;;
;;; Return one step function of rk4-method
;;; Included clozure
;;;
(defun mk-rkstep (f-list x-list ti h)
  (let* ((n (length f-list))
         (f (make-array n :initial-contents f-list))
         (x (make-array n :initial-contents x-list))
         (k (make-array `(4 ,n) :element-type 'double-float :initial-element 0d0))
         (temp (make-array n :element-type 'double-float :initial-element 0d0))
         (prex (make-array n :initial-contents x-list)))
    ;; change type to double-float
    (loop for i from 0 to (1- n) do
         (setf h (float h 1d0)
               ti (float ti 1d0)
               (aref x i) (float (aref x i) 1d0)
               (aref prex i) (float (aref prex i) 1d0)))
    ;; clozure
    (lambda ()
      (setf (aref prex 0) (aref x 0)
            (aref prex 1) (aref x 1))
      ;; k0
      (loop for j from 0 to (1- n) do
           (setf (aref k 0 j) (funcall (aref f j) ti x))
           (setf (aref temp j) (+ (aref x j) (/ (* h (aref k 0 j)) 2))))
      ;; k1
      (loop for j from 0 to (1- n) do
           (setf (aref k 1 j) (funcall (aref f j) (+ ti (/ h 2)) temp)))
      (loop for j from 0 to (1- n) do
           (setf (aref temp j) (+ (aref x j) (/ (* h (aref k 1 j)) 2))))
      ;; k2
      (loop for j from 0 to (1- n) do
           (setf (aref k 2 j) (funcall (aref f j) (+ ti (/ h 2)) temp)))
      (loop for j from 0 to (1- n) do
           (setf (aref temp j) (+ (aref x j) (* h (aref k 2 j)))))
      ;; k3
      (loop for j from 0 to (1- n) do
           (setf (aref k 3 j) (funcall (aref f j) (+ ti h) temp)))
      ;; x-vector
      (loop for j from 0 to (1- n) do
           (setf (aref x j) (+ (aref x j)
                               (/ (* (+ (aref k 0 j)
                                        (* 2 (aref k 1 j))
                                        (* 2 (aref k 2 j))
                                        (aref k 3 j))
                                     h)
                                  6))))
      (setf ti (+ ti h))
      ;; return values
      `(,(aref x 0) ,(aref x 1)))))
