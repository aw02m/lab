(load "~/work/lab/0418/lib/runge-kutta.lisp")

(defun f (ti x)
  (* 10 (- (aref x 1) (aref x 0))))

(defun g (ti x)
  (- (* 28 (aref x 0)) (* (aref x 0) (aref x 2)) (aref x 1)))

(defun h (ti x)
  (- (* (aref x 0) (aref x 1)) (* (float (/ 8 3) 1d0) (aref x 2))))

;; 出力
(defun main ()
  (let ((f-vector (make-array 3 :initial-contents '(f g h)))
        (x-vector (make-array 3 :initial-contents '(1 1 1))))
    (with-open-file (stream "~/work/lab/0418/chen-ueta/output" :direction :output :if-exists :supersede)
      (loop for r in (loop for n from 0
                        for (ti x y z) in (runge-kutta f-vector x-vector 0 100 10000)
                        collect (list ti x y z))
         do (format stream "~&~{~^~,16f ~}" r)))))

(main)
