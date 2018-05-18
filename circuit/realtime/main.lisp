(load "~/work/lab/0418/lib/runge-kutta.lisp")

;; 出力
(defun main ()
  (let* ((b 1.5d0)
         (input nil))
    (format t "Press enter to continue or #\\e to end~&")
    (loop
       (setf input (read-char))
       (if (equal input #\e) (return nil))
       (with-open-file (stream "~/work/lab/0418/circuit/output" :direction :output :if-exists :supersede)
         (loop for r in (runge-kutta (list (lambda (ti x)
                                             (+ (* 0 ti) (aref x 1)))
                                           (lambda (ti x)
                                             (+ (- (* 0.2 (aref x 1))) (- (aref x 0)) (- (* (aref x 0) (aref x 0) (aref x 0))) (* b (cos ti)))))
                                     (list 0 1) 0 100 10000)
            do (format stream "~&~{~^~,16f ~}" r)))
       (format t "B = ~,16f~&" b)
       (setf b (+ b 0.1d0)))))
