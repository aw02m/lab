(load "~/work/lab/0418/lib/runge-kutta.lisp")
(load "~/work/lab/0418/pendulum/func.lisp")

;; 出力
(defun main ()
  (loop
     (let ((input nil))
       (setf input (read-char))
       (if (equal input #\e) (return 0))
       (with-open-file (stream "~/work/lab/0418/pendulum/output" :direction :output :if-exists :supersede)
         (loop for r in (loop for n from 0
                           for (ti x y) in (runge-kutta '(f g) '(0 1) 0 100 10000)
                           collect (list ti x y))
            do (format stream "~&~{~^~,16f ~}" r)))
       (format t "~,16f~&" *B*)
       (setf *B* (+ *B* 0.1d0)))))

(main)
