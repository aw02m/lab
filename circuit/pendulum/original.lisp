(setf *read-default-float-format* 'long-float)

;;;
;;; RK4 method
;;; function : (runge-kutta vector:関数 vector:初期値 t_0:開始時刻 t_n:終了時刻 div:区間分割数(ループ回数))
;;; return   : 区間計算値のリスト
;;;
(defun runge-kutta (f x t0 h l)
  (let* ((n (array-dimension f 0))  ; n階微分方程式
         (ti t0)
         (k (make-array (list 4 n) :element-type 'long-float  ; 行列k(vector-fの要素数 * rk法次数4)
                        :initial-element 1l0))
         (temp (make-array n :element-type 'long-float  ; バッファ
                           :initial-element 1l0)))
    (loop for i from 0 to (1- n) do
         (setf ti (float ti 1l0)
               h (float h 1l0)
               (aref x i) (float (aref x i) 1l0)))  ; 初期条件をdouble-float型に変換しておく
    (cons (append `(,ti) (coerce x 'list))  ; 初期値をcons
          (loop for i from 0 to l do
               ;; k0
               (loop for j from 0 to (1- n) do
                    (setf (aref k 0 j) (funcall (aref f j) ti x)
                          (aref temp j) (+ (aref x j) (* (* h 0.5l0) (aref k 0 j)))))
               ;; k1
               (loop for j from 0 to (1- n) do
                    (setf (aref k 1 j) (funcall (aref f j) (+ ti (* h 0.5l0)) temp)))
               (loop for j from 0 to (1- n) do
                    (setf (aref temp j) (+ (aref x j) (* (* h 0.5l0) (aref k 1 j)))))
               ;; k2
               (loop for j from 0 to (1- n) do
                    (setf (aref k 2 j) (funcall (aref f j) (+ ti (* h 0.5l0)) temp)))
               (loop for j from 0 to (1- n) do
                    (setf (aref temp j) (+ (aref x j) (* h (aref k 2 j)))))
               ;; k3 と 初期値vector : x-vector
               (loop for j from 0 to (1- n) do
                    (setf (aref k 3 j) (funcall (aref f j) (+ ti h) temp)
                          (aref x j) (+ (aref x j)
                                        (/ (* (+ (aref k 0 j)
                                                 (* 2l0 (aref k 1 j))
                                                 (* 2l0 (aref k 2 j))
                                                 (aref k 3 j))
                                              h)
                                           6l0))))
               (setf ti (+ ti h))
             ;; ループ毎の値をcollect
             collect (append `(,ti) (coerce x 'list))))))


;; to file
(let ((diff (runge-kutta #'f #'g 0.0d0 1.0d0 0 100 10000)))
  (with-open-file (stream "~/work/lab/0418/output" :direction :output :if-exists :supersede)
    (output-to stream (loop for n from 0
                         for (time x . y) in diff
                         when (zerop (mod n 10))
                         collect (list time x y)))))

;; stdout
(let ((diff (runge-kutta #'f #'g 0 1.0d0 0 100 10000)))
    (output-to t (loop for n from 0
                    for (time x . y) in diff
                    when (zerop (mod n 10))
                    collect (list time x y))))

(defun runge-kutta (f g x y time time-end n)  ;関数f 初期値(x,y)=(x0,y0) 終点x 区間[x0~終点x]の分割数n
  (let ((h (float (/ (- time-end time) n) 1.0d0))  ;分割区間長h
        k0 k1 k2 k3
        l0 l1 l2 l3)
    ;;引数型変換(fixnum->double-float)
    (setf  x (float x 1.0d0)
           y (float y 1.0d0))
    ;;ループ毎の数値(t,x,y)
    (cons (cons time (cons x y))  ;このコンスセルは初期値
          (loop for i below n do
               (setf k0 (* h (funcall f time x y))
                     l0 (* h (funcall g time x y))
                     k1 (* h (funcall f (+ time (* h 0.5d0))
                                      (+ x (* k0 0.5d0))
                                      (+ y (* l0 0.5d0))))
                     l1 (* h (funcall g (+ time (* h 0.5d0))
                                      (+ x (* k0 0.5d0))
                                      (+ y (* l0 0.5d0))))
                     k2 (* h (funcall f (+ time (* h 0.5d0))
                                      (+ x (* k1 0.5d0))
                                      (+ y (* l1 0.5d0))))
                     l2 (* h (funcall g (+ time (* h 0.5d0))
                                      (+ x (* k1 0.5d0))
                                      (+ y (* l1 0.5d0))))
                     k3 (* h (funcall f (+ time h)
                                      (+ x k2)
                                      (+ y l2)))
                     l3 (* h (funcall g (+ time h)
                                      (+ x k2)
                                      (+ y l2)))
                     time (+ time h)
                     x (+ x (/ (* h (+ k0 (* 2.0d0 k1) (* 2.0d0 k2) k3)) 6.0d0))
                     y (+ y (/ (* h (+ l0 (* 2.0d0 l1) (* 2.0d0 l2) l3)) 6.0d0)))
             collect (cons time (cons x y))))))

(let ((diff (runge-kutta #'f #'g 0 1.0 0 100 10000)))
  (pick-element (loop for n from 0
                       for (time x . y) in diff
                       when (zerop (mod n 10))
                       collect (list time x y))))

(defun pick-element (list)
  (if (null (cdr list))
      (progn (format *stream* "~A ~A ~A~%" (caar list) (cadar list) (caddar list))
             list)
      (progn (format *stream* "~A ~A ~A~%" (caar list) (cadar list) (caddar list))
             (pick-element (cdr list)))))

(defparameter *stream* (open "output" :direction :output :if-exists :overwrite))

(close *stream*)
