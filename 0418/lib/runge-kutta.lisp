;;;
;;; RK4 method
;;; function : (runge-kutta f-list:関数 x-list:初期値 t_0:開始時刻 t_n:終了時刻 div:区間分割数(ループ回数))
;;; return   : 区間計算値のリスト
;;;
(defun runge-kutta (f-list x-list t0 tn div)
  (let* ((n (length f-list))  ; n階微分方程式
         (h (float (/ (- tn t0) div) 1d0))  ; 分割区間長h
         (ti t0)
         (f (make-array n :initial-contents f-list))
         (x (make-array n :initial-contents x-list))
         (k (make-array (list 4 n) :element-type 'double-float  ; 行列k(vector-fの要素数 * rk法次数4)
                        :initial-element 1d0))
         (temp (make-array n :element-type 'double-float  ; バッファ
                           :initial-element 1d0)))
    (loop for i from 0 to (1- n) do
         (setf ti (float ti 1d0))
         (setf (aref x i) (float (aref x i) 1d0)))  ; 初期条件をlong-float型に変換しておく
    (cons (append `(,ti) (coerce x 'list))  ; 初期値をcons
          (loop for i from 0 to (1- div) do
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
               ;; k3 と 初期値vector : x-vector
               (loop for j from 0 to (1- n) do
                    (setf (aref k 3 j) (funcall (aref f j) (+ ti h) temp))
                    (setf (aref x j) (+ (aref x j)
                                        (/ (* (+ (aref k 0 j)
                                                 (* 2 (aref k 1 j))
                                                 (* 2 (aref k 2 j))
                                                 (aref k 3 j))
                                              h)
                                           6))))
               (setf ti (+ ti h))
             ;; ループ毎の値をcollect
             collect (append `(,ti) (coerce x 'list))))))
