;;;
;;; RK4 method
;;; (runge-kutta f-list:関数 x-list:初期値 t_0:開始時刻 h:ステップ時間 loops:ループ回数)
;;; return : 計算値のリスト
;;;
(defun runge-kutta (f-list x-list t0 h loops)
  (let* ((ti t0)
         (n (length f-list))
         (f (make-array n :initial-contents f-list))
         (x (make-array n :initial-contents x-list))
         (k (make-array `(4 ,n) :element-type 'double-float :initial-element 1d0))
         (temp (make-array n :element-type 'double-float :initial-element 1d0)))
    ;; 型変換 -> double-float
    (loop for i from 0 to (1- n) do
         (setf h (float h 1d0))
         (setf ti (float ti 1d0))
         (setf (aref x i) (float (aref x i) 1d0)))
    ;; 本体
    (cons (append `(,ti) (coerce x 'list))  ; 初期値を先頭にcons
          (loop for i from 0 to (1- loops) do
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
             ;; ループ毎の値をcollect
             collect (append `(,ti) (coerce x 'list))))))
