;;;
;;; RK4 method
;;; function : (runge-kutta vector:関数 vector:初期値 t_0:開始時刻 t_n:終了時刻 div:区間分割数(ループ回数))
;;; return   : 区間計算値のリスト
;;;
(defun runge-kutta (f x t0 tn div)
  (let* ((n (array-dimension f 0))  ; n階微分方程式
         (h (float (/ (- tn t0) div) 1d0))  ; 分割区間長h
         (ti t0)
         (k (make-array (list 4 n) :element-type 'double-float  ; 行列k(vector-fの要素数 * rk法次数4)
                        :initial-element 1d0))
         (temp (make-array n :element-type 'double-float  ; バッファ
                           :initial-element 1d0)))
    (loop for i from 0 to (1- n) do
         (setf ti (float ti 1d0))
         (setf (aref x i) (float (aref x i) 1d0)))  ; 初期条件をlong-float型に変換しておく
    (cons (append `(,ti) (coerce x 'list))  ; 初期値をcons
          (loop for i from 0 to div do
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

;; f(t,x,y)
(defun f (ti x)
  (+ (* 0 ti) (aref x 1)))

;; g(t,x,y)
(defun g (ti x)
  (+ (* 1.01 (cos ti)) (- (sin (aref x 0))) (- (* 0.1 (aref x 1)))))

;; リストの要素を整形してstreamに出力
(defun output-to (stream list)
  (loop for r in list
     do (format stream "~&~{~^~,16f ~}" r)))

;; 出力
(let* ((f-vector (make-array 2 :initial-contents '(f g)))
       (x-vector (make-array 2 :initial-contents '(0 1)))
       (diff (runge-kutta f-vector x-vector 0 1000 100000)))
  (with-open-file (stream "~/work/lab/0418/output" :direction :output :if-exists :supersede)
    (output-to stream (loop for n from 0
                    for (ti x y) in diff
                    collect (list ti x y)))))

;;;
;;; d^2x/dt^2 = -ky -sin(x) -Bcos(t) : k = 0.1
;;; ->
;;; dx/dt = f(t,x,y) = y
;;; dy/dt = g(t,x,y) = -ky -sin(x) -Bcos(t)
;;;
