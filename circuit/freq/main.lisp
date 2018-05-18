(defun main ()
  (let ((b 0d0)
        (div 128)
        (loops 12800))
;    (with-open-file (stream1 "./output1" :direction :output :if-exists :supersede)
      (with-open-file (stream2 "./output2" :direction :output :if-exists :supersede)
        (loop for l from 0 to 1000 do
             (let* ((n 2)  ; n階微分方程式
                    (h (float (* 2 (/ PI div)) 1d0))  ; 分割区間長h
                    (ti 0)
                    (f (make-array n :initial-contents (list (lambda (ti x)
                                                               (+ (* 0 ti) (aref x 1)))
                                                             (lambda (ti x)
                                                               (+ (* b (cos ti)) (- (sin (aref x 0))) (- (* 0.1 (aref x 1))))))))
                    (x (make-array n :initial-contents (list 0 1)))  ;初期値
                    (k (make-array (list 4 n) :element-type 'double-float  ; 行列k(vector-fの要素数 * rk法次数4)
                                   :initial-element 1d0))
                    (temp (make-array n :element-type 'double-float  ; バッファ
                                      :initial-element 1d0)))
               ;; 変数,配列の型の初期化
               (loop for i from 0 to (1- n) do
                    (setf ti (float ti 1d0))
                    (setf (aref x i) (float (aref x i) 1d0)))
               ;(format stream1 "~&~{~^~,16f ~}" (append `(,b ,ti) (coerce x 'list)))
               ;; runge-kutta本体
               (loop for i from 0 to loops do
                    ;(if (equal i 0) (setf i 1))
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
                    (setf ti (* (mod i div) h))
                  ;; ファイルへ出力
                                        ;(format stream1 "~&~{~^~,16f ~}" (append `(,b ,ti) (coerce x 'list)))
                    (if (> i (* loops 0.8))
                        (if (equal (mod i div) 0)
                            (format stream2 "~&~{~^~,16f ~}" (append `(,b ,ti) (coerce x 'list)))))))
           ;; 初期値更新 
             (format t "B = ~,16f~&" b)
             (setf b (+ b 0.01d0))))));)
