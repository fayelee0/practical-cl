;;;; 11 collections

(vector)
(vector 1)
(vector 1 2)

(make-array 5 :initial-element nil)


(defparameter *x* (make-array 5 :fill-pointer 0))
(vector-push 'a *x*)
(vector-push 'b *x*)
(vector-push 'c *x*)
(vector-push 'd *x*)
(vector-push 'e *x*)
(vector-push 'f *x*)

*x*


(defparameter *y* (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character))
;; (vector-push-extend "你好" *y*)
(vector-push #\a *y*)
(vector-push #\a *y*)
(vector-push #\a *y*)
(vector-push #\a *y*)
(vector-push #\a *y*)

*y*

(vector-push-extend #\b *y*)

*y*


(defparameter *z* (vector 1 2 3))

(length *z*)
(elt *z* 0)
;; (elt *z* 3)

(setf (elt *z* 2) 10)
*z*


;;; vector

(let ((x #(1 2 3 4)))
  (progn
    x
    ;; (vector-push 5 x)
    (aref x 3)
    (setf (aref x 3) 5)
    x))

(let ((x (make-array 5 :initial-element nil)))
  (progn
    x
    ;; (dotimes (i 6)
    ;;   (vector-push-extend i x))
    (dotimes (i 5)
      (setf (aref x i) i))
    x))

(let ((x (make-array 5 :fill-pointer 0)))
  (progn
    x
    (dotimes (i 6)
      (vector-push i x)
      ;; (vector-push-extend i x)
      )
    x))

(let ((x (make-array 5 :fill-pointer 0 :adjustable t)))
  (progn
    (format t "~d~%" (length x))
    (dotimes (i 8)
      (vector-push i x))
    (format t "~d~%" (length x))
    (dotimes (i 8)
      (vector-push-extend i x))
    (format t "~d~%" (length x))
    (dotimes (i 10)
      (vector-pop x))
    (format t "~d~%" (length x))))


(let ((x #(1 2 1 2 3 1 2 3 4))
      (y '(1 2 1 2 3 1 2 3 4))
      (z "foobarbaz"))
  (count 1 x)
  (remove 1 x)
  (remove 1 y)
  (remove #\a z)
  (substitute 10 1 x)
  (substitute 10 1 y)
  (find 1 x)
  (find 10 x)
  (position 1 x))


(let ()
  (count "foo" #("foo" "bar" "baz") :test #'string=)
  (find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first))


(let ((x '(1 2 3 4 4 3 2 1 1 2 3 4 4 3 2 1)))
  (format t "~a~%" x)
  (sort x #'<)
  (format t "~a~%" x))

(fboundp 'merge)
(let ()
  (merge 'vector #(1 3 5) '(2 4 6) #'<)
  (merge 'list #(1 3 5) '(2 3 4) #'<))


(let* ((x "foobarbaz")
       (y (copy-seq x)))
  y
  (setf (subseq y 3 6) "xxx")
  (format t "~a ~a ~%" x y)
  (setf (subseq y 3 6) "abcd")
  (format t "~a ~a ~%" x y)
  (setf (subseq y 3 6) "xx")
  (format t "~a ~a ~%" x y))


(let ((x #(1 2 3 4 5)))
  (every #'evenp x)
  (some #'evenp x)
  (notany #'evenp x)
  (notevery #'evenp x))

(let ((x #(1 2 3 4))
      (y #(5 4 3 2)))
  (every #'> x y)
  (some #'> x y)
  (notany #'> x y)
  (notevery #'> x y))


(map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6))

(let ((a '(1 2 3))
      (b '(4 5 6))
      (c '(5 6 7)))
  (map-into a #'+ a b c))


(reduce #'+ '(1 2 3 4 5 6 7 8 9 0))
(reduce #'max '(1 2 3 4 5 6 7 8 9 0))


(let ((h (make-hash-table)))
  (gethash 'foo h)
  (setf (gethash 'foo h) 'quux)
  (gethash 'foo h))


(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
        (format nil "Value ~a actually present." value)
        (format nil "Value ~a because key not found." value))))

(let ((x (make-hash-table)))
  (show-value 'foo x)
  (setf (gethash 'foo x) 'bar)
  (show-value 'foo x)

  (setf (gethash 'bar x) 'bar)
  (setf (gethash 'baz x) 'baz)

  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) x))
