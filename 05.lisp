;;;; 05 functions

(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

;; (documentation #'verbose-sum)

(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))

(function foo)

(defun foo (x)
  (* 2 x))

(funcall #'foo 100)

(defun plot (fn min max step)
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

(plot #'exp 0 4 1/2)
