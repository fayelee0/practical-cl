;;;; 05 functions

;;; Different flavors of parameters handle required, optional, multiple, and keyword arguments.


(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))


(defun bar (a b &optional c d)
  (list a b c d))

(bar 1 2)
(bar 1 2 3)
(bar 1 2 3 4)

(defun baz (a &optional (b 10))
  (list a b))

(baz 1 2)
(baz 1)

(defun make-rectangle (width &optional (height width))
  (cons width height))

(make-rectangle 1)
(make-rectangle 1 2)
;; (make-rectangle 1 2 3)

(defun bazz (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

(bazz 1 2)
(bazz 1 2 3)

(defun bzz (&key a b c)
  (list a b c))

(bzz)
(bzz :a 1)
(bzz :a 1 :b 2)
(bzz :a 1 :b 2 :c 3)

(defun foz (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

(foz)
(foz :a 1)
(foz :b 2)
(foz :b 1 :c 4)
(foz :a 2 :b 1 :c 4)

(defun fzz (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))

(fzz :apple 10 :box 20 :charlie 30)

;;; Whenever more than one flavor of parameter is used, they must be declared in the order I've discussed them:
;;; 1. the names of the required parameters;
;;; 2. the optional parameters
;;; 3. the rest parameter
;;; 4. the keyword parameters

(defun fooo (x &optional y &key z)
  (list x y z))

(fooo 1 2 :z 3)
(fooo 1)
;; (fooo 1 :z 3)


(defun barr (&rest rest &key a b c)
  (list rest a b c))

(barr :a 1 :b 2 :c 3)
;; (barr 10 20 30 :a 1 :b 2 :c 3)


;;; *RETURN-FROM* special operator to immediately return any value from the function

;;; When you define a function with *DEFUN*, you're really doing two things:
;;; 1. creating a new function object
;;; 2. and giving it a name

;;; FUNCTION
;;; #'

;;; FUNCALL is the one to use when you know the number of arguments you're going to pass to the function at the time you write the code.
;;; APPLY receives a list



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
