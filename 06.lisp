;;;; 06 variables

;;; CL supports two kinds of variables:
;;; 1. lexical
;;; 2. dynamic

;;; Each time a function is called, Lisp creates new *bindings* to hold the arguments passed by the function's caller.
;;; A binding is the runtime manifestation of a variable.

(macroexpand-1 '(incf x))

(macroexpand-1 '(incf x 10))

(macroexpand-1
 '(incf (aref *array* (random (length *array*)))))

(defvar a 10)
(defvar b 20)

(shiftf a b 30)

a
b

(rotatef a b)

a
b

(setf a 10)
(setf b 20)
(let ((a 1)
      (b 2))
  (+ a b))
(+ a b)

;;; The *scope* of function parameters and LET variables
;;; the area of the program where the variable name can be used to refer to the variable's binding
;;; is delimited by the form that introduces the variable
;;;
;;; This form
;;; the function or the LET
;;; is called the *binding form*

(defun foooo (x)
  (format t "parameter: ~a~%" x)
  (let ((x 2))
    (format t "outer let: ~a~%" x)
    (let ((x 3))
      (format t "inner let: ~a~%" x))
    (format t "outer let: ~a~%" x))
  (format t "parameter: ~a~%" x))

(foooo 10)


(macroexpand-1
 '(dotimes (x 10)
   (format t "~d " x)))

;;; By default all binding forms in Common Lisp introduce *lexically scoped* variables.
;;; Laxically scoped variables can be referred to only by code that's textually within the binding form.

(defparameter *fn*
      (let ((count 0))
        #'(lambda ()
            (setf count (1+ count)))))

(funcall *fn*)
(funcall *fn*)

;;; The key thing to understand about closures is that it's the binding, not the value of the variable, that's captured.
;;; Thus, a closure can not only access the value of the variables it closes over but can also assign new values that will persist between calls to the closure.

(let ((count 0))
  (list
   #'(lambda () (incf count))
   #'(lambda () (decf count))
   #'(lambda () count)))



;;; dynamic scop
(defvar *x* 10)

(defun foo () (format t "X: ~d~%" *x*))

(foo)

(let ((*x* 20)) (foo))

(foo)

(defun bar ()
  (foo)
  (let ((*x* 20)) (foo))
  (foo))

(bar)

(defun foo ()
  (format t "before assignment ~18tX: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "after  assignment ~18tX: ~d~%" *x*))

(foo)

(bar)

;;; The name of every variable defined with *DEFVAR* and *DEFPARAMETER* is automatically declared globally special.


(macroexpand-1
 '(setf x 10))


;; setf
;;
;; incf
;; decf
;;
;; random
;;
;; push
;; pop
;;
;; rotatef
;; shiftf
