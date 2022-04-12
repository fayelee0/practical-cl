;;;; a unit test framework

(= (+ 1 2) 3)
(= (+ 1 2 3) 6)
(= (+ -1 -3) -4)


(defun test-+ ()
  (and
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4)))

(test-+)


(defun test-+ ()
  (format t "~:[FAIL~;PASS~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;PASS~] ... ~a~%" (= (+ 1 2 3) 5) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;PASS~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

(test-+)


(defun report-result (result form)
  (format t "~:[FAIL~;PASS~] ... ~a~%" result form))

(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 5) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

(test-+)


(defmacro check (form)
  `(report-result ,form ',form))

(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 5))
  (check (= (+ -1 -3) -4)))

(test-+)


(defmacro check (&body form)
  `(progn
     ,@(loop for f in form collect `(report-result ,f ',f))))

(defun test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 5)
   (= (+ -1 -3) -4)))

(test-+)


(defun report-result (result form)
  (format t "~:[FAIL~;PASS~] ... ~a~%" result form)
  result)

(defun test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 5)
   (= (+ -1 -3) -4)))

(test-+)


(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 5)
   (= (+ -1 -3) -4)))

(test-+)


(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(test-*)


(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(test-arithmetic)


(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;PASS~] ... ~a: ~a~%" result *test-name* form))

(test-arithmetic)



(defun test-+ ()
  (let ((*test-name* 'test-+))
    (check
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 5)
      (= (+ -1 -3) -4))))

(test-+)


(defun test-* ()
  (let ((*test-name* 'test-*))
    (check
      (= (* 2 2) 4)
      (= (* 3 5) 15))))

(test-*)


(test-arithmetic)


(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* ',name))
       ,@body)))

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 5)
    (= (+ -1 -3) -4)))

(test-+)


(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 5)
    (= (+ -1 -3) -4)))

(test-+)

(deftest test-* ()
  (check
      (= (* 2 2) 4)
      (= (* 3 5) 15)))

(test-*)


(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(test-arithmetic)


(deftest test-math ()
  (test-arithmetic))

(test-math)
