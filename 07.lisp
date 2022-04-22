;;;; 07 Macros: standard control constructs

(if (> 2 3) "Yup" "Nope")
(if (> 2 3) "Yup")
(if (> 3 2) "Yup" "Nope")


(defmacro ^when (condition &rest body)
  `(if ,condition (progn ,@body)))

(macroexpand-1
 '(^when (spam-p current-message)
   (file-in-spam-folder current-message)
   (update-spam-database current-message)))


(defmacro ^unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))


(macroexpand-1
 `(and 1 2 3))


(macroexpand-1
 '(dolist (x '(1 2 3 4 5))
   (format t ".~a~%" x)))

(dolist (x '(1 2 3 4))
  (print x))

(dolist (x '(1 2 3 4))
  (print x)
  (if (evenp x)
      (return)))

(dotimes (i 4)
  (print i))

(dotimes (i 4)
  (print i)
  (if (oddp i)
      (return)))

(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

;; n c nx
;; 0 0 1
;; 1 1 1
;; 2 1 2
;; 3 2 3
;; ...
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))             ; let
    ((= 10 n) cur))

(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))


(do ((nums nil)
     (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))

(loop for i from 1 to 10 collecting i)
(loop for x from 1 to 10 summing (expt x 2))
(loop for x across "the quick brown fox jumps over the lazy dog"
      counting (find x "aeiou"))
(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return a))
