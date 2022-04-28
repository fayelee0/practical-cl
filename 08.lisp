;;;; 08 Macros

;;; Macros are part of the language to allow you to create abstractions on top of the core language and
;;; standard library that move you closer toward being able to directly express the things you want to express.

(defmacro $when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defun foo (x)
  ($when (> x 10) (print 'big)))

(macroexpand-1
 '($when (> x 10) (print 'big)))

(foo 10)
(foo 11)


;; do-primes
(defun primep (number)
  ($when (> number 1)
         (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(primep 3)
(primep 1125)

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

(next-prime 4)

;; (do-primes (p 0 19)
;;   (format t "~d " p))

(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))


(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))     ; 可以使用 destructuring
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))


(macroexpand-1
 '(do-primes (p 0 19) (format t "~d " p)))

(macroexpand-1
 '(do-primes (p 0 (random 100)) (format t "~d " p)))


;; (defmacro do-primes ((var start end) &body body)
;;   `(do ((ending-value ,end)
;;         (,var (next-prime ,start) (next-prime (1+ ,var)))) ; 存在参数加载顺序的问题
;;        ((> ,var ending-value))
;;      ,@body))

;; (defmacro do-primes ((var start end) &body body)
;;   `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
;;         (ending-value ,end))            ; 变量重名问题，shadow
;;        ((> ,var ending-value))
;;      ,@body))


(macroexpand-1
 '(do-primes (ending-value 0 10)
   (print ending-value)))

(macroexpand-1
 '(let ((ending-value 0))
   (do-primes (p 0 10)
     (incf ending-value p))
   ending-value))


(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))


;; It's actually fairly simple if you follow these rules of thumb:
;; + Unless there's a particular reason to do otherwise, include any subforms in the expansion in positions that will be evaluated in the same order as the subforms appear in the macro call.
;; + Unless there's a particular reason to do otherwise, make sure subforms are evaluated only once by creating a variable in the expansion to hold the value of evaluating the argument form and then using that variable anywhere else the value is needed in the expansion.
;; + Use *GENSYM* at macro expansion time to create variable names used in the expansion.


(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
