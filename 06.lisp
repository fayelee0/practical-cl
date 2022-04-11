;;;; 06 variables

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
