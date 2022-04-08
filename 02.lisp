10

(+ 2 3)

"hello, world"

(format t "hello, world~%")

(defun hello-world ()
  (format t "hello, world~%"))

(hello-world)

(list 1 2 3)

(list :a 1 :b 2 :c 3)

(getf (list :a 1 :b 2 :c 3) :a)
(getf (list :a 1 :b 2 :c 3) :c)

(defun make-cd (title artist rating ripped)
  (list :title title
        :artist artist
        :rating rating
        :ripped ripped))

(make-cd "Roses" "Kathy Mattea" 7 t)

(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

*db*

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(dump-db)

(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(dump-db)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   ;; (prompt-read "Rating")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   ;; (prompt-read "Ripped [y/n]")
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? : [y/n]: "))
            (return))))

;; (add-cds)

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(save-db "./my-cds.db")

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(load-db "./my-cds.db")

*db*

(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 0))

(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 0))

(remove-if-not
 #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")) *db*)

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(select-by-artist "Dixie Chicks")
