;;;; 03 practical, a simple database

(list 1 2 3)

;; make cd
(defun make-cd (title artist rating ripped)
  (list :title title
        :artist artist
        :rating rating
        :ripped ripped))

;; global store
(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

*db*

(defun dump-db ()
  (dolist (cd *db* )
    (format t "~{~a:~10t~a~%~}~%" cd)))

(dump-db)

;;; prompt read
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
        (if (not (y-or-n-p "Another? [y/n]: "))
            (return))))

;;; save *db*
(defun save-db (filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(save-db "my-cds.db")

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))


;;; query db
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 0))
(remove-if-not #'(lambda (x) (zerop (mod x 2)))
               '(1 2 3 4 5 6 7 8 9 0))


(remove-if-not
 #'(lambda (cd)
     (equal
      (getf cd :artist)
      "Dixie Chicks"))
 *db*)

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(select-by-artist "Dixie Chicks")

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(select (artist-selector "Dixie Chicks"))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(select (where :artist "Dixie Chicks"))
(select (where :rating 10 :ripped nil))

;;; update db
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row)
         *db*)))

(update (where :artist "Dixie Chicks") :rating 11)
(dump-db)

;;; delete db
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(delete-rows (where :artist "Dixie Chicks"))
(dump-db)

;;; defmacro
(reverse '(1 2 3))

(defmacro backwards (expr) (reverse expr))


(backwards ("hello, world" t format))

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

(make-comparison-expr :rating 10)
(make-comparison-expr :title "Give US a Break")

`(+ 1 2 (+ 1 2))
`(+ 1 2 ,(+ 1 2))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(make-comparison-expr :rating 10)
(make-comparison-expr :title "Give US a Break")


(defun make-comparison-list (fields)
  (loop
    while
    fields
    collecting
    (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))

`(and ,(list 1 2 3))
`(and ,@(list 1 2 3))
`(and ,@(list 1 2 3) 4)

(macroexpand-1 '(where :title "Give Us a Break" :ripped t))

(select (where :title "Give Us a Break" :ripped t))
