;;;; 04 syntax and semantics

;;; A typical division is to split the processor into three phases
;;; 1. a lexical analyzer breaks up the stream of characters into tokens
;;; 2. feeds them to a parser that builds a tree representing the expressions in the program, according to the language's grammar <AST>
;;; 3. fed to an evaluator that either interprets it directly or compiles it inot some other language such as machine code


;;; Common Lisp defines two black boxes
;;; 1. *Reader* translates text into Lisp objects
;;;    >> The reader defines how strings of characters can be translated inot Lisp objects called *s-expressions*.
;;; 2. *Evaluator* implements the semantics of the language in terms of those objects
;;;    >> The evaluator then defines a syntax of Lisp forms that can be built out of s-expressions.
;;;
;;; This split of the black box has a couple of consequences
;;; 1. You can use s-expressions as an externalizable data format for data other than source code, using *READ* to read it and *PRINT* to print it.
;;; 2. Since the semantics of the language are defined in terms of trees of objects rather than strings of characters,
;;;    it's easier to generate code within the language thant it would be if you had to generate code as text.


;;; the syntax of s-expressions understood by the reader
;;; the syntax of Lisp forms understood by the evaluator


;;; Lists are delimited by parentheses and can contain any number of whitespace-separated elements.
;;; Atoms are everything else.


(type-of 12345678901234567890)
(typecase 12345678901234567890)
(type-of 123)
(type-of 3/7)
(type-of 1.0)
(type-of 1.0e0)
(type-of 1.0d0)
(type-of 1.0e-4)
(type-of +42)
(type-of -42)
(type-of -1/4)
(type-of -2/8)
(type-of 246/2)


"foo"
"fo\o"
"fo\\o"
"fo\"o"

;;; Then characters that serve other syntactic purposes can't appear in names:
;;; ( )
;;; " '
;;; ` ,
;;; : ;
;;; \ |
;;;
;;; Standard style, these days, is to write code in all lowercase and let the reader chagne names to uppercase.

;;; variable        -> hello-world
;;; global variable -> *query-io*
;;; constant        -> +pi+

(quote (+ 1 2))
(list '+ 1 2)
'(+ 1 2)


#|
The evaluation of a macro form proceeds in two phase:
1. the elements of the macro form are passed, unevaluated, to the macro function
2. the form returned by the macro function -- called its expansion -- is evaluated according to the normal evaluation rules
|#

(defvar x 20)
(let ((x 10)) x)

(= 1 1)
(= 1 1.0)

(char= #\a #\b)
(char= #\a #\a)


(eq 'a 'b)
(eq 'a 'a)
(eq 2 1)
(eq 1 1.0)
(eq 1 1)

(eql 'a 'b)
(eql 'a 'a)
(eql 2 1)
(eql 1 1.0)
(eql 1 1)

(eql "hello" "hello")
(equal "hello" "hello")

(eql '(1 2 3) '(1 2 3))
(equal '(1 2 3) '(1 2 3))

(equalp "hello" "HELLO")
(equal "hello" "HELLO")

(equalp 1 1.0)

;;;; package comment

;;; function coment

;; code coment

;; -- ; line coment
