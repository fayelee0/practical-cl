;;;; 04 syntax and semantics

#|
The evaluation of a macro form proceeds in two phase:

1. the elements of the macro form are passed, unevaluated, to the macro function
2. the form returned by the macro function -- called its expansion -- is evaluated according to the normal evaluation rules
|#
