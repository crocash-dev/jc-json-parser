(defpackage #:jc-jp
  (:use #:common-lisp
	#:iterate)
  (:export #:main))

(in-package #:jc-jp)

(defun test-fun (x)
  (iter (for i from 1 to x)
    (princ i)))
