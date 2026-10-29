(require :asdf)

(load (merge-pathnames "quicklisp/setup.lisp"
		       (user-homedir-pathname)))

(asdf:load-asd (truename "jc-jp.asd"))
(asdf:load-system "jc-jp")

(sb-ext:save-lisp-and-die
 "jc-jp"
 :executable t
 :toplevel #'jc-jp:main)
