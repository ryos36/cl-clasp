#!/usr/local/bin/clisp -q -E EUC-JP -K full

#-:asdf (load "/usr/local/lib/common-lisp/asdf/clispfasl/asdf.fasl")
(pushnew "../" asdf:*central-registry* :test #'equal)
(asdf:load-system :cl-ppcre)
(asdf:load-system :cl-who)
(asdf:load-system :cl-clasp)

(setf h
      (asp:prop-list-to-hash-table 
	'((main "test")
	  (sub :file "sub.who")
	  (sub-lib :lib "lib.props"))))

(format t "~s~%" h)

(asp:use-sharp-html-macro)
(setf html-who #<<div> "どうですかね" </div>>#)
(format t "~s~%" html-who)

(format t "~s~%" 
	(asp:load-html-as-who #P"html-data/test.html"))
