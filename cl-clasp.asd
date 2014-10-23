;;; Sinby Corp. 2011, 2014

(defpackage :cl-clasp
  (:use :cl :cl-who :cl-ppcre))

(in-package :cl-clasp)

(asdf:defsystem :cl-clasp
  :version "1.06"
  :depends-on (:cl-who :cl-ppcre)
  :serial t
  :components
   ((:file "package")
    (:file "clasp")
    (:file "sharp-html")
    (:file "pre-reader")
    (:file "cgi")))
