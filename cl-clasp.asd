;;; Sinby Corp. 2011-2019

(defpackage :cl-clasp
  (:use :cl))

(in-package :cl-clasp)

(asdf:defsystem :cl-clasp
  :version "1.8"
  :depends-on (:cl-who :cl-ppcre)
  :serial t
  :components
   ((:file "package")
    (:file "clasp")
    (:file "sharp-html")
    (:file "pre-reader")
    (:file "cgi")))
