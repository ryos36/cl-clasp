;;; Sinby Corp. 2011-2022

(defpackage :cl-clasp
  (:use :cl))

(in-package :cl-clasp)

(asdf:defsystem :cl-clasp
  :version "1.9.1"
  :depends-on (:cl-who :cl-ppcre :cl-fad :uiop :local-time)
  :serial t
  :components
   ((:file "package")
    (:file "clasp")
    (:file "sharp-html")
    (:file "pre-reader")
    (:file "cgi")))
