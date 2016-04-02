;;; Sinby Corp. 2011

(in-package :cl-user)

(defpackage :cl-clasp
  (:use :cl :cl-who :cl-ppcre)
  (:nicknames :clasp :asp)
  (:export 
    :*html-data-dir*
    :*prologue-flag*
    :eval-to-who 
    :prop-list-to-hash-table 
    :hash-table-to-let-list
    :query-string-to-hash-table
    :get-page-property
    :page-property-is-already-converted
    :nconvert-page-property
    :merge-list-to-hash-table
    :update-hash-table
    :make-content
    :add-property

    :*pre-line-n*
    :load-text-file
    :escape-html-char
    :use-sharp->-macro

    :*eval-html-string*
    :use-sharp-<-macro
    :|#<-reader|
    :load-html-as-who
    :ignore-space

    :get-query-string
    :make-checker
    :encode-token
    :decode-token))
