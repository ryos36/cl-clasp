;;; Sinby Corp. 2011

(in-package :cl-user)

(defpackage :cl-clasp
  (:use :cl)
  (:nicknames :clasp :asp)
  (:export
    :*html-data-dir*
    :*html-local-dir*
    :*config-txt-dir*
    :*config-txt-n*
    :*special-package-name-aliases*
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

    :*html-escape-mode*
    :*pre-line-n*
    :expand-file-name
    :expand-load
    :load-text-file
    :escape-html-char
    :use-sharp->-macro

    :*eval-html-string*
    :use-sharp-<-macro
    :|#<-reader|
    :load-html-as-who
    :ignore-space

    :make-dependency

    :get-query-string
    :make-checker
    :encode-token
    :decode-token))
