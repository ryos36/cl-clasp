;;;
;;; original is cybertiggyr-cgi
;;;

(in-package :cl-clasp)

(defvar *separators* '(#\; #\&)
  "List of characters which may separate the key-value pairs of
a URL-encoded query strings.  DECODE-URL accepts any character
that's in this list as a separator.  ENCODE-URL uses the first
character in this list as the separator.")

(defun append-str (adjstr to-append)
  "Private utility function.
Use VECTOR-PUSH-EXTEND to append the characters of TO-APPEND
to ADJSTR.  Goes without saying that ADJSTR is an adjustable
array of characters."
  (do ((len (length to-append))
       (i 0 (1+ i)))
      ((>= i len) adjstr)
      (vector-push-extend (char to-append i) adjstr)))

(defvar *encode* (make-hash-table)
  "Map characters to their encodings.  The keys are characters.  The
values are strings.  Many of the values will be strings of one
character.  The reason for using a hash table that I've
initialized in a brute-force way, with literal statements, is
to simplify the encoding algorithm itself.  Also, it might be more
portable to non-ASCII machines (because the porting was done
when you converted this source file from ASCII to your local
character set.)")
(setf (gethash #\Newline  *encode*) "%0A"
      (gethash #X0D       *encode*) "%0D" ; carraige return, assumes ASCII
      (gethash #\Space    *encode*) "+"
      (gethash #\!        *encode*) "!"
      (gethash #\"        *encode*) "%22"
      (gethash #\#        *encode*) "%23"
      (gethash #\$        *encode*) "$"
      (gethash #\%        *encode*) "%25"
      (gethash #\&        *encode*) "%26"
      (gethash #\'        *encode*) "'"
      (gethash #\(        *encode*) "("
      (gethash #\)        *encode*) ")"
      (gethash #\*        *encode*) "*"
      (gethash #\+        *encode*) "%2B"
      (gethash #\,        *encode*) ","
      (gethash #\-        *encode*) "-"
      (gethash #\.        *encode*) "."
      (gethash #\/        *encode*) "%2F"
      (gethash #\0        *encode*) "0"
      (gethash #\1        *encode*) "1"
      (gethash #\2        *encode*) "2"
      (gethash #\3        *encode*) "3"
      (gethash #\4        *encode*) "4"
      (gethash #\5        *encode*) "5"
      (gethash #\6        *encode*) "6"
      (gethash #\7        *encode*) "7"
      (gethash #\8        *encode*) "8"
      (gethash #\9        *encode*) "9"
      (gethash #\:        *encode*) "%3A"
      (gethash #\;        *encode*) "%3B"
      (gethash #\<        *encode*) "%3C"
      (gethash #\=        *encode*) "%3D"
      (gethash #\>        *encode*) "%3E"
      (gethash #\?        *encode*) "%3F"
      (gethash #\@        *encode*) "%40"
      (gethash #\A        *encode*) "A"
      (gethash #\B        *encode*) "B"
      (gethash #\C        *encode*) "C"
      (gethash #\D        *encode*) "D"
      (gethash #\E        *encode*) "E"
      (gethash #\F        *encode*) "F"
      (gethash #\G        *encode*) "G"
      (gethash #\H        *encode*) "H"
      (gethash #\I        *encode*) "I"
      (gethash #\J        *encode*) "J"
      (gethash #\K        *encode*) "K"
      (gethash #\L        *encode*) "L"
      (gethash #\M        *encode*) "M"
      (gethash #\N        *encode*) "N"
      (gethash #\O        *encode*) "O"
      (gethash #\P        *encode*) "P"
      (gethash #\Q        *encode*) "Q"
      (gethash #\R        *encode*) "R"
      (gethash #\S        *encode*) "S"
      (gethash #\T        *encode*) "T"
      (gethash #\U        *encode*) "U"
      (gethash #\V        *encode*) "V"
      (gethash #\W        *encode*) "W"
      (gethash #\X        *encode*) "X"
      (gethash #\Y        *encode*) "Y"
      (gethash #\Z        *encode*) "Z"
      (gethash #\[        *encode*) "%5B"
      (gethash #\\        *encode*) "%5C"
      (gethash #\]        *encode*) "%5D"
      (gethash #\^        *encode*) "%5E"
      (gethash #\_        *encode*) "_"
      (gethash #\`        *encode*) "%60"
      (gethash #\a        *encode*) "a"
      (gethash #\b        *encode*) "b"
      (gethash #\c        *encode*) "c"
      (gethash #\d        *encode*) "d"
      (gethash #\e        *encode*) "e"
      (gethash #\f        *encode*) "f"
      (gethash #\g        *encode*) "g"
      (gethash #\h        *encode*) "h"
      (gethash #\i        *encode*) "i"
      (gethash #\j        *encode*) "j"
      (gethash #\k        *encode*) "k"
      (gethash #\l        *encode*) "l"
      (gethash #\m        *encode*) "m"
      (gethash #\n        *encode*) "n"
      (gethash #\o        *encode*) "o"
      (gethash #\p        *encode*) "p"
      (gethash #\q        *encode*) "q"
      (gethash #\r        *encode*) "r"
      (gethash #\s        *encode*) "s"
      (gethash #\t        *encode*) "t"
      (gethash #\u        *encode*) "u"
      (gethash #\v        *encode*) "v"
      (gethash #\w        *encode*) "w"
      (gethash #\x        *encode*) "x"
      (gethash #\y        *encode*) "y"
      (gethash #\z        *encode*) "z"
      (gethash #\{        *encode*) "%7B"
      (gethash #\|        *encode*) "%7C"
      (gethash #\}        *encode*) "%7D"
      (gethash #\~        *encode*) "%7E")

(defun encode-token (str)
  "Encode STR so it can be used in an URL.  Alphanumeric
characters & some others encode as themselves.  Other
characters have special encodings.  Returns the new string.
Does not affect STR."
  (do ((strlen (length str))
       (result (make-array 10 :element-type 'character :fill-pointer 0
			   :adjustable t))
       (i 0 (1+ i)))
      ((>= i strlen) result)
      (append-str result
		  (if (gethash (char str i) *encode*)
		      (gethash (char str i) *encode*)
		    (format nil "%~2,'0X" (char-code (char str i)))))))
	
(defun encode-url (alst)
  "ALST is an association-list (elements are key-value pairs).
ENCODE-URL encodes the list as an URL & returns the URL in a
string.  It uses the first character of *SEPARATORS* as the
character to separate key-value pairs."
  (let ((str ""))
    (when alst
      ;;
      ;; The first key/value pair is a special case.  It is not
      ;; preceeded by a separator character.
      ;;
      (setq str (format nil "~A=~A" (encode-token (car (first alst)))
			(encode-token (cdr (first alst)))))
      (setq alst (rest alst))

      ;;
      ;; Now do the rest of the key/value pairs.
      ;;
      (do ()
	  ((endp alst))
	  (setq str (format nil "~A~A~A=~A" str (first *separators*)
			    (encode-token (car (first alst)))
			    (encode-token (cdr (first alst)))))
	  (setq alst (rest alst))))
    str))


(defvar *decode* (make-hash-table)
  "Map strings to their decoded characters.  The keys are strings.
The values are characters.  Some of the key strings are associated
with the same character values as other key strings.")
(setf (gethash "+" *decode*) #\Space
      (gethash "!" *decode*) #\!
      (gethash "$" *decode*) #\$
      (gethash "'" *decode*) #\'
      (gethash "(" *decode*) #\(
      (gethash ")" *decode*) #\)
      (gethash "*" *decode*) #\*
      (gethash "," *decode*) #\,
      (gethash "-" *decode*) #\-
      (gethash "." *decode*) #\.
      (gethash "0" *decode*) #\0
      (gethash "1" *decode*) #\1
      (gethash "2" *decode*) #\2
      (gethash "3" *decode*) #\3
      (gethash "4" *decode*) #\4
      (gethash "5" *decode*) #\5
      (gethash "6" *decode*) #\6
      (gethash "7" *decode*) #\7
      (gethash "8" *decode*) #\8
      (gethash "9" *decode*) #\9
      (gethash "A" *decode*) #\A
      (gethash "B" *decode*) #\B
      (gethash "C" *decode*) #\C
      (gethash "D" *decode*) #\D
      (gethash "E" *decode*) #\E
      (gethash "F" *decode*) #\F
      (gethash "G" *decode*) #\G
      (gethash "H" *decode*) #\H
      (gethash "I" *decode*) #\I
      (gethash "J" *decode*) #\J
      (gethash "K" *decode*) #\K
      (gethash "L" *decode*) #\L
      (gethash "M" *decode*) #\M
      (gethash "N" *decode*) #\N
      (gethash "O" *decode*) #\O
      (gethash "P" *decode*) #\P
      (gethash "Q" *decode*) #\Q
      (gethash "R" *decode*) #\R
      (gethash "S" *decode*) #\S
      (gethash "T" *decode*) #\T
      (gethash "U" *decode*) #\U
      (gethash "V" *decode*) #\V
      (gethash "W" *decode*) #\W
      (gethash "X" *decode*) #\X
      (gethash "Y" *decode*) #\Y
      (gethash "Z" *decode*) #\Z
      (gethash "a" *decode*) #\a
      (gethash "b" *decode*) #\b
      (gethash "c" *decode*) #\c
      (gethash "d" *decode*) #\d
      (gethash "e" *decode*) #\e
      (gethash "f" *decode*) #\f
      (gethash "g" *decode*) #\g
      (gethash "h" *decode*) #\h
      (gethash "i" *decode*) #\i
      (gethash "j" *decode*) #\j
      (gethash "k" *decode*) #\k
      (gethash "l" *decode*) #\l
      (gethash "m" *decode*) #\m
      (gethash "n" *decode*) #\n
      (gethash "o" *decode*) #\o
      (gethash "p" *decode*) #\p
      (gethash "q" *decode*) #\q
      (gethash "r" *decode*) #\r
      (gethash "s" *decode*) #\s
      (gethash "t" *decode*) #\t
      (gethash "u" *decode*) #\u
      (gethash "v" *decode*) #\v
      (gethash "w" *decode*) #\w
      (gethash "x" *decode*) #\x
      (gethash "y" *decode*) #\y
      (gethash "z" *decode*) #\z
      (gethash "%00" *decode*) (code-char 0)
      (gethash "%01" *decode*) (code-char 1)
      (gethash "%02" *decode*) (code-char 2)
      (gethash "%03" *decode*) (code-char 3)
      (gethash "%04" *decode*) (code-char 4)
      (gethash "%05" *decode*) (code-char 5)
      (gethash "%06" *decode*) (code-char 6)
      (gethash "%07" *decode*) (code-char 7)
      (gethash "%08" *decode*) (code-char 8)
      (gethash "%09" *decode*) #\Tab
      (gethash "%0A" *decode*) #\Newline
      (gethash "%0B" *decode*) (code-char #X0B)
      (gethash "%0C" *decode*) (code-char #X0C)
      (gethash "%0D" *decode*) (code-char #X0D)
      (gethash "%0E" *decode*) (code-char #X0E)
; paused
)

(defun decode-token (str)
  "Decodes an URL-encoded string, returning the original,
un-encoded string.  Does not parse key/value pairs.  Just
translates the encoded special characters into their
originals.  All other characters are copied exactly.  
Returns a new string.  Does not alter the encoded string."
  (let ((decoded (make-array 1 :element-type 'character :fill-pointer 0
			     :adjustable t))
	(strlen (length str))
	(bytes (make-array 2 :element-type 'byte ))
	kanji)
    (do ((i 0 (1+ i)))
	((>= i strlen))
	(cond ((equal (char str i) #\%)
	       ;;
	       ;; We found a hex-encoded character.  We parse the
	       ;; two-digit hex number & use the value to get a
	       ;; character.  We're assuming that CODE-CHAR works
	       ;; on ASCII.  For true portability, we should use
	       ;; a table that maps integers to their ASCII values.
	       ;;
	       (let ((num (parse-integer str :start (1+ i) :end (+ i 3) :radix 16)))
		  ;(format t "~a num:~a if?:~a~%" str num (< num 128))
	          (if (< num 128)
		    (vector-push-extend (code-char num) decoded)
		    (if (null (aref bytes 0))
		      (setf (aref bytes 0) num)
			(progn
			  (setf (aref bytes 1) num)
			  (vector-push-extend (char (ext:convert-string-from-bytes bytes charset:euc-jp) 0) decoded)
			  (if (null kanji) (setf kanji 
						 (ext:convert-string-from-bytes bytes charset:euc-jp))
			    (setf kanji (concatenate 'string kanji 
						       (ext:convert-string-from-bytes bytes charset:euc-jp))))
			  (fill bytes nil)))))
	       (incf i 2))
	      ((equal (char str i) #\+)
	       ;;
	       ;; Space character, encoded as +
	       ;;
	       (vector-push-extend #\space decoded))
	      (t ;; any other character
	       (vector-push-extend (char str i) decoded))))
    kanji
    decoded
    ))

;;----------------------------------------------------------------
; fastcgi に依存している
#+:fastcgi
(defun get-query-string ()
  (decode-token 
    (cond ((string-equal (fastcgi:getenv "REQUEST_METHOD") "POST")
	   (FASTCGI:SLURP-STDIN))
	  (t  ; assume GET data by default
	    (fastcgi:getenv "QUERY_STRING")))))

;----------------------------------------------------------------
(defun only-space-string-p (string)
  (cond ((null string) t)
	 ((not (stringp string)) nil)
	 ((= (length string) 0) t)
	 (t (every #'(lambda (x) (char= #\space x)) string))))

(defmacro old-make-checker (func-name name)
  `(defun ,func-name (h-table)
     (let ((value (gethash ',name h-table)))
       (if (and value
		(not (only-space-string-p value)))
	 '((,func-name t))
	 '((,func-name nil))))))

;; たぶんいいとおもうけど
;; テストしていない。上の関数が実績がある。
(defmacro make-checker (func-name name)
  "name が h-table にあるかどうかを返す関数を返す"
  `(defun ,func-name (h-table)
     (let ((value (gethash ',name h-table)))
       '((,func-name 
	   (and value (not (only-space-string-p value))))))))
;----------------------------------------------------------------
