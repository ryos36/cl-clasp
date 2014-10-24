(in-package :cl-clasp)

;----------------------------------------------------------------
; < と > と & を &lt; &gt; &amp; に変換する
; いくらなんでも効率悪いだろう。構造化もされていないし。
(defun escape-html-char (str)
  (cl-ppcre:regex-replace-all
    "'"
    (cl-ppcre:regex-replace-all
      "\""
      (cl-ppcre:regex-replace-all
	"<"
	(cl-ppcre:regex-replace-all
	  ">" 
	  (cl-ppcre:regex-replace-all
	    "&" str "&amp;")
	  "&gt;" )
	"&lt;" )
      "&quot;")
    "&apos;"))

;----------------------------------------------------------------
; 各 who から呼ばれるように設計されている
; 例えば典型的なのは pre
; (:pre (load-text-file "test.txt"))
; load-text-file は html-data-dir からのパスになる。
; property を展開してしまった後に実行されるため
; property には確かに file: dir0/dir1/file.who などとあり、
; その file.who から load-text-file された場合相対アドレスになってほしい
; しかし、dir0/dir1/file.who は初期段階で展開され、どこから展開されたかを
; この load-text-file に教えることは難しい

(defun load-text-file (file-name)
  (with-open-stream (stream (open (merge-pathnames (concatenate 'string *html-data-dir* file-name)) :direction :input))
    (let (rv)
      (do ((line (read-line stream)
		 (read-line stream nil nil)))
	((null line))
	(setf rv
	      (concatenate 'string (if (null rv) "" rv)
			   (string #\newline)
			   line)))
      (escape-html-char rv))))

;;----------------------------------------------------------------
; #>-reader の折り返し行
; 0 を設定すると無視する、、、と思う

(defparameter *pre-line-n* 80)

;;----------------------------------------------------------------
; who から
; (pre 
; #>PTEXT
; PTEXT)
; 等のようにして使うように設計されている
; 先頭の # で lisp 式を書くことができる

(defun to-symbol (chars)
  (with-input-from-string
    (in-str (coerce chars 'string))
    (read in-str)))

(defun |#>-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars alist pattern (count 0) (second-char #\#))
    (do ((curr (read-char stream)
	       (read-char stream)))
      ((char= #\newline curr))
      (if (char= #\space curr)
	(progn 
	  (if pattern
	    (push (to-symbol (nreverse chars)) alist)
	    (setf pattern (nreverse chars)))
	  (setf chars nil))
	(push curr chars)))

    (if pattern
      (push (to-symbol (nreverse chars)) alist)
      (setf pattern (nreverse chars)))

    ; #>PTEXT:% のように最後から２番目の文字が : の場合
    ; pattern を PTEXT にしたうえで second-char を最後の文字
    ; この場合は % にする。
    ; second-char は lisp にエスケープするときに有効
    ; したがって、このばあい #% で始まる場合のみ lisp 式とする
    ; デフォルトでは ## である。
    (let ((len (length pattern)))
      (if (char= #\: (elt pattern (- len 2)))
	(setf second-char (elt pattern (- len 1))
	      pattern (subseq pattern 0 (- (length pattern ) 2)))))

    (let ((pointer pattern) output check-second)
      (do ((curr (read-char stream)
		 (read-char stream)))
	((null pointer))
	(cond ((char= #\< curr) (progn
				  (push #\& output)
				  (push #\l output)
				  (push #\t output)
				  (push #\; output)))
	      ((char= #\> curr) (progn
				  (push #\& output)
				  (push #\g output)
				  (push #\t output)
				  (push #\; output)))
	      ((char= #\& curr) (progn
				  (push #\& output)
				  (push #\a output)
				  (push #\m output)
				  (push #\p output)
				  (push #\; output)))
	      ((char= #\' curr) (progn
				  (push #\& output)
				  (push #\a output)
				  (push #\p output)
				  (push #\o output)
				  (push #\s output)
				  (push #\; output)))
	      ((char= #\" curr) (progn
				  (push #\& output)
				  (push #\q output)
				  (push #\u output)
				  (push #\o output)
				  (push #\t output)
				  (push #\; output)))
	      ((and (= 0 count) (char= #\# curr)) 
				  (setf check-second t))
	      (check-second (progn
				(setf check-second nil)
				(if (char= second-char curr)
				  (progn 
					  (push (coerce (nreverse output) 'string) alist)
					  (setf output nil)
					  (push (read stream) alist))
				  (progn 
					  (push #\# output)
					  (push curr output)))))
	      (t (push curr output)))

	(if (not (char= #\# curr))
	  (incf count))

	(if (char= #\newline curr)
	  (setf count 0)
	  ; count に達したら折り返す
	  (when (= count *pre-line-n*)
	    (setf count 0)
	    (push #\newline output)))

	(setf pointer
	      (if (char= (car pointer) curr)
		(cdr pointer)
		pattern))
	(if (null pointer)
	  (return)))
      (let ((last-string
	     (coerce (nreverse
		       (nthcdr (length pattern) output))
		     'string)))
	(if alist (list 'quote (nreverse (push last-string alist)))
	  last-string)))))

;----------------------------------------------------------------
(defun use-sharp->-macro ()
	(set-dispatch-macro-character
	  #\# #\> #'|#>-reader|))

