(in-package :cl-clasp)

; |#>-reader| に抱けた対応するという中途半端さ
(defparameter *html-escape-mode* t)

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
(defun expand-file-name (file-name)
  (if (char-equal (char file-name 0) #\/)
    (merge-pathnames (concatenate 'string *html-data-dir* file-name))
    (merge-pathnames (concatenate 'string *html-data-dir* *html-local-dir* "/" file-name))))

;----------------------------------------------------------------
(defun expand-load (file-name)
  (load (expand-file-name file-name)))

;----------------------------------------------------------------
; 各 who から呼ばれるように設計されている
; 例えば典型的なのは pre
; (:pre (load-text-file "test.txt"))
; load-text-file は
;       "/" で始まれば html-data-dir からのパスになる。
;       それ以外は相対パス。
; 相対パスの起点は
; property を展開してしまった後に実行されるため :lib などで
; 指定されたディレクトリを相対パスの起点としている
; property には確かに file: dir0/dir1/file.who などとあり、
; *html-local-dir* を参照している

(defun load-text-file (file-name &optional (use-escape-html t))
  (with-open-stream (stream (open (expand-file-name file-name) :direction :input))
    (let (rv)
      (do ((line (read-line stream)
		 (read-line stream nil nil)))
	((null line))
	(setf rv
	      (concatenate 'string (if (null rv) "" rv)
			   (string #\newline)
			   line)))
      (if use-escape-html
        (escape-html-char rv)
        rv))))

;;----------------------------------------------------------------
; #>-reader の折り返し行
; 0 を設定すると無視する、、、と思う
;
; PRE の時は上の pre-line-n で折り返してほしい
; しかし、P の時は newline をいれると IE が余計な空白を入れるので
; newline を push しないようにする。
;
; pre-line-n はしてほしいときとしてほしくない時がある
; pre-line-n は実質的に没
(defparameter *pre-line-n* 256)

;;----------------------------------------------------------------
; (pre 
; #>PTEXT
; PTEXT)
;
; 当初はそうしてたけどカッコがめんどくさいので
; #>PTEXT :PRE 
; PTEXT 
; に変えた
;
; 将来的には
; @pre
; #>PTEXT
; PTEXT 
; にすればよい
;
; who 内で
; #>PTEXT :PRE 
; text
; PTEXT 
;
; 等のようにして使うように設計されている
; 先頭の # で lisp 式を書くことができる

(defun to-symbol (chars)
  (with-input-from-string
    (in-str (coerce chars 'string))
    (read in-str)))

(defun pattern-split (r-char-lst &optional curr lst)
  (if (null r-char-lst) (cons curr lst)
    (let ((top-char (car r-char-lst)))
      (if (char= top-char #\:)
        (pattern-split (cdr r-char-lst) nil (cons curr lst))
        (pattern-split (cdr r-char-lst) (push top-char curr) lst)))))

(defun html-escape (r-char-list pre-line-n n &optional result)
  (if (null r-char-list) result
    (let ((top (car r-char-list))
          (remain (cdr r-char-list)))
      (case top
        (:newline
          (html-escape remain pre-line-n pre-line-n
                       (cons #\newline
                             (nconc
                               (make-list (- pre-line-n n) :initial-element #\space)
                               result))))
        (#\newline
          (html-escape remain pre-line-n pre-line-n
                       (cons #\newline result)))
        (t (html-escape remain pre-line-n (- n 1) (cons top result)))))))

(defun |#>-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars alist pattern (count 0) (second-char #\#) pre-mode 
              (not-escape-mode (not *html-escape-mode*))
              (pre-line-n *pre-line-n*))
    (do ((curr (read-char stream)
	       (read-char stream)))
      ((char= #\newline curr))
      (if (char= #\space curr)
	(progn 
	  (if pattern
	    (push (to-symbol (nreverse chars)) alist)
	    (setf pattern chars))
	  (setf chars nil))
	(push curr chars)))

    (if pattern
      (push (to-symbol (nreverse chars)) alist)
      (setf pattern chars))

    ; alist に pattern 以降のリストが入る
    ; #>PTEXT :P :style "nantoka"
    ; alist は ("nantoka" :style :P)
    ; pattern は (T X E T P)
    ; どうせ後でひっくり返すので
    ; pattern は逆のまま

    (let ((first-mode (car (last alist))))
      ;(print `(:first-mode ,first-mode))
      (case first-mode 
        (:pre (setf pre-mode alist))
        (:not-escape (setf 
                       not-escape-mode t
                       alist (nreverse (cdr (nreverse alist) ))))))
                                 ;(setf (last alist) nil)
    (if (eq (last alist) :not-escape) (setf not-escape t))
    ;(princ `(:last ,(last alist) ,pre-mode))

    ; #>PTEXT:% のように最後から２番目の文字が : の場合
    ; pattern を PTEXT にしたうえで second-char を最後の文字
    ; この場合は % にする。
    ; second-char は lisp にエスケープするときに有効
    ; したがって、このばあい #% で始まる場合のみ lisp 式とする
    ; デフォルトでは ## である。
    #|
    (let ((len (length pattern)))
      (if (char= #\: (elt pattern (- len 2)))
	(setf second-char (elt pattern (- len 1))
	      pattern (subseq pattern 0 (- (length pattern ) 2)))))
    |#

    (let ((pattern-list (pattern-split pattern)))
      (setf pattern (car pattern-list))
      (let ((second-char-pattern (cadr pattern-list))
            (pre-line-n-pattern (caddr pattern-list)))
        (if second-char-pattern
          (setf second-char (car second-char-pattern)))
        (if pre-line-n-pattern
          (setf pre-line-n (to-symbol (concatenate 'string pre-line-n-pattern))))))

    ;(princ `(,pattern ,second-char ,pre-line-n))

    (let ((pointer pattern) output check-second skip-backslash-escape)
      (do ((curr (read-char stream)
		 (read-char stream)))
	((null pointer))
	(cond ((char= #\\ curr)
         (setf skip-backslash-escape t))
        (skip-backslash-escape
          (progn
           (setf skip-backslash-escape nil)
           (push curr output)))
        (not-escape-mode
          (push curr output))
        ((char= #\< curr) (progn
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
	      ((and (= 0 count) (char= #\# curr)) 
				  (setf check-second t))
          ((char= #\newline curr)
                  (if pre-mode (push curr output)))
	      (t (push curr output)))

	(if (and pre-mode (not (char= #\# curr)))
	  (incf count))

    (if (char= #\newline curr)
      (setf count 0)
      ; count に達したら折り返す :newline マーカーを入れる
      (when (= count pre-line-n)
        (setf count 0)
        (push :newline output)))

	(setf pointer
	      (if (char= (car pointer) curr)
		(cdr pointer)
		pattern))
	(if (null pointer)
	  (return)))
      (let ((last-string
	     (coerce (html-escape 
                   (nthcdr (length pattern) output) 
                   pre-line-n pre-line-n) 'string)))

	(if alist (list 'quote (nreverse (push last-string alist)))
	  last-string)))))

;----------------------------------------------------------------
(defun use-sharp->-macro ()
	(set-dispatch-macro-character
	  #\# #\> #'|#>-reader|))

