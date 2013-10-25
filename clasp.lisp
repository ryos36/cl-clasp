(in-package :cl-clasp)

;;----------------------------------------------------------------
(defparameter *html-data-dir* "html-data/")
(defparameter *prologue-flag* nil)

;;----------------------------------------------------------------
; props list の :file, :lib 展開用
(defun load-template-file (file-name)
  "html-data-dir から指定されたテンプレートを読む"

  (with-open-file (in (merge-pathnames (concatenate 'string *html-data-dir* file-name)))
    (read in)))

#|
古いバージョン
名残雪的に置いてある
(defun make-key-quote-value (one-prop)
  "与えられた '(key value) から quote された値をとる"

	(let ((key (car one-prop))
	      (value (cadr one-prop)))
	  (list key (list 'quote value))))

(defun make-key-quote-value (one-prop)
  "与えられた '(key value) から quote された値をとる。
   キーワードにしたら必要なくなった関数"

        (let ((key (car one-prop))
              (value (cadr one-prop))
              (template-rv (copy-list '(key 'value))))
          (setf (car template-rv) key)
          (setf (cadadr template-rv) value)
          template-rv))
|#

;;----------------------------------------------------------------
(defun prop-list-to-hash-table (prop-list &optional h)
  "prop-list の形式のものを hash-table に展開する。
   hash-table は指定されなかったら make する。
   prop-list についてはこの ReadMe.txt 説明を書く"

  (if (null h) (setf h (make-hash-table)))
  (dolist (x prop-list)
    (let ((len (length x))
	  key value updated-value)
      (setf key nil)
      (cond ((= len 2) (setf key (car x) value (cadr x)))
	    ((= len 3) (cond ((eq (cadr x) :file) 
			      (setf key (car x) value (load-template-file (caddr x))))
			     ((eq (cadr x) :lib)
			      (let ((props (load-template-file (caddr x))))
				(prop-list-to-hash-table props h))))))

      (setf updated-value
	     (cond ((symbolp value) (eval value))
		   ((listp value)
		    (let ((first-one (car value)))
		      (if (not (keywordp first-one)) (eval value) value)))
		   (t value)))

      (if key
	(setf (gethash key h) updated-value))))
  h)

; のちに let で eval しやすいように quote をいれている。
; たとえば ((a (:h1 "title"))) で let の式をつくると
; (let ((a (:h1 "title"))) .... ) これを評価すると
; (:h1 "title") を評価することになりエラー
; (let ((a '(:h1 "title"))) .... ) これを評価すると
; という式を作りたい。

(defun hash-table-to-let-list (h)
  (let ((acc nil))
    (maphash #'(lambda (key value)
		 (push
		   (list key (list 'quote value)) acc)) h)
    acc))

; string から symbol をつくる。もっと簡単な方法がありそうだが
; ext:string-concat がつかえなかったので concatenate を使う
; (ext:string-concat "a" "b") だった
; (concatenate 'string "a" "b") とするのが筋？
; スピード的には ext:string-concat が一番早い
; GC は　concatenate が一番少ない
; format は遅すぎる上に GC 回数も多いので使わないことにした
; (clisp 上)
(defun string-to-keyword (str)
  (with-input-from-string (in (concatenate 'string ":" str)) (read in)))

(defun string-to-symbol (str)
  (with-input-from-string (in str) (read in)))

; 単純に value を返すのではなく
; key が status であったときにキーワードを返す
(defun key-value-converter (key value-str &key (converter #'identity))
  (if (eq key 'cl-user::status) (string-to-keyword value-str)
    (funcall converter value-str)))

(defun query-string-to-hash-table (query-string &optional h &key (converter #'identity))
  (if (null h) (setf h (make-hash-table)))
  (let ((value-pair-str (cl-ppcre:split "&" query-string)))
    (if value-pair-str
      (mapcar #'(lambda (x)
		  (let* ((key-value-str (cl-ppcre:split "=" x))
			 (key-str (car key-value-str))
			 (key (string-to-symbol key-str))
			 (value-str (key-value-converter key (cadr key-value-str) :converter converter)))
		    (setf (gethash key h) value-str)))
	      value-pair-str)))
  h)

(defun eval-to-one-who (symbol-content &optional args)
  "eval-to-who のヘルパー関数。
   symbol-content を args の環境を使って評価する。
   symbol-content は lisp であることを前提にしている。
   つまり シンボルか (func ...) の形式"

  (let* ((eval-content0
	   (append '(let) (list args)))
	 (eval-content1
	   (append eval-content0 (list symbol-content))))
    #+:p-debug
    (progn
      #+:p-debug
      (format t "eval-to-one-who:0:~s~%" eval-content0)
      #+:p-debug
      (format t "eval-to-one-who:1:~s~%" eval-content1))
    (eval eval-content1)))

; なんとなくだがこの手のは macro を使った方がもっと
; すなおに書ける気がする。
(defun eval-to-who (content &optional args)
  "content で来た引数を評価し cl-who 形式のデータを返す
   content は cl-who 形式かリスプ式。args に環境を入れておくと
   それをもとに評価する。ここで評価されたくないもの(遅延評価？)
   は (:lisp-symbol symbol) と書いておくと symbol に展開される。"

  #+:p-debug
  (format t "eval-to-who:~a~%" content)
  (cond ((keywordp content) content)
	((symbolp content) (eval-to-who (eval-to-one-who content args) args))
	((atom content) content)
	(t (let ((top-one (car content)))
	     #+:p-debug
	     (format t "eval-to-who:top-one:~a ~a~%" top-one
		     (eq top-one :lisp-symbol))
	     (cond ((eq top-one :lisp-symbol)
		    (cadr content))
		   ((eq (symbol-package top-one) (symbol-package 'cl-who:str))
		    content)
		   ((and (symbolp top-one) (not (keywordp top-one)))
		    (eval-to-who (eval-to-one-who content args) args))
		   (t 
		     (let ((acc nil))
		       (dolist (x content)
			 (let ((val (eval-to-who x args)))
			   (if val (push val acc))))
		       (nreverse acc))))))))

;make-content-string
;make-content-string2 などがあるけど、、、
;contact-us.fcgi ではこれを最終的に使っている
;defmacro を使った方がよい？
;nconvert-page-property 経由で eval-to-who をつかっている。
(defun make-content (content &optional args)
  "cl-who 形式の content を展開して HTML にする。
  そのとき args を環境として使う。
  環境をともなった評価を入れ子にできないので協力ではない
  例えば
  (let ((gege "xyz"))
      (format t "~s~%"
	      (asp:make-content '(:div (who:fmt gege)) '((xgege "xabc")))))
  これは gege を展開できない。
  (setf gege "xyz")
  (format t "~s~%"
	  (asp:make-content '(:div (who:fmt gege)) '((xgege "xabc"))))
  なら大丈夫

  (:h2 lisp-symbol)
  とあったら、lisp-symbol を args にしたがって、置き換えることは出来る。
  置き換えた結果が
  (:h2 (:a :href url new-lisp-symbol)) だったら
  本当はそこから url や new-lisp-symbol も置き換えて評価してほしい。
  がそこまでできない。 出来るように　eval-to-who を開発した。
  eval-to-who は who形式から who形式に展開する
  eval-to-who で who形式を展開して結果は who 形式だが、
  結果として多段の lisp-symbol がなくなる。
  そこで、make-content を一段変換で使えばよい。"

  (let* ((new-args
	   (if (hash-table-p args) (hash-table-to-let-list args)
	     args))
	 (eval-content0
	   (append '(let) (list new-args)))
	 (eval-content1
	   (append `(who:with-html-output (out nil :prologue ,*prologue-flag*)) (list content)))
	 (eval-content2
	   (append '(with-output-to-string (out)) (list eval-content1)))
	 (eval-content3
	   (append eval-content0 (list eval-content2))))
    #+:p-debug
    (progn
      #+:p-debug
      (format t "~s~%" eval-content0)
      #+:p-debug
      (format t "~s~%" eval-content1)
      #+:p-debug
      (format t "~s~%" eval-content2)
      #+:p-debug
      (format t ">>>~s<<<~%" eval-content3))
    (eval eval-content3)))

;----------------------------------------------------------------
(defun get-page-property (status page-property-list)
  (find status page-property-list :key #'first))

(defun page-property-is-already-converted (page-property)
  (null (caddr page-property)))

; 一度 hash-table　にしてから let-list にしている
; これは冗長だ。ダイレクトに変換する関数を作るべき。
;
; 静的に who から who 変換をし page-property に書き戻している。
(defun nconvert-page-property (page-property)
  (let ((main-content (car page-property))
	(let-list (asp:hash-table-to-let-list (asp:prop-list-to-hash-table (cadr page-property)))))
    #+:p-debug
    (format t "main-content:~s~%let-list:~s~%" main-content let-list)
    (let ((converted-content 
	    (asp:eval-to-who main-content let-list)))
      (setf (car page-property) converted-content)
      (setf (cadr page-property) nil)
      converted-content)))

(defun add-property (page-property property)
  (if (null property) page-property
    (list (car page-property) 
	  (append
	    (cadr page-property)
	    (list property)))))

(defun merge-list-to-hash-table (lst hash-table)
  (if (and lst hash-table)
    (mapcar #'(lambda (x)
		(let ((key (car x))
		      (value (cadr x)))
		  (setf (gethash key hash-table) value))) lst))
  t)

;名前がよくない
;update もするが、各関数を funcall して、その結果を
;反映させる関数
(defun update-hash-table (hash-table page-property)
  (let ((funcs (cadddr page-property)))
    (let ((pre-func (car funcs))
	  (check-funcs (cadr funcs))
	  (post-func (caddr funcs))
	  lst-lst)
      (if pre-func
	(merge-list-to-hash-table (funcall pre-func hash-table) hash-table))
      (if check-funcs
	(setf lst-lst (mapcar #'(lambda (f)
				  (if f (funcall f hash-table))) check-funcs)))
      (mapcar #'(lambda (x) (merge-list-to-hash-table x hash-table)) lst-lst)

      (if post-func
	(merge-list-to-hash-table (funcall post-func hash-table) hash-table))
      hash-table)))

