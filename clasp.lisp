(in-package :cl-clasp)

;;----------------------------------------------------------------
(defparameter *html-local-dir* "")
(defparameter *html-data-dir* "html-data/")
(defparameter *prologue-flag* t)

(defparameter *page-cover-file* "cover")
(defparameter *page-image-ext* '("jpeg" "jpg" "png" "gif" "svg"))
(defparameter *page-delimiter-for-summary* "<!--more-->")

(defparameter *page-props-file* "page.props")

; ライブラリが page の local-dir を知る機構
(defparameter *this-page-props* nil)

; 将来的にはいらないかも
(defparameter *page-local-gen-n* 0)
(defparameter *page-local-dir* nil)

(defparameter *special-package-name-aliases* nil)

;;----------------------------------------------------------------
; props list の :file, :lib 展開用
(defun load-template-file (file-name)
  "html-data-dir から指定されたテンプレートを読む"
  (with-open-file (in (merge-pathnames (concatenate 'string *html-data-dir* file-name)))
    (read in)))

;;----------------------------------------------------------------
(defun is-asp-middle-keyword (key)
  (or (eq key :load) (eq key :file) (eq key :lib) (eq key :data)))

;----------------------------------------------------------------
; *page-props-file* からプロパティを自動生成する
; プロパティリストを返す
; プロパティは 
; features として show-page-properties を有効にすると表示する。
;
; プロパティリストに書かれていなくても自動的 image ファイルリストと 
; cover イメージを生成する
;
; get-dir-list は #'member 使えばよい

(defun page-props-file-to-one-props (ahome-dir path with-summary &optional opt-to-dir)
  (let ((home-dir (string-trim "/" ahome-dir)))
    (labels ((get-dir-list (path)
               (if (or (null path) (string= home-dir (car path))) (cdr path)
                 (get-dir-list (cdr path))))
             (concat-dir (lst delim delim2 filename)
               (concatenate 'string 
                 (reduce #'(lambda (x y) (concatenate 'string x delim y)) lst)
                  delim2 filename)))
  
      ; 毎回作っているのは冗長かも
      (let ((dir-list (get-dir-list (pathname-directory path)))
            ;(file-name (file-namestring path))
            (sym-local-dir (intern "LOCAL-DIR"))
            (sym-date (intern "DATE"))
            (sym-images (intern "IMAGES"))
            ;(sym-list (intern "LIST"))
            (sym-cover (intern "COVER"))
            (sym-main-content (intern "MAIN-CONTENT"))
            (sym-summary (intern "SUMMARY"))
            (sym-layout (intern "LAYOUT")))
  
        (let* ((to-dir (or opt-to-dir (car dir-list)))
               (dir-name (concat-dir dir-list "/" "" ""))
               (to-file-name (concat-dir (cdr dir-list) "/" "." "html"))
               (to-path-name (concatenate 'string to-dir "/" to-file-name))
               (local-props (with-open-file (in path) (read in))))
  
          ; insert *page-local-dir*
          (unless (assoc 'cl-clasp:*page-local-dir* local-props)
             (push `(cl-clasp:*page-local-dir* ,dir-name) local-props))

          ; insert local-dir
          (unless (assoc sym-local-dir local-props)
            (push `(,sym-local-dir ,dir-name) local-props))

          ;(print `(:ahome-dir ,ahome-dir ,home-dir ,dir-name ,to-dir ,dir-list))
          (assert dir-name)
  
          (flet ((make-semi-abs-pathname (file)
                   (assert (or (stringp file) (pathnamep file)))
                   (concatenate 'string dir-name "/" (file-namestring file))))
  
    
            ; insert date as local-time format
            (let* ((created-date-pair (assoc sym-date local-props))
                   (created-date (cadr created-date-pair)))
              (if created-date-pair
                (let ((created-ltime 
                        (funcall 
                          (cond ((stringp created-date) #'local-time:parse-timestring)
                                ((numberp created-date) #'local-time:universal-to-timestamp)
                                (t #'(lambda(x) (error (format t "Fatal error date Format in clasp:~a~%" x))))) created-date)))
                  (setf (cadr created-date-pair) created-ltime))
                (push `(,sym-date ,(local-time:universal-to-timestamp (file-write-date path))) local-props)))
    
            ; 
            #+:p-debug
            (let ((*package* (find-package :common-lisp-user)))
              (dolist (e local-props)
                (print `(:lp ,(car e) ,(symbolp (car e))))))

            ; insert images and cover
            (let (images cover)
              (dolist (file (uiop:directory-files (concatenate 'string ahome-dir "/" dir-name)))
                (let ((file-ext (string-downcase (pathname-type file))))
                  ;(print `(:file ,file ,file-ext ,(find file-ext *page-image-ext* :test #'string=)))
                  (when (find file-ext *page-image-ext* :test #'string=)
                    (push (make-semi-abs-pathname file) images)
                    (if (string= *page-cover-file* (string-downcase (pathname-name file)))
                      (setf cover (make-semi-abs-pathname file))))))
            
              (if images (push `(,sym-images ,(cons :data images)) local-props))
              (if cover (push `(,sym-cover ,cover) local-props)))

            ; create summary
            (if (and with-summary (not (assoc sym-summary local-props)))
              (let* ((main-content-info (assoc sym-main-content local-props))
                     ;(check-info-key (assert (eq (cadr main-content-info) :file)))
                     (main-content-file (caddr main-content-info))
                     (main-content-lst (load-template-file (make-semi-abs-pathname main-content-file)))
                     (summary-text (if main-content-lst (get-summary main-content-lst))))
                ;(print `(:summary ,summary-text))
                (if summary-text (push `(,sym-summary ,summary-text) local-props))))

            
            ; converter semi-abs-pathname from file 
            (let ((sym (gensym)))
            `(,to-path-name ,sym
                         ,(subst sym sym-layout
                            (mapcar #'(lambda (alst)
                               #+:show-page-properties
                               (print `(:alst ,alst))
                               (if (>= (length alst) 3)
                                 (let ((key (cadr alst))
                                       (fname (caddr alst)))
                                   (assert (is-asp-middle-keyword key))
                                   (assert (stringp fname))
                                   (assert (> (length fname) 0))
                                   (setf (caddr alst)
                                         (if (eq (char fname 0) #\/)
                                            (subseq fname 1) 
                                            (make-semi-abs-pathname fname)))))
                               alst ) local-props))))))))))

;;----------------------------------------------------------------
; 再帰的にディレクトリをおりて *page-props-file* からプロパティを自動生成する
; プロパティリストのリストを返す
(defun load-page-props-recursively (dir-name &optional with-summary)
  (flet ((get-page-props-file ()
           (let ((rv)) (cl-fad:walk-directory (concatenate 'string *html-data-dir* dir-name) #'(lambda(f) (push f rv)) :test  #'(lambda (file) (string-equal (file-namestring file) *page-props-file*)) ) rv )))

    ;(print `(:dir ,(concatenate 'string cl-clasp:*html-data-dir* dir-name)))
    ;(print `(:get-config-txt ,(sort (get-config-txt) #'(lambda (a b) (string> (namestring a) (namestring b))))))
    (let ((file-lst (get-page-props-file)))
       (mapcar #'(lambda (afile)
                   (page-props-file-to-one-props *html-data-dir* afile with-summary)) file-lst ))))
;;----------------------------------------------------------------
;; load-local-package と *special-package-name-aliases* 
;; deprecated にすべき。そもそも local package ではない。
;; common lisp のパッケージだが動的にこのアプリ用に作られたものだ
;; load package for clasp
;; 
(defun load-package (file-name &optional nickname)
  (let* ((*html-local-dir*
           (multiple-value-bind (match regs)
             (cl-ppcre:scan-to-strings "(.*)/[^/]*$" file-name)
             (declare (ignorable match))
             (elt regs 0)))
         (package-name (intern (string-upcase (concatenate 'string "CL-CLASP/" *html-local-dir*) ) :keyword))
         (path-name (merge-pathnames (concatenate 'string *html-data-dir* file-name)))
         (my-package (or (or (find-package package-name) (find-package nickname)) (make-package package-name :nicknames (list nickname)))))
    (let ((*package* my-package))
      (use-package :cl)
      (use-package :cl-clasp)
      (load path-name))))

;;----------------------------------------------------------------
;; *special-package-name-aliases* にあるのは名前を変える。
(defun load-local-package (file-name)
  (flet ((rename-by-aliases (name)
           (let ((hit-alias (assoc name *special-package-name-aliases*)))
             (if hit-alias (cdr hit-alias)
               name))))

    (let ((new-package-name (rename-by-aliases (intern (string-upcase *html-local-dir*) :keyword))))

      (if (not (find-package new-package-name))
        (make-package new-package-name))
      (let ((*package* (find-package new-package-name)))
        (use-package 'cl-clasp)
        (load file-name)))))

;;----------------------------------------------------------------
(defun prop-list-to-hash-table (prop-list &optional h)
  "prop-list の形式のものを hash-table に展開する。
   hash-table は指定されなかったら make する。
   prop-list についてはこの ReadMe.txt 説明を書く
   :load は load するだけでテーブルに登録しているが
   #P で始まるパスが設定されるだけである。
   defun などが書いてあることを期待している
   "

  (if (null h) (setf h (make-hash-table)))
  (dolist (x prop-list)
    (let ((len (length x))
	  key value updated-value)
      (setf key nil)
      (cond ((= len 2) (setf key (car x) value 
                             (let ((v (cadr x)))
                                   (if (atom v) v
                                     (if (keywordp (car v)) v
                                       (eval v))))))
            ((= len 3) 
             (let* ((key-word (car x))
                    (op-word (cadr x))
                    (file-name (caddr x))
                    (*html-local-dir* 
                      (multiple-value-bind (match regs)
                        (cl-ppcre:scan-to-strings "(.*)/[^/]*$" file-name)
                        (declare (ignorable match))
                        (elt regs 0))))
               (cond ((eq op-word :file) 
                      (setf key key-word 
                            value (load-template-file file-name)))

                     ((eq op-word :load)
                      (setf key key-word
                            value (load-local-package (merge-pathnames (concatenate 'string *html-data-dir* file-name)))))

                     ((eq op-word :lib)
                      (let ((props (load-template-file file-name)))
                        (prop-list-to-hash-table props h)))

                     ((eq op-word :data) file-name)))))

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
  (let ((gege \"xyz\"))
      (format t \"~s~%\"
	      (asp:make-content '(:div (who:fmt gege)) '((xgege \"xabc\")))))
  これは gege を展開できない。
  (setf gege \"xyz\")
  (format t \"~s~%\"
	  (asp:make-content '(:div (who:fmt gege)) '((xgege \"xabc\"))))
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
	   (append `(who:with-html-output (out nil :prologue "<!DOCTYPE html>")) (list content)))
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
; contents-props と the-file-str で示されるストリングから
; 依存関係のリストを作る。
; prop-list の最初は . で区切っていないが assoc は使える
;
;(card-contents (web-top-make-card-contents "top/card-contents.list"))
; のような記述は積極的に関連すると考える
; 
(defun make-dependency (contents-props the-file-str)
  (let* ((first-prop-item (car contents-props))
         (contents-data-dir (cadr (assoc :contents-data-dir first-prop-item)))
         (created-html-dir (cadr (cl-ppcre:split (namestring (uiop:getcwd)) contents-data-dir)))
         (the-prop (assoc the-file-str (cdr contents-props) :test #'string-equal))
         (prop-list (caddr the-prop))
         files)
         ;(print `(:make-d ,(symbolp (caar first-prop-item)) ,first-prop-item ,created-html-dir ,contents-data-dir))

    (assert created-html-dir)
    (labels ((find-string (str-list &optional result)
               (if (null str-list) result
                 (let ((first-obj (car str-list))
                       (remain-obj (cdr str-list)))
                   (if (atom first-obj)
                     (if (stringp first-obj) (push first-obj result))
                     (find-string first-obj result))
                   (find-string remain-obj result)))))

      (dolist (x prop-list)
        (let ((len (length x))
              (second-keyword (cadr x)))
          (cond ((and (= len 2) 
                      (not (atom second-keyword))
                      (not (keywordp (caadr x))))
                 (find-string second-keyword files)) ; second-keyword is list

                ((and (= len 3)
                      (find second-keyword '(:file :load)))

                 (let ((afile (caddr x))) ; just rename
                   (push afile files))))))
      files)))

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
    #+:p-debug
    (print `(:let-list ,let-list))

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
  (if (and lst (listp lst) hash-table)
    (mapcar #'(lambda (x)
		(let ((key (car x))
		      (value (cadr x)))
		  (setf (gethash key hash-table) value))) lst))
  t)

;名前がよくない
;update もするが、各関数を funcall して、その結果を
;反映させる関数
(defun update-hash-table0 (hash-table func-or-funcs)
  (let ((func-list (if (listp func-or-funcs) func-or-funcs (list func-or-funcs))))
      (mapcar #'(lambda (func) 
		  (if func
		    (merge-list-to-hash-table (funcall func hash-table) hash-table))) func-list)))

(defun update-hash-table (hash-table page-property)
  (let ((funcs (cadddr page-property)))
    (let ((pre-func (car funcs))
	  (check-funcs (cadr funcs))
	  (post-func (caddr funcs)))
#|
      (if pre-func
	(merge-list-to-hash-table (funcall pre-func hash-table) hash-table))
      (if check-funcs
	(setf lst-lst (mapcar #'(lambda (f)
				  (if f (funcall f hash-table))) check-funcs)))
      (mapcar #'(lambda (x) (merge-list-to-hash-table x hash-table)) lst-lst)

      (if post-func
	(merge-list-to-hash-table (funcall post-func hash-table) hash-table))
|#
      (update-hash-table0 hash-table pre-func)	
      (update-hash-table0 hash-table check-funcs)	
      (update-hash-table0 hash-table post-func)	
      hash-table)))

;;----------------------------------------------------------------
(defun get-summary (who-lst)
  (if (listp who-lst)
    (labels ((last-id (lst)
               (let ((last-id-remain (member-if #'keywordp lst)))
                 (if last-id-remain
                   (last-id (cddr last-id-remain))
                   lst)))
             (my-concatenate (lst &optional result)
               ;(print `(:mc ,lst ,result ,(eval `(concatenate 'string ,@(reverse result)))))
               (if (null lst)
                 (eval `(concatenate 'string ,@(nreverse result)))
                 (let ((one (car lst))
                       (my-remain (cdr lst)))
                   ;(print `(:one ,one ,my-remain ,result))
                   (if (stringp one)
                     (my-concatenate my-remain (push one result))
                     (if (and (symbolp one) (string= (symbol-name one) (string-upcase *page-delimiter-for-summary*)))
                         (my-concatenate nil result)
                         (my-concatenate my-remain (push (get-summary one) result))))))))
  
      (let* ((cdr-who-lst (cdr who-lst))
             (now-id (car cdr-who-lst))
             (remain (last-id cdr-who-lst)))
        (if remain
          (my-concatenate remain)
          (if (stringp now-id)
            (eval `(concatenate 'string ,@cdr-who-lst))
            ""))))
  ""))
