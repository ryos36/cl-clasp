(in-package :cl-clasp)

;;----------------------------------------------------------------
(defparameter *html-data-dir* "html-data/")
(defparameter *prologue-flag* nil)

;;----------------------------------------------------------------
; props list �� :file, :lib Ÿ����
(defun load-template-file (file-name)
  "html-data-dir ������ꤵ�줿�ƥ�ץ졼�Ȥ��ɤ�"

  (with-open-file (in (merge-pathnames (concatenate 'string *html-data-dir* file-name)))
    (read in)))

#|
�Ť��С������
̾����Ū���֤��Ƥ���
(defun make-key-quote-value (one-prop)
  "Ϳ����줿 '(key value) ���� quote ���줿�ͤ�Ȥ�"

	(let ((key (car one-prop))
	      (value (cadr one-prop)))
	  (list key (list 'quote value))))

(defun make-key-quote-value (one-prop)
  "Ϳ����줿 '(key value) ���� quote ���줿�ͤ�Ȥ롣
   ������ɤˤ�����ɬ�פʤ��ʤä��ؿ�"

        (let ((key (car one-prop))
              (value (cadr one-prop))
              (template-rv (copy-list '(key 'value))))
          (setf (car template-rv) key)
          (setf (cadadr template-rv) value)
          template-rv))
|#

;;----------------------------------------------------------------
(defun prop-list-to-hash-table (prop-list &optional h)
  "prop-list �η����Τ�Τ� hash-table ��Ÿ�����롣
   hash-table �ϻ��ꤵ��ʤ��ä��� make ���롣
   prop-list �ˤĤ��ƤϤ��� ReadMe.txt �������"

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

; �Τ��� let �� eval ���䤹���褦�� quote �򤤤�Ƥ��롣
; ���Ȥ��� ((a (:h1 "title"))) �� let �μ���Ĥ����
; (let ((a (:h1 "title"))) .... ) �����ɾ�������
; (:h1 "title") ��ɾ�����뤳�Ȥˤʤꥨ�顼
; (let ((a '(:h1 "title"))) .... ) �����ɾ�������
; �Ȥ��������ꤿ����

(defun hash-table-to-let-list (h)
  (let ((acc nil))
    (maphash #'(lambda (key value)
		 (push
		   (list key (list 'quote value)) acc)) h)
    acc))

; string ���� symbol ��Ĥ��롣��äȴ�ñ����ˡ�����ꤽ������
; ext:string-concat ���Ĥ����ʤ��ä��Τ� concatenate ��Ȥ�
; (ext:string-concat "a" "b") ���ä�
; (concatenate 'string "a" "b") �Ȥ���Τ��ڡ�
; ���ԡ���Ū�ˤ� ext:string-concat �������ᤤ
; GC �ϡ�concatenate �����־��ʤ�
; format ���٤������� GC �����¿���ΤǻȤ�ʤ����Ȥˤ���
; (clisp ��)
(defun string-to-keyword (str)
  (with-input-from-string (in (concatenate 'string ":" str)) (read in)))

(defun string-to-symbol (str)
  (with-input-from-string (in str) (read in)))

; ñ��� value ���֤��ΤǤϤʤ�
; key �� status �Ǥ��ä��Ȥ��˥�����ɤ��֤�
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
  "eval-to-who �Υإ�ѡ��ؿ���
   symbol-content �� args �δĶ���Ȥä�ɾ�����롣
   symbol-content �� lisp �Ǥ��뤳�Ȥ�����ˤ��Ƥ��롣
   �Ĥޤ� ����ܥ뤫 (func ...) �η���"

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

; �ʤ�Ȥʤ��������μ�Τ� macro ��Ȥä�������ä�
; ���ʤ��˽񤱤뵤�����롣
(defun eval-to-who (content &optional args)
  "content ���褿������ɾ���� cl-who �����Υǡ������֤�
   content �� cl-who �������ꥹ�׼���args �˴Ķ�������Ƥ�����
   ������Ȥ�ɾ�����롣������ɾ�����줿���ʤ����(�ٱ�ɾ����)
   �� (:lisp-symbol symbol) �Ƚ񤤤Ƥ����� symbol ��Ÿ������롣"

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
;make-content-string2 �ʤɤ����뤱�ɡ�����
;contact-us.fcgi �ǤϤ����ǽ�Ū�˻ȤäƤ���
;defmacro ��Ȥä������褤��
;nconvert-page-property ��ͳ�� eval-to-who ��Ĥ��äƤ��롣
(defun make-content (content &optional args)
  "cl-who ������ content ��Ÿ������ HTML �ˤ��롣
  ���ΤȤ� args ��Ķ��Ȥ��ƻȤ���
  �Ķ���Ȥ�ʤä�ɾ��������ҤˤǤ��ʤ��ΤǶ��ϤǤϤʤ�
  �㤨��
  (let ((gege "xyz"))
      (format t "~s~%"
	      (asp:make-content '(:div (who:fmt gege)) '((xgege "xabc")))))
  ����� gege ��Ÿ���Ǥ��ʤ���
  (setf gege "xyz")
  (format t "~s~%"
	  (asp:make-content '(:div (who:fmt gege)) '((xgege "xabc"))))
  �ʤ������

  (:h2 lisp-symbol)
  �Ȥ��ä��顢lisp-symbol �� args �ˤ������äơ��֤������뤳�ȤϽ���롣
  �֤���������̤�
  (:h2 (:a :href url new-lisp-symbol)) ���ä���
  �����Ϥ������� url �� new-lisp-symbol ���֤�������ɾ�����Ƥۤ�����
  �������ޤǤǤ��ʤ��� �����褦�ˡ�eval-to-who ��ȯ������
  eval-to-who �� who�������� who������Ÿ������
  eval-to-who �� who������Ÿ�����Ʒ�̤� who ����������
  ��̤Ȥ���¿�ʤ� lisp-symbol ���ʤ��ʤ롣
  �����ǡ�make-content ������Ѵ��ǻȤ��Ф褤��"

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

; ���� hash-table���ˤ��Ƥ��� let-list �ˤ��Ƥ���
; ����Ͼ�Ĺ���������쥯�Ȥ��Ѵ�����ؿ�����٤���
;
; ��Ū�� who ���� who �Ѵ��� page-property �˽��ᤷ�Ƥ��롣
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

;̾�����褯�ʤ�
;update �⤹�뤬���ƴؿ��� funcall ���ơ����η�̤�
;ȿ�Ǥ�����ؿ�
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

