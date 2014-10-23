(in-package :cl-clasp)

;----------------------------------------------------------------
; < �� > �� & �� &lt; &gt; &amp; ���Ѵ�����
; ������ʤ�Ǥ��Ψ������������¤���⤵��Ƥ��ʤ�����
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
; �� who ����ƤФ��褦���߷פ���Ƥ���
; �㤨��ŵ��Ū�ʤΤ� pre
; (:pre (load-text-file "test.txt"))
; load-text-file �� html-data-dir ����Υѥ��ˤʤ롣
; property ��Ÿ�����Ƥ��ޤä���˼¹Ԥ���뤿��
; property �ˤϳΤ��� file: dir0/dir1/file.who �ʤɤȤ��ꡢ
; ���� file.who ���� load-text-file ���줿������Х��ɥ쥹�ˤʤäƤۤ���
; ��������dir0/dir1/file.who �Ͻ���ʳ���Ÿ�����졢�ɤ�����Ÿ�����줿����
; ���� load-text-file �˶����뤳�Ȥ��񤷤�

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
; #>-reader ���ޤ��֤���
; 0 �����ꤹ���̵�뤹�롢�����Ȼפ�

(defparameter *pre-line-n* 80)

;;----------------------------------------------------------------
; who ����
; (pre 
; #>PTEXT
; PTEXT)
; ���Τ褦�ˤ��ƻȤ��褦���߷פ���Ƥ���
; ��Ƭ�� # �� lisp ����񤯤��Ȥ��Ǥ���

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

    ; #>PTEXT:% �Τ褦�˺Ǹ夫�飲���ܤ�ʸ���� : �ξ��
    ; pattern �� PTEXT �ˤ��������� second-char ��Ǹ��ʸ��
    ; ���ξ��� % �ˤ��롣
    ; second-char �� lisp �˥��������פ���Ȥ���ͭ��
    ; �������äơ����ΤФ��� #% �ǻϤޤ���Τ� lisp ���Ȥ���
    ; �ǥե���ȤǤ� ## �Ǥ��롣
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
	  ; count ��ã�������ޤ��֤�
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

