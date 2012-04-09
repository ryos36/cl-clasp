(in-package :cl-clasp)

; �ǥХå��Ѥ��ѿ�
; eval-html-string �� nil �ˤ���� 
; |#<-reader| �� who �����ؤ��Ѵ��򤷤ʤ�
(defparameter *eval-html-string* t)

(defun atom-filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (if (atom x)
        (let ((val (funcall fn x)))
          (if val (push val acc)))
        (push (atom-filter fn x) acc)))
    (nreverse acc)))

(defun only-space-string (string)
  (if (not (and (stringp string)
                (every #'(lambda (x) (char= #\space x)) string)))
    string))

(defun ignore-space (lst)
        (atom-filter #'only-space-string lst))

;#Paul's filter
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))


; #< �ǤϤ��ޤ� ># �Ǥ���� html �� who �������Ѵ����롣
; JavaScript �� ccs �� "  " ������Ȳ��ϤǤ��ʤ��Τ����

(defun |#<-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((left-k #\() (right-k #\)) (ryos #\r)
        chars (status 'init) pre-pre-char pre-char one-char)
    (do ((curr (read-char stream) pre-fetch-curr)
         (pre-fetch-curr (read-char stream) (read-char stream)))
      ((and (char= curr #\>) (char= pre-fetch-curr #\#)))

      (setf one-char curr)
      ;(format t "~%pre-fetch-curr:<~a>~%" one-char)
#|
�������ܤǻ����ʤࡣ������֤� 'init
�ǽ�Ū�� cond ���̲ᤷ�����
one-char �������å����Ѥޤ�Ƥ���
one-char �θ����ͤϺ��ɤ���ͤǤ��� curr �ˤʤ�
one-char �� nil �ʤ�ʤˤ�Ĥޤ�ʤ�

< ��ȯ�������� 'tag-fetch �ˤʤ롣one-char �� nil
��������ʤ��ä��� 'reading

'reading ���֤��� one-char �Ϥ��ΤޤޤʤΤ��Ѥޤ��
�Х�ȯ�����ǽ�� 'reading ���֤˴٤�褦�ʤ�Τ� " ���Ĥޤ�ʤ���

'tag-fetch �Ǥι�θ�� / �� space �Ȥ��Τۤ�

/ �� >(right-k����Ϥ���) 'tag-end ��
space �ϥ����åפ��� (<   pre �ʤɤ� < �� pre �δ֤Υ����åפι�θ)
	tag ̾�Υ����åפ��ü�ʤΤ� 'tag-start-space-skip
	(XML �λ��;�⤷�������� <  pre �Ȥ��Ϥ��ꤨ�ʤ����⤷��ʤ���
	     ������礷�Ƥ���)

���Τۤ��ξ��� ̾������Ф��뤿��
	'tag-start-reading

'tag-start-reading �Ǥ�
	space ������Ф��Τۤ��θ�꡼�ӥ塼�Ȥ��ɤ� 'tag-reading
	> �Ǥ����� 'init ����

���⤬����Ⱦü������
|#
      (cond ((eq status 'init)
             (cond ((char= one-char #\<)
                    (setf status 'tag-fetch
                          one-char nil))
                   (t (setf status 'reading
                            pre-pre-char #\space
                            pre-char #\"))))
            ((eq status 'reading)
             (cond ((char= one-char #\<)
                    (setf status 'tag-fetch
                          one-char #\"))))
            ((eq status 'tag-fetch)
             (cond ((char= one-char #\/)
                    (setf status 'tag-end
                          one-char right-k ))
                   ((char= one-char #\space)
                    (setf status 'tag-start-space-skip
                            pre-char left-k
                            one-char #\: ))
                   (t (setf status 'tag-start-reading
                            pre-pre-char left-k
                            pre-char #\: ))))
            #|
            ((eq status 'tag-start)
             (cond ((char= one-char #\>)
                    (setf status 'init
                          one-char #\space))
                   (t (setf status 'tag-start-reading))))
            |#
            ((eq status 'tag-start-space-skip)
             (cond ((char= one-char #\space )
                    (setf status 'tag-start-space-skip
                          one-char nil))
                   (t (setf status 'tag-start-reading))))
            ((eq status 'tag-start-reading)
             (cond ((char= one-char #\>)
                    (setf status 'init
                          one-char nil))
                   ((char= one-char #\space)
                    (setf status 'tag-reading))))
            ((eq status 'tag-reading)
             (cond ((char= one-char #\>)
                    (setf status 'init
                          one-char nil))
                   ((char= one-char #\=)
                    (setf one-char nil))
                   ((char= one-char #\")
                    (setf status 'tag-reading-in-string))
                   ((char= one-char #\/)
                    (setf status 'tag-reading-end-end
                          one-char right-k ))
                   ((not (char= one-char #\space))
                    (setf status 'tag-reading-attr
                          pre-char #\:))))
            ((eq status 'tag-reading-attr)
             (cond ((char= one-char #\>)
                    (setf status 'init
                          one-char nil))
                   ((char= one-char #\=)
                    (setf status 'tag-reading
                          one-char #\space))
                   ((char= one-char #\space)
                    (setf status 'tag-reading))))
            ((eq status 'tag-reading-in-string)
             (cond ((char= one-char #\")
                    (setf status 'tag-reading))))
            ((eq status 'tag-reading-end-end)
             (setf status 'init
                   one-char nil))
            ((eq status 'tag-end)
             (setf status 'tag-end-reading
                   one-char nil))
            ((eq status 'tag-end-reading)
             (cond ((char= one-char #\>)
                    (setf status 'init
                          one-char nil))
                   (t (setf one-char nil)))))
      ;(print "--")
      ;(print curr)
      ;(print one-char)
      ;(print status)

      (if pre-pre-char (push pre-pre-char chars))
      (setf pre-pre-char nil)

      (if pre-char (push pre-char chars))
      (setf pre-char nil)

      (if (and one-char (not (char= one-char #\newline)))
               (push one-char chars)))

    (if *eval-html-string*
      (with-input-from-string
        (in-str (concatenate 'string "'(" (coerce (nreverse chars) 'string) ")"))
        (read in-str))
      (coerce (nreverse chars) 'string))))

(defun use-sharp-<-macro ()
	(set-dispatch-macro-character
	  #\# #\< #'|#<-reader|))

;----------------------------------------------------------------
; ignore-space �ѤΥإ�ѡ�
(defun atom-filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (if (atom x)
        (let ((val (funcall fn x)))
          (if val (push val acc)))
        (push (atom-filter fn x) acc)))
    (nreverse acc)))

;----------------------------------------------------------------
(defun only-space-string (string)
  (if (not (and (stringp string)
                (every #'(lambda (x) (char= #\space x)) string)))
    string))

;----------------------------------------------------------------
; |#<-reader| ��;�פ� "" (����ƥ�Ĥʤ��Υ��ȥ��)
; ����������ΤǤ��������ؿ�

(defun ignore-space (lst)
        (atom-filter #'only-space-string lst))

;----------------------------------------------------------------
;
; ���ꤵ�줿 file-name ���Ǥ� html �� who �������Ѵ�
; ����� <div> </div> ��Ĥ���褦�ˤ���
; ��Ĺ�ʤ��Ȥ⤢��Τ����
; �ޤ������Υѡ������ϴ����ǤϤʤ�
; �ä� pre ��ɾ����style �����Ȥ� java script �� " �������äƤ�����
;
(defun load-html-as-who (file-name)
  (let (html-str
	rv)
    (with-open-stream (stream (open file-name :direction :input))
		       (do ((curr (read-line stream)
				  (read-line stream nil nil)))
			 ((null curr))
			 (setf html-str
			       (concatenate 'string (if (null html-str) "" html-str)
					      (string #\newline)
					      curr))))
    (setf rv 
	  (with-input-from-string 
	    (in (concatenate 'string "#<<div>" html-str "</div>>#"))
	    (read in)))
    (ignore-space rv)))

