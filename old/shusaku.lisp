
;deprected...
(defun make-content-string-simple (content)
  "���
   content ��ɾ������ who:with-html-output �ʤɤ��ɲä���
   �ǽ�Ū��ɾ�����롣
   setf ��Ȥ��Ф������Ȥ˸�ǵ����դ�����
   �ǡ���ɤ���äƥޥ����Ĥ�������ʤ��ȡ�
   ������ǵ����դ�����"

  (let* ((eval-content0
          (append '(who:with-html-output (out)) (list content)))
         (eval-content
          (append '(with-output-to-string (out)) (list eval-content0))))
    (eval eval-content)))

;deprected...
(defmacro make-content-string (content &body body)
  "Ʊ���褦�� defmacro ��Ĥ��ä���setf ��Ȥ��Ф����Τ�
   ���뤤�Ϥ�äȥޥ���򤦤ޤ��Ȥ��Ф����Τ���
   ����ˤ��ƤϽ��褬����"
  `(let* ((eval-content00
            (append '(let) (quote ,(if (null body) '(()) body))))
          (eval-content0
            (append '(who:with-html-output (out)) (list ,content)))
          (eval-content
            (append '(with-output-to-string (out)) (list eval-content0)))
          (eval-content-X
            (append eval-content00 (list eval-content))))
    (eval eval-content-X)))

;deprected...
(defun make-content-string2 (content &optional args)
  "�������褬������"
  (let* ((eval-content0
	   (append '(let) (list args)))
	 (eval-content1
	   (append '(who:with-html-output (out)) (list content)))
	 (eval-content2
	   (append '(with-output-to-string (out)) (list eval-content1)))
	 (eval-content3
	   (append eval-content0 (list eval-content2))))

    #+:p-debug
    (progn
      (format t "~s~%" eval-content0)
      (format t "~s~%" eval-content1)
      (format t "~s~%" eval-content2)
      (format t "~s~%" eval-content3))
    (eval eval-content3)))
