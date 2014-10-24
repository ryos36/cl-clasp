
;deprected...
(defun make-content-string-simple (content)
  "習作。
   content を評価して who:with-html-output などを追加して
   最終的に評価する。
   setf を使えばいいことに後で気が付いた。
   で、結局これってマクロをつかうんだなぁと、
   これも後で気が付いた。"

  (let* ((eval-content0
          (append '(who:with-html-output (out)) (list content)))
         (eval-content
          (append '(with-output-to-string (out)) (list eval-content0))))
    (eval eval-content)))

;deprected...
(defmacro make-content-string (content &body body)
  "同じように defmacro をつかった。setf を使えばいいのか
   あるいはもっとマクロをうまく使えばいいのか。
   習作にしては出来が悪い"
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
  "これも出来が悪いい"
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
