(in-package :cl-clasp)

; デバッグ用の変数
; eval-html-string を nil にすると 
; |#<-reader| は who 形式への変換をしない
(defparameter *eval-html-string* t)


;----------------------------------------------------------------
; ignore-space 用のヘルパー
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
; |#<-reader| が余計な "" (コンテンツなしのストリング)
; を生成するのでそれを除去する関数

(defun ignore-space (lst)
        (atom-filter #'only-space-string lst))


;#Paul's filter
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))


; #< ではじまり ># でおわる html を who 形式に変換する。
; JavaScript や ccs の "  " をちゃんと解析できないので注意

(defun |#<-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((left-k #\() (right-k #\))
        chars (status 'init) pre-pre-char pre-char one-char)
    (do ((curr (read-char stream) pre-fetch-curr)
         (pre-fetch-curr (read-char stream) (read-char stream)))
      ((and (char= curr #\>) (char= pre-fetch-curr #\#)))

      (setf one-char curr)
      ;(format t "~%pre-fetch-curr:<~a>~%" one-char)
#|
状態遷移で事が進む。初期状態は 'init
最終的に cond を通過した後の
one-char がスタックに積まれていく
one-char の元の値は今読んだ値である curr になる
one-char が nil ならなにもつまれない

< を発見したら 'tag-fetch になる。one-char は nil
そうじゃなかったら 'reading

'reading 状態だと one-char はそのままなので積まれる
バグ発見、最初に 'reading 状態に陥るようなものは " がつまれない。

'tag-fetch での考慮は / と space とそのほか

/ は >(right-kを出力して) 'tag-end へ
space はスキップする (<   pre などの < と pre の間のスキップの考慮)
	tag 名のスキップは特殊なので 'tag-start-space-skip
	(XML の仕様上もしかしたら <  pre とかはありえないかもしれないが
	     こうりょしている)

そのほかの場合は 名前を抽出するため
	'tag-start-reading

'tag-start-reading では
	space があればそのほかの後リービューとを読む 'tag-reading
	> でおわれば 'init 状態

解説が中途半端だけど
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
;
; 指定された file-name の素の html を who 形式に変換
; 前後に <div> </div> をつけるようにした
; 冗長なこともあるので注意
; また、このパーサーは完全ではない
; 特に pre の評価、style シートや java script に " が混ざっている場合
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

