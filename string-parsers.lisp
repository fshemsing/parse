;;; parse-string: match a sequence of characters specified by
;;;   the given string
(defun make-string-parser (string)
  (if (= (length string) 0)
      (make-constant-parser "")
      (let ((first-char (char string 0))
	    (other-chars (subseq string 1)))
	(lambda (stream)
	  (let ((parser
		 (compose-parsers
		  (call-on-parser (clambda (char s)
					   (string-cons char s))
				  (make-symbol-parser #'char= first-char))
		  (make-string-parser other-chars))))
	    (funcall parser stream))))))
