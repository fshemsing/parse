;;; pLetter-a: match the letter 'a' in a stream of characters
(defun pLetter-a (char-stream)
  ;; char-parse-pair: when successful returns a singleton stream containing
  ;;   the pair of the matched character and the rest of the input stream
  (flet ((char-parse-pair (char &optional (rest (mzero)))
	   (if (char= #\a char)
	       (unit (make-parse-pair #\a rest))
	       (mzero))))
    (case-stream char-stream
		 (mzero)
		 ((char)
		  (char-parse-pair char
				   (mzero)))
		 ((first-char other-chars)
		  (char-parse-pair first-char
				   (funcall other-chars))))))

;;; pString_aa: example using <*> to construct a parser that matches
;;;   the characeter #\a twice and returns "aa"
(defun pString_aa (stream)
  (let ((parser 
	 (compose-parsers
	  (compose-parsers (make-constant-parser
			    (lambda (char)
			      (lambda (x)
				(concatenate 'string
					     x
					     (string-of char)))))
			   #'pLetter-a)
	  (compose-parsers (make-constant-parser #'string-of)
			   #'pLetter-a))))
    (funcall parser stream)))

;;; parens: parses a balenced sequence of parentheses and succeeds
;;;   with an integer as witness equal to the nesting depth of the
;;;   parenthesization
(defun parens (stream)
  (let ((parser
	 (concatenate-parsers
	  (reduce #'compose-parsers
		  (list
		   (make-constant-parser
		    (clambda (_ b _ d)
			     (max (1+ b) d)))
		   (make-symbol-parser #'char= #\()
		   #'parens
		   (make-symbol-parser #'char= #\))
		   #'parens))
	  (make-constant-parser 0))))
    (funcall parser stream)))

;;; parens-<$>: parens implemented with <$>
(defun parens-<$> (stream)
  (let ((parser
	 (concatenate-parsers
	  (reduce #'compose-parsers
		  (list (call-on-parser (clambda (_ b _ d)
						 (max (1+ b) d))
					(make-symbol-parser #'char= #\())
			#'parens
			(make-symbol-parser #'char= #\))
			#'parens))
	  (make-constant-parser 0))))
    (funcall parser stream)))

;;; parens-ignores: parens implemented with combinators that
;;;   ignore unneeded witnesses
(defun pParens (parser)
  (compose-parsers-return-function-parser-witness
   (compose-parsers
    (call-on-parser (lambda (x) x)
		    (make-symbol-parser #'char= #\()
		    :return-function T)
    parser)
   (make-symbol-parser #'char= #\))))

(defun parens-ignores (stream)
  (let ((parser
	 (opt (compose-parsers
	       (call-on-parser (clambda (x y)
					(max (1+ x) y))
			       (pParens #'parens-ignores))
	       #'parens-ignores)
	      0)))
    (funcall parser stream)))

;;; parse:
(defun parse (parser string)
  (stream->list (funcall parser
			 (string->stream string))))

;;; parse-file:
;; (defun parse-file parser filepath
;;        (with-stream-from-file (s filepath)
;; 	   (stream->list (funcall parser
;; 				  s))))
