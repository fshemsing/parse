(defparameter *debug-run* nil)

;;; run: REPL interface for parsers
(defun run (parser sample)
  (let ((input-value nil))
    (flet ((print-query ()
	     (format T "Give an expression like: ~a or (q) to quit~&" sample)))
      (print-query)
      (loop
	 :until (eq (setf input-value (read)) 'q)
	 :doing (let ((parser-output
		       (funcall parser
				(string->stream (princ-to-string input-value)))))
		  (when *debug-run*
		    (format T "Input was: ~a~&" input-value)
		    (format T "Stream input: ~a~&" (string->stream
						    (princ-to-string input-value)))
		    (format T "Parser output: ~a~&" (stream->list parser-output)))
		  (if parser-output
		      (format T "Result is: ~{~a ~}~&"
			      (mapcar #'parse-pair-witness
				      (stream->list parser-output)))
		      (write-line "Incorrect input"))
		  (print-query))))))
