;;;; primitive-parser-combinators: combine parsers

;;; compose-parsers: takes a parser whose witnesses are functions and a parser
;;;   whose witnesses are inputs and composes them
;;;   (aka <*>)
;;;
;;;    Note: compose-parsers is like <**>, but parses the function and the
;;;      input in the opposite order
(defun compose-parsers (function-parser input-parser)
  (lambda (stream)
    (bind (funcall function-parser stream)
	  (lambda (function-parse-pair)
	    (flet ((apply-witness-function (input-parse-pair)
		     (make-parse-pair
		      (funcall (parse-pair-witness function-parse-pair)
			       (parse-pair-witness input-parse-pair))
		      (parse-pair-rest input-parse-pair))))
	      (map-stream #'apply-witness-function
			  (funcall input-parser
				   (parse-pair-rest function-parse-pair))))))))

;;; concatneate-parsers: concatenate the results of two parsers
(defun concatenate-parsers (fore-parser rear-parser)
  (lambda (stream)
    (concatenate-stream (funcall fore-parser stream)
			(funcall rear-parser stream))))

;;; remove-if-parser:
(defun remove-if-parser (parse-pair-test parser)
  (lambda (stream)
    (bind (funcall parser stream)
	  (lambda (elem)
	    (if (funcall parse-pair-test elem)
		nil
		(unit elem))))))

;;; <x>: interleave the results of two parsers
(defun <x> (substrate-parser admixture-parser)
  (lambda (stream)
    (interleave-stream (funcall substrate-parser stream)
		       (funcall admixture-parser stream))))

