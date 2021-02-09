;;;; derived-parser-combinators

;;; opt: 
(defun opt (parser witness)
  (concatenate-parsers parser
		       (make-constant-parser witness)))

;;; pChoice: concatenate the results of a list of parsers
(defun pChoice (parser-list)
  (reduce #'concatenate-parsers parser-list
	  :from-end t
	  :initial-value #'fail-parser))

;;; pChoice-i: interleave the results of a list of parsers
(defun pChoice-i (parser-list)
  (reduce #'<x> parser-list :from-end t :initial-value #'fail-parser))

;;; call-on-parser: parser formed by applying the given function to the
;;;   witnesses of the given parser
(defun call-on-parser (function parser &key return-function)
  (if return-function
      (call-on-parser-return-function function parser)
      (compose-parsers (make-constant-parser function) parser)))

;;; <$: like <$> but ignore the witness of the parser and
;;;   return the function as the witness
(defun call-on-parser-return-function (function parser)
  (compose-parsers (call-on-parser (clambda (x _) x)
				   (make-constant-parser function))
		   parser))

;;; <*: like <*> but ignore the witness of the input-parser
;;;   and return the witness of the function parser as the
;;;   witness
(defun compose-parsers-return-function-parser-witness
    (function-parser input-parser)
  (compose-parsers (call-on-parser (clambda (x _) x)
				   function-parser)
		   input-parser))

;;; *>: like <*> but ignore the witness of the
;;;   function-parser and return the witness of the
;;;   input-parser as the witness
(defun compose-parsers-return-input-parser-witness
    (function-parser input-parser)
  (compose-parsers (call-on-parser (lambda (x) x)
				   function-parser
				   :return-function T)
		   input-parser))

;;; <**>: takea a parser whose witnesses and a parser
;;;   whose inputs are functions and composes them
;;;
;;;    Note: <*> is like <**>, but parses the function and the
;;;      input in the opposite order
;;;
;;;
;;; Example/
;;;
;;;     The <**> combinator can be used to simplify parsers
;;;   that reuse a parsed input in the following manner:
;;;
;;;   Without <**>:
;;;   (<+> (<*> (<$> f q) a)  ; q is parsed
;;;        (<*> (<$> g q) b)) ; q is parsed again here 
;;;
;;;   With <**>:
;;;   (<**> q                         ; q is only parsed here
;;;         (<+> (<$> (flip f) a)     ; f uses the parsed q
;;;              (<$> (flip g) b))))  ; g uses the parsed q
;;;
(defun <**> (input-parser function-parser)
  (compose-parsers (call-on-parser (clambda (x f)
					    (funcall f x))
				   input-parser)
		   function-parser))

;;; <??>: a modification of <**> that defaults to using the
;;;   identity function when the function parser fails
;;;
;;;  Example/
;;;
;;;    (pChainR op p) = (<??> p (<*> (<$> flip op)
;;;                                  (lambda (x)
;;;                                    (funcall (pChainR op p) x))
;;;
(defun <??> (input-parser function-parser)
  (<**> input-parser
	(opt function-parser
	     (lambda (x) x))))

;;; pOp-str: parser a given operator with a given string
(defun pOp-str (sem string)
  (call-on-parser sem
		  (make-string-parser string)
		  :return-function :T))

(defun anyOp (op/sym-table)
  (let ((parser-list (mapcar (lambda (x)
			       (apply #'pOp-str x))
			     op/sym-table)))
    (pChoice parser-list)))

;;;; parser sequencers

;;; pMany: match parser as many times as possible, return their
;;;   witnesses combined with the given binary combiner function
(defun pMany (combiner parser)
  (opt (compose-parsers
	(call-on-parser (clambda (x y)
				 (funcall combiner x y))
			parser)
	;; Recursive call to pMany must be lazily evaluated.
	;; Applicative order evaluation causes an infinite
	;; recursion without surrounding lambda.
	(lambda (stream)
	  (funcall (pMany combiner parser) stream)))
       nil))

;;; pMany1: 
(defun pMany1 (combiner parser)
  (compose-parsers
   (call-on-parser (clambda (x y)
			    (funcall combiner x y))
		   parser)
   (lambda (stream)
     (funcall (pMany combiner parser) stream))))

;;; pChainL: parse an input using parser then parse a list of functions
;;;   using operation-parser and apply those functions to the input
;;;   iteratively
(defun pChainL (operation-parser parser)
  (compose-parsers (call-on-parser (clambda (input flist)
					    (apply-all input flist))
				   (lambda (stream)
				     (funcall parser stream)))
		   (pMany #'cons
			  (compose-parsers
			   (call-on-parser
			    (clambda (f a b)
				     (reduce #'funcall (list b a) :initial-value f))
			    (lambda (stream)
			      (funcall operation-parser stream)))
			   (lambda (stream)
			     (funcall parser stream))))))

;;; pChainR:
(defun pChainR (operation-parser input-parser)
  (<??> input-parser
	(compose-parsers (call-on-parser #'flip
					 operation-parser)
			 (pChainR operation-parser input-parser))))
