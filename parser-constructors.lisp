;;;; parser: a parser is a function that takes a stream of graphemes and returns
;;;;   a stream of pairs representing possible parsings of the stream.
;;;;   If the parse succeeds, the pairs consist of a witness for the success
;;;;   and a stream of graphemes to be parsed.
;;;;   When a parser fails, it returns the empty stream.

(defun make-parse-pair (witness rest)
  (cons witness rest))

(defun parse-pair-witness (parse-pair)
  (car parse-pair))

(defun parse-pair-rest (parse-pair)
  (cdr parse-pair))

;;; fail-parser: a parser that always fails
;;; (aka pFail)
(defun fail-parser (stream)
  (declare (ignorable stream))
  (mzero))

;;; make-symbol-parser: returns a parser that matches the specified element in
;;;   a stream of graphemes using the specified test to check for equality
;;;   of stream elements. Also provides the specified element as the witness
;;;   for the match.
;;;   (aka pSym)
(defun make-symbol-parser (test match-elem)
  (lambda (stream)
    (flet ((elem-parse-pair (elem rest)
	     (if (funcall test elem match-elem)
		 (unit (make-parse-pair match-elem rest))
		 (mzero))))
      (case-stream stream
		   (mzero)
		   ((elem)
		    (elem-parse-pair elem
				     (mzero)))
		   ((first-elem other-elems)
		    (elem-parse-pair first-elem
				     (funcall other-elems)))))))

;;; make-test-argument-parser: returns a parser that succeeds when
;;;   the first element of the stream it is parsing passes the given
;;;   test and provides as witness the given function applied to that
;;;   element.
;;; (aka pFun - probably not used)
(defun make-test-argument-parser (test fun)
  (lambda (stream)
    (flet ((elem-parse-pair (elem rest)
	     (if (funcall test elem)
		 (unit (make-parse-pair (funcall fun elem)
					rest))
		 (mzero))))
      (case-stream stream
		   (mzero)
		   ((elem)
		    (elem-parse-pair elem
				     (mzero)))
		   ((first-elem other-elems)
		    (elem-parse-pair first-elem
				     (funcall other-elems)))))))

;;; make-test-parser: returns a parser that matches an element if the
;;;   element passes the given test, returning that element as witness
;;;  (aka pSatisfy ?)
(defun make-test-parser (test)
  (make-test-argument-parser test (lambda (x) x)))

;;; make-constant-parser: returns a parser that matches any input providing
;;;   the given element as the witness for the match paired with the entire
;;;   unmodified input stream.
;;;  (aka pReturn)
(defun make-constant-parser (witness)
  (lambda (stream)
    (unit (make-parse-pair witness stream))))
