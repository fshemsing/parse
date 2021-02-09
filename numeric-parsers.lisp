;;;; numeric curried functions
(cdefun c* (x y) (* x y))
(cdefun c+ (x y) (+ x y))
(cdefun c- (x y) (- x y))

(defun make-incrementer (n)
  (lambda (x) (+ n x)))

;;;; numeric parsers

;;; parse-digit: match a single character that represents a digit
;;;   return that character as witness
;;;   (aka pDigit)
(defun parse-digit (stream)
  (let ((parser
	 (make-test-parser (lambda (x)
			     (char<= #\0 x #\9)))))
    (funcall parser stream)))

;;; parse-digit-as-int: match a single character that represents a digit
;;;   return the corresponding integer as witness
;;;   (aka pDigitAsInt)
;;;
;;;   Note: digit-char-p is the built-in LISP function for testing
;;;    if a character is a digit. When the character passed to
;;;    digit-char-p is a digit, digit-char-p returns the
;;;    corresponding integer.
(defun parse-digit-as-int (stream)
  (let ((parser (call-on-parser #'digit-char-p
				#'parse-digit)))
    (funcall parser stream)))

;;; parse-natural: parse a sequence of digits as a natural number
;;; (aka pNatural)
(defun parse-natural (stream)
  (let ((parser
	 (call-on-parser (lambda (x)
			   (reduce (lambda (a b)
				     (+ (* 10 a) b))
				   x))
			 (pMany1 #'cons #'parse-digit-as-int))))
    (funcall parser stream)))

;;; parse-int: parse a sequence of digits, possibly preceeded by
;;;   a negative sign as an integer
;;; (aka pInteger)
(defun parse-int (stream)
  (let ((parser
	 (compose-parsers
	  (opt (call-on-parser (lambda (x)
				 (* x -1))
			       (make-string-parser "-"))
	       (lambda (x) x))
	  #'parse-natural)))
    (funcall parser stream)))

;;; parse-sum: parse an expression like "x+y" returning the sum of
;;;   x and y as the witness.
;;; (aka pplus)
(defun parse-sum (stream)
  (let ((parser
	 (compose-parsers
	  (compose-parsers-return-function-parser-witness
	   (call-on-parser #'make-incrementer
			   #'parse-int)
	   (make-string-parser "+"))
	  #'parse-int)))
    (funcall parser stream)))

;;; parse-sum-multiple: parse a sum expression with more than two addends
;;;   like "1+2+3+4" returning the sum of the addends as the witness
;;; (aka pPlus-multiple)
(defun parse-sum-multiple (stream)
  (let ((parser
	 (compose-parsers
	  (call-on-parser (clambda (input flist)
				   (apply-all input flist))
			  #'parse-int)
	  (pMany #'cons
		 (compose-parsers
		  (call-on-parser #'make-incrementer
				  (make-string-parser "+")
				  :return-function T)
		  #'parse-int)))))
    (funcall parser stream)))

;;; apply-all: apply a list of functions to an input iteratively
(defun apply-all (input flist)
  (if (null flist)
      input
      (apply-all (funcall (car flist) input)
		 (cdr flist))))

;;; parse-difference-multiple: parse a sequence of subtractions with
;;;   more than two subtrahends like "5-4-3-2" returning the result
;;;   of the subtraction as the witness
;;; (aka pMinus-multiple)
(defun parse-difference-multiple (stream)
  (let ((parser
	 (compose-parsers
	  (call-on-parser (clambda (input flist)
				   (apply-all input flist))
			  #'parse-int)
	  (pMany #'cons
		 (compose-parsers (call-on-parser (clambda (x y) (- y x))
						  (make-string-parser "-")
						  :return-function T)
				  #'parse-int)))))
    (funcall parser stream)))

;;; parse-sum/difference: parse an arithmetic statement composed of
;;;   sums and differences returning the integer result of the
;;;   arithmetic as the witness
;;; (aka pPlusMinus)
(defun parse-sum/difference (stream)
  (let ((parser (compose-parsers
		 (call-on-parser (clambda (input flist)
					  (apply-all input flist))
				 #'parse-int)
		 (pMany #'cons
			(compose-parsers
			 (concatenate-parsers
			  (call-on-parser (clambda (x y) (- y x))
					  (make-string-parser "-")
					  :return-function T)
			  (call-on-parser #'c+
					  (make-string-parser "+")
					  :return-function T))
			 #'parse-int)))))
    (funcall parser stream)))

;;; parse-sum/difference*:
;;; (aka pPlusMinus*)
(defun parse-sum/difference* (stream)
  (let ((parser (pChainL (concatenate-parsers
			  (call-on-parser #'c-
			      (make-string-parser "-")
			      :return-function T)
			  (call-on-parser #'c+
			      (make-string-parser "+")
			      :return-function T))
			 #'parse-int)))
    (funcall parser stream)))


;;; mul-ops:  match the asterisk character "*" returning the
;;;   curried multiplication operation #'c* as the witness
(defun mul-ops (stream)
  (funcall (anyOp `((,#'c* "*")))
	   stream))

;;; parse-product: parse an arithmetic statement consisting of
;;;   a product with any number of factors like "5*6*7" returning
;;;   the result of the multiplication as the witness
;;; (aka pTimes)
(defun parse-product (stream)
  (funcall (pChainL #'mul-ops #'parse-int)
	   stream))

;;; add-ops: match the plus "+" character or the minus "-"
;;;   character returning the corresponding curried operation
;;;   (#'c+ or #'c-) as the witness
(defun add-ops (stream)
  (funcall (anyOp `((,#'c+ "+")
		    (,#'c- "-")))
	   stream))

;;; parse-sum/difference/product: parse an arithmetic statment
;;;   composed of subtractions, additions, and multiplications
;;;   retruning the numerical result of the arithemtic as the
;;;   witness
;;; (aka pPlusMinusTimes)
(defun parse-sum/difference/product (stream)
  (funcall (pChainL #'add-ops
		    #'parse-product)
	   stream))
