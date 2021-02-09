;;; string-of: convert a char to a string if needed
(defun string-of (x)
  (if (stringp x)
      x
      (make-sequence 'string 1 :initial-element x)))

;;; string-cons: form a string by appending 
(defun string-cons (char string)
  (concatenate 'string (string-of char) string))
