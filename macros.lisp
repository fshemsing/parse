(defmacro defparser (parser-name &rest body)
  `(defun ,parser-name (stream)
     (funcall ,body strean)))
