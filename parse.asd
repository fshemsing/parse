(defpackage #:parse-asd
  (:use :cl :asdf))

(defsystem parse
    :name "parse"
    :version "0.0.0"
    :maintainer "Frank Hemsing"
    :description "combinator-based parsing"
    :long-description "applicative parsers based on tutorial by S. D. Swierstra"
    :depends-on ("stream"
		 "curried-functions")
    :components ((:file "string-utilities")
		 (:file "parser-constructors")
		 (:file "primitive-parser-combinators"
			:depends-on ("parser-constructors"
				     "string-utilities"))
		 (:file "derived-parser-combinators"
			:depends-on ("primitive-parser-combinators"
				     "parser-constructors"
				     "string-utilities"))
		 (:file "tests"
			:depends-on ("parser-constructors"
				     "primitive-parser-combinators"
				     "derived-parser-combinators"
				     "string-utilities"))
		 (:file "string-parsers"
			:depends-on ("primitive-parser-combinators"
				     "derived-parser-combinators"))
		 (:file "numeric-parsers"
			:depends-on ("parser-constructors"
				     "primitive-parser-combinators"
				     "derived-parser-combinators"
				     "string-parsers"))))
