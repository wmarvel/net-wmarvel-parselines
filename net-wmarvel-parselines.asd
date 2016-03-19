;;;; net-wmarvel-parselines.asd

(asdf:defsystem #:net-wmarvel-parselines
  :description "Describe net-wmarvel-parselines here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:alexandria :closer-mop :cl-ppcre)
  :components ((:module "main"
		:serial t
		:components ((:file "package")
			     (:file "macros")
			     (:file "datatypes")
			     (:file "parselines")
			     (:file "defmsg")))))


