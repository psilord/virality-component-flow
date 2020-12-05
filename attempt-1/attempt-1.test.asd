;;;; attempt-1.test.asd

(asdf:defsystem #:attempt-1.test
  :description "Describe attempt-1 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:attempt-1
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :pathname "test"
  :perform (asdf:test-op (op c)
			 (uiop:symbol-call '#:attempt-1.test '#:run-tests c))
  :components (
               (:file "package")
	       (:file "engine-tests")
               (:test-file "test")
               ))
