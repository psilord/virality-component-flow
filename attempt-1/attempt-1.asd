;;;; attempt-1.asd

(asdf:defsystem #:attempt-1
  :description "Describe attempt-1 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
	       #:queues
	       #:net.mfiano.lisp.golden-utils
	       #:net.mfiano.lisp.algae)
  :components ((:file "package")
               (:file "attempt-1")))
