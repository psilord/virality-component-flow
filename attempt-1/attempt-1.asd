;;;; attempt-1.asd

(asdf:defsystem #:attempt-1
  :description "Describe attempt-1 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
	       #:golden-utils
	       #:queues
	       #:doubly-linked-list)
  :components ((:file "package")
               (:file "attempt-1")))
