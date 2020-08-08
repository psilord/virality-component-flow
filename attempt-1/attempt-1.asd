;;;; attempt-1.asd

(asdf:defsystem #:attempt-1
  :description "Describe attempt-1 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
	       #:queues
	       #:printv
               #:global-vars
	       #:net.mfiano.lisp.golden-utils
	       #:net.mfiano.lisp.algae)
  :components (;; "V" engine code
	       (:file "package") ;; Globals
	       (:file "sort-class") ;; Sorting Class implementation
               (:file "attempt-1") ;; Quack implementation

	       ;; Test code for "engine"
	       (:file "engine-tests")

	       ;; The "user project" code that uses the "engine"
	       (:file "proj")

	       ;; Test code for "project" code
	       (:file "proj-tests")
	       ))
