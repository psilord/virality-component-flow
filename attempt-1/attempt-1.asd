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
               #:cl-digraph
               #:net.mfiano.lisp.golden-utils
               #:net.mfiano.lisp.algae)
  :in-order-to ((asdf:test-op (asdf:test-op #:attempt-1.test)))
  :components (;; "V" engine code
               (:file "package") ;; Globals
	       (:file "common") ;; Utilities, etc.
               (:file "engine-stubs") ;; Low level V simulation stubs.

	       (:file "conditions") ;; all define-condition forms.
               (:file "sort-class") ;; Sorting Class implementation
               (:file "attempt-1") ;; Quack implementation

               ;; The "user project" code that uses the "engine"
               (:file "proj")
               ))
