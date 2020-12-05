(in-package #:cl-user)

(defpackage #:attempt-1.test
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:dll #:net.mfiano.lisp.algae.data-structures.doubly-linked-list)
   (#:a1 #:attempt-1)
   (#:v #:attempt-1))
  (:use #:cl #:prove))

(in-package #:attempt-1.test)

;; HACK: Prove is using ASDF in a deprecated way causing an annoying but
;; harmless warning message in the middle of tests, so wrap test calling to
;; hide the annoyance for now.
(defun run-tests (object)
  (handler-bind ((asdf/operate:recursive-operate #'muffle-warning))
    (prove:run object)))
