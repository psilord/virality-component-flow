(in-package #:cl-user)

(defpackage #:attempt-1.test
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:dll #:net.mfiano.lisp.algae.data-structures.doubly-linked-list)
   (#:a1 #:attempt-1)
   (#:v #:attempt-1))
  (:use #:cl #:prove))
