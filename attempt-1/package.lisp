;;;; package.lisp


(defpackage #:attempt-1 ;; simulating the V engine 'v' package.
  (:use #:cl)
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:dll #:net.mfiano.lisp.algae.data-structures.doubly-linked-list))

  (:export ;; simulate the V package exports.
   #:define-sorting-classes
   #:sort/base
   #:sort/render-layer
   #:define-component
   #:define-column-sorter
   #:override-column-sorter

   ;; sorting column names
   #:render-layer
   ))

(defpackage #:proj ;; the user's project
  (:use #:cl)
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v #:attempt-1)))


;; Globals
(in-package #:attempt-1)

;; like defvar
(global-vars:define-global-var =meta/sorting-classes= (u:dict #'eq))
