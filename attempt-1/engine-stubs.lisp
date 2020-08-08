(in-package #:attempt-1)

;; This file is a bunch of stubs we made to simulate V just enough to write
;; the component-flow. Some should be copied into V, most not. Commented
;; as appropriate.

;; Globals (for DSL storage).
;; Copy into V's metadata.lisp file.
(global-vars:define-global-var =meta/sorting-classes=
    (u:dict #'eq
	    'known-sorting-classes (u:dict #'eq 'sort/base t)))

;; Don't copy into V.
(defmacro define-component (name (&rest parents) &body body)
  (declare (ignore name parents body))
  nil)
