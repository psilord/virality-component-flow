(in-package #:attempt-1)

;; This file is a bunch of stubs we made to simulate V just enough to write
;; the component-flow. Some should be copied into V, most not. Commented
;; as appropriate.

;; Globals (for DSL storage).
;; Copy into V's metadata.lisp file.
(eval-when (:compile-toplevel :load-toplevel)
  (global-vars:define-global-var =meta/sorting-classes= (list)))

;; Don't copy into V.
(defmacro define-component (name (&rest parents) &body body)
  (declare (ignore name parents body))
  nil)


(defun copy (object)
  (if (typep object 'sequence)
      (map-into (copy-seq object) #'copy object)
      object))
