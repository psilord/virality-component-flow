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


(defun initialize-engine ()
  (let ((core (make-core)))
    (init-sorting-class-info-table core)
    core))


(defun toposort (edges)
  "Constructs a graph from a list of EDGES, and calculates a valid topological
sorting of the nodes. EDGES is a list of conses, with the CDR being a
dependency of the CAR. If the CAR is non-NIL and the CDR is NIL, it means there
is no dependency on CAR."
  (flet ((make-graph (edges)
           (let ((nodes nil)
                 (neighbors (u:dict #'eql)))
             (dolist (edge edges)
               (destructuring-bind (a . b) edge
                 (pushnew a nodes)
                 (when b
                   (pushnew b nodes)
                   (pushnew b (u:href neighbors a)))))
             (values nodes neighbors))))
    (u:mvlet ((nodes neighbors (make-graph edges))
              (indegrees (u:dict #'eql))
              (roots nil)
              (ordering nil))
      (dolist (node nodes)
        (setf (u:href indegrees node) 0))
      (dolist (node nodes)
        (dolist (neighbor (u:href neighbors node))
          (incf (u:href indegrees neighbor))))
      (dolist (node nodes)
        (when (zerop (u:href indegrees node))
          (push node roots)))
      (u:while (plusp (length roots))
        (let ((node (pop roots)))
          (push node ordering)
          (dolist (neighbor (u:href neighbors node))
            (decf (u:href indegrees neighbor))
            (when (zerop (u:href indegrees neighbor))
              (push neighbor roots)))))
      (unless (= (length ordering)
                 (length nodes))
        (error "Cycle detected in graph."))
      (nreverse ordering))))
