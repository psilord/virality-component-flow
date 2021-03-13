(in-package #:attempt-1)

;; Ultimately this stuff will end up in the right packages for each subsystem,
;; and in the right files.

;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Engine base conditions
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Error condition hierarchy root
(define-condition engine/error (error) ())
;; Warning condition hierarchy root
(define-condition engine-warning (warning) ())

;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Component Flow condition hierarchy.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition component-flow/error (engine/error) ())
(define-condition component-flow-warning (engine-warning) ())

;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Sorting Class DSL Processing condition hierarchy.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition sorting-class/error (component-flow/error)
  ((%item :reader item
          :initarg :item)))


;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Sorting class Syntax Error condition hierarchy root.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a general syntax error otherwise not more specific
;; used when there isn't a more specific syntax error condition
(define-condition sorting-class/syntax (sorting-class/error)
  ()
  (:report (lambda (c s)
             (format s "Sorting class has a syntax error: ~S"
                     (item c)))))

;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Individual syntax error conditions
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; NOTE: Figure out where this should actually go or change to be.
;; When we make warning ones that collide, add -warn before /
(define-condition sorting-class/bad-sorting-classes-form (sorting-class/syntax)
  ()
  (:report (lambda (c s)
             (format s "There are no sorting classes: ~S"
                     (item c)))))

(define-condition sorting-class/bad-sorting-class-token (sorting-class/syntax)
  ()
  (:report (lambda (c s)
             (format s "Spec name is not a valid sorting-class token: ~S"
                     (item c)))))

(define-condition sorting-class/bad-parents-form (sorting-class/syntax)
  ()
  (:report (lambda (c s)
             (format s "The sorting class parent list is not a list: ~S"
                     (item c)))))

(define-condition sorting-class/bad-parent-token (sorting-class/syntax)
  ()
  (:report (lambda (c s)
             (format s "The sorting class parent is not a valid token: ~S"
                     (item c)))))

(define-condition sorting-class/bad-columns-form (sorting-class/syntax)
  ()
  (:report (lambda (c s)
             (format s "The sorting class columns list is not a list: ~S"
                     (item c)))))

(define-condition sorting-class/bad-column-form (sorting-class/syntax)
  ()
  (:report (lambda (c s)
             (format s "The sorting class column form is not a proper list: ~S"
                     (item c)))))

(define-condition sorting-class/bad-column-token (sorting-class/syntax)
  ()
  (:report (lambda (c s)
             (format s "The sorting class columns is not a valid token: ~S"
                     (item c)))))

(define-condition sorting-class/bad-column-options-form (sorting-class/syntax)
  ()
  (:report (lambda (c s)
             (format s "The sorting class column options is badly formed: ~S"
                     (item c)))))

(define-condition sorting-class/missing-column-comparator
    (sorting-class/syntax)
  ()
  (:report (lambda (c s)
             (format s "The sorting class column comparator is missing for column: ~S"
                     (item c)))))

(define-condition sorting-class/bad-column-comparator-value
    (sorting-class/syntax)
  ()
  (:report (lambda (c s)
             (format s "The sorting class column comparator value is bad: ~S"
                     (item c)))))

(define-condition sorting-class/missing-column-default
    (sorting-class/syntax)
  ()
  (:report (lambda (c s)
             (format s "The sorting class column default is missing for column: ~S"
                     (item c)))))

;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Sorting class Type Error condition hierarchy root.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition sorting-class/type (sorting-class/error)
  ()
  (:report (lambda (c s)
             (format s "Sorting class has a type error: ~S"
                     (item c)))))

(define-condition sorting-class/sort-base-is-not-root
    (sorting-class/type)
  ()
  (:report (lambda (c s)
             (format s "Sorting class SORT/BASE may not have parents: ~A"
                     (item c)))))

(define-condition sorting-class/must-not-be-root
    (sorting-class/type)
  ()
  (:report (lambda (c s)
             (format s "Sorting class ~A: parents cannot be NIL."
                     (item c)))))

(define-condition sorting-class/no-duplicate-parents
    (sorting-class/type)
  ((%parents :reader parents
             :initarg :parents)
   (%duplicates :reader duplicates
                :initarg :duplicates))
  (:report (lambda (c s)
             (format s "Sorting class ~A may not have duplicate parents.~%~%~
              Parents: ~A.~%~
              Duplicate parents: ~A"
                     (item c) (parents c) (duplicates c)))))

(define-condition sorting-class/no-duplicate-sorting-columns
    (sorting-class/type)
  ((%columns :reader columns
             :initarg :columns)
   (%duplicates :reader duplicates
                :initarg :duplicates))
  (:report (lambda (c s)
             (format s "Sorting class ~A may not have duplicate columns.~%~%~
              Columns: ~A.~%~
              Duplicate columns: ~A"
                     (item c) (columns c) (duplicates c)))))

(define-condition sorting-class/no-inheritance-cycles
    (sorting-class/type)
  ((%cycle :reader cycle
           :initarg :cycle))
  (:report (lambda (c s)
             (format s "Sorting class ~A must not be in an inheritance cycle.~%~%~
              Cycle: ~A.~%"
                     (item c) (cycle c)))))

(define-condition sorting-class/too-many-roots
    (sorting-class/type)
  ()
  (:report (lambda (c s)
             (format s "The sorting class inheritance graph can have only ~
                        one root, but found roots: ~A"
                     (item c)))))

(define-condition sorting-class/wrong-root
    (sorting-class/type)
  ()
  (:report (lambda (c s)
             (format s
                     "The sorting class inheritance graph root must be ~
                      SORT/BASE, not ~A"
                     (item c)))))

(define-condition sorting-class/undefined-sorting-classes
    (sorting-class/type)
  ()
  (:report (lambda (c s)
             (format s
                     "These sorting classes are undefined:~%~{ ~A~^~%~}"
                     (item c)))))

(define-condition sorting-class/duplicate-sorting-classes
    (sorting-class/type)
  ()
  (:report (lambda (c s)
             (format s "A sorting class name may not be defined more than ~
                        once.~%~% Duplicate sorting classes: ~A"
                     (item c)))))

(define-condition sorting-class/incomplete-column-specification
    (sorting-class/type)
  ()
  (:report (lambda (c s)
             (format s "Sorting class ~A: sorting column reference(s) from ~
                        one or more parents are missing."
                     (item c)))))

(define-condition sorting-class/duplicate-sorting-column-definitions
    (sorting-class/type)
  ((%duplicates :reader duplicates
                :initarg :duplicates))
  (:report (lambda (c s)
             (format s "Sorting class ~A: Cannot declare a sorting column ~
                        (with another sorting class or itself) more than once:~%~
                        Duplicates: ~A."
                     (item c) (duplicates c)))))

(define-condition sorting-class/bad-column-ordering
    (sorting-class/type)
  ((%col-0 :reader col-0
           :initarg :col-0)
   (%col-1 :reader col-1
           :initarg :col-1)
   (%previous-use :reader previous-use
                  :initarg :previous-use))
  (:report (lambda (c s)
             (format s "Sorting Class ~A: Two column pairs are in a reversed order:~% Column ~A is inconsistent with column ~A given other uses of ~A and ~A~% as found in: ~A."

                     (item c)
                     (col-0 c)
                     (col-1 c)
                     (col-0 c)
                     (col-1 c)
                     (previous-use c)))))
