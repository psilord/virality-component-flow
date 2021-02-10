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
