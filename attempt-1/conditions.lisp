(in-package #:attempt-1)

;; Ultimately this stuff will end up in the right packages for each subsystem.

;; Error condition hierarchy
(define-condition engine/error (error) ())

(define-condition component-flow/error (engine/error) ())

(define-condition sorting-class/error (component-flow/error)
  ((%item :reader item
          :initarg :item)))

;; When we make warning ones that collide, add -warn before /
(define-condition sorting-class/is-empty (sorting-class/error)
  ()
  (:report (lambda (c s)
             (format s "A value is empty, but shouldn't be: ~S"
                     (item c)))))

;; a general syntax error otherwise not more specific
;; used when there isn't a more specific syntax error condition
(define-condition sorting-class/syntax (sorting-class/error)
  ()
  (:report (lambda (c s)
             (format s "Sorting class has a syntax error: ~S"
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


;; Warning condition hierarchy
(define-condition engine-warning (warning) ())
(define-condition component-flow-warning (engine-warning) ())
