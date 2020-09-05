(in-package #:attempt-1.test)

;; :test-file skeleton.

(setf *default-reporter* :list) ;; Fix my theme so :list works.
(setf *enable-colors* nil)
(setf prove:*debug-on-error* t)

(plan nil) ;; number of subtests to plan or NIL if not sure yet.

#|
(subtest "First subtest"
(plan 2) ;; This subtest plans to run a single test inside of itself.

(is (+ 1 2) 3 "hello world 0")
(is (+ 2 3) 5 "hello world 1")

;; subtest closer for the subtest plan.
(finalize))

(subtest "Second subtest"
(plan 2) ;; This subtest plans to run a single test inside of itself.

(is (+ 1 2) 3 "Goodbye world 0")
(is (+ 2 3) 6 "Goodbye world 1")

;; subtest closer for the subtest plan.
(finalize))
|#

(subtest "sorting-class-codebase"
  (plan 4)
  (ok (doit2) "Test Quack")
  (ok (doit3) "Test single linearization")
  (ok (doit4) "Test many random linearizations")
  (ok (doit5) "Test raw db rules"))

(finalize) ;; toplevel closer for the plan
