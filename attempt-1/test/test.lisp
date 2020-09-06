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
  (ok (quack-proof-of-concept)
      "Quack Proof Of Concept")

  (ok (sorting-class/random/single-lin)
      "Random single linearization")

  (ok (sorting-class/random/many-lin)
      "Random many random linearizations")

  (ok (sorting-class/rule-db)
      "Test rule-db rules on sorting classes & groups"))


(subtest "lexicographic/package-then-symbol-<"
  (plan 10)

  (ok (a1::lexicographic/package-then-symbol-< 'attempt-1::a 'attempt-1::b)
      "attempt-1:a lexicographic/package-then-symbol-< attempt-1:b")

  (ok
   (not (a1::lexicographic/package-then-symbol-< 'attempt-1::b 'attempt-1::a))
   "not ( attempt-1:blexicographic/package-then-symbol-< attempt-1:a )")

  ;; attempt-1:a is less than common-lisp:a because
  ;; attempt-1 package name is less than common-lisp package name.
  (ok (a1::lexicographic/package-then-symbol-< 'attempt-1::a 'cl-user::a)
      "attempt-1:a lexicographic/package-then-symbol-< common-lisp:a")

  ;; common-lisp:a is not less than attempt-1:a because
  ;; common-lisp package name is not less than attemot-1 package name.
  (ok
   (not (a1::lexicographic/package-then-symbol-< 'cl-user::a 'attempt-1::a))
   "not ( common-lisp:a lexicographic/package-then-symbol-< attempt-1:a )")

  (ok (a1::lexicographic/package-then-symbol-< 'cl-user::a 'cl-user::b)
      "common-lisp:a lexicographic/package-then-symbol-< common-lisp:b")

  (ok
   (not (a1::lexicographic/package-then-symbol-< 'cl-user::b 'cl-user::a))
   "not ( common-lisp:b lexicographic/package-then-symbol-< common-lisp:a )")

  ;; attempt-1:z is less than common-lisp:a because package attempt-1 is less
  ;; that common-lisp
  (ok (a1::lexicographic/package-then-symbol-< 'attempt-1::z 'cl-user::a)
      "attempt-1:z lexicographic/package-then-symbol-< common-lisp:a")

  ;; common-lisp:z is not less than attempt-1:a because package attempt-1 is
  ;; not less than common-lisp
  (ok
   (not (a1::lexicographic/package-then-symbol-< 'cl-user::z 'attempt-1::a))
   "not ( common-lisp:z lexicographic/package-then-symbol-< attempt-1:a )")

  ;; :a is less than :b
  (ok (a1::lexicographic/package-then-symbol-< :a :b)
      ":a lexicographic/package-then-symbol-< :b")

  ;; :b is not less than :a
  (ok
   (not (a1::lexicographic/package-then-symbol-< :b :a))
   "not ( :b lexicographic/package-then-symbol-< :a )")

  ;; NOTE: We don't test uninterned symbols (which is actually super
  ;; hard because they are incomparable). There doesn't seem to be a
  ;; way to compare them meaningfully since each uninterned symbol is
  ;; treated as if it is in it own package (with no name).

  )

;; NOTE: Add a1::lexicographic/symbol-then-package-<
;; NOTE: change comments to actual output messages.


(finalize) ;; toplevel closer for the plan
