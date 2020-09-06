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

  (ok (a1::lexicographic/package-then-symbol-< 'attempt-1::a 'cl-user::a)
      "attempt-1:a lexicographic/package-then-symbol-< common-lisp:a")

  (ok
   (not (a1::lexicographic/package-then-symbol-< 'cl-user::a 'attempt-1::a))
   "not ( common-lisp:a lexicographic/package-then-symbol-< attempt-1:a )")

  (ok (a1::lexicographic/package-then-symbol-< 'cl-user::a 'cl-user::b)
      "common-lisp:a lexicographic/package-then-symbol-< common-lisp:b")

  (ok
   (not (a1::lexicographic/package-then-symbol-< 'cl-user::b 'cl-user::a))
   "not ( common-lisp:b lexicographic/package-then-symbol-< common-lisp:a )")

  (ok (a1::lexicographic/package-then-symbol-< 'attempt-1::z 'cl-user::a)
      "attempt-1:z lexicographic/package-then-symbol-< common-lisp:a")

  (ok
   (not (a1::lexicographic/package-then-symbol-< 'cl-user::z 'attempt-1::a))
   "not ( common-lisp:z lexicographic/package-then-symbol-< attempt-1:a )")

  (ok (a1::lexicographic/package-then-symbol-< :a :b)
      ":a lexicographic/package-then-symbol-< :b")

  (ok
   (not (a1::lexicographic/package-then-symbol-< :b :a))
   "not ( :b lexicographic/package-then-symbol-< :a )")

  ;; NOTE: We don't test uninterned symbols (which is actually super
  ;; hard because they are incomparable). There doesn't seem to be a
  ;; way to compare them meaningfully since each uninterned symbol is
  ;; treated as if it is in it own package (with no name).

  )

(subtest "lexicographic/symbol-then-package-<"
  (plan 10)

  (ok (a1::lexicographic/symbol-then-package-< 'attempt-1::a 'attempt-1::b)
      "attempt-1:a lexicographic/symbol-then-package-< attempt-1:b")

  (ok
   (not (a1::lexicographic/symbol-then-package-< 'attempt-1::b 'attempt-1::a))
   "not ( attempt-1:blexicographic/symbol-then-package-< attempt-1:a )")

  (ok (a1::lexicographic/symbol-then-package-< 'attempt-1::a 'cl-user::a)
      "attempt-1:a lexicographic/symbol-then-package-< common-lisp:a")

  (ok
   (not (a1::lexicographic/symbol-then-package-< 'cl-user::a 'attempt-1::a))
   "not ( common-lisp:a lexicographic/symbol-then-package-< attempt-1:a )")

  (ok (a1::lexicographic/symbol-then-package-< 'cl-user::a 'cl-user::b)
      "common-lisp:a lexicographic/symbol-then-package-< common-lisp:b")

  (ok
   (not (a1::lexicographic/symbol-then-package-< 'cl-user::b 'cl-user::a))
   "not ( common-lisp:b lexicographic/symbol-then-package-< common-lisp:a )")

  (ok (a1::lexicographic/symbol-then-package-< 'cl-user::a 'attempt-1::z)
      "cl-user:a lexicographic/symbol-then-package-< attempt-1:z")

  (ok
   (not (a1::lexicographic/symbol-then-package-< 'cl-user::z 'attempt-1::a))
   "not ( common-lisp:z lexicographic/symbol-then-package-< attempt-1:a )")

  (ok (a1::lexicographic/symbol-then-package-< :a :b)
      ":a lexicographic/symbol-then-package-< :b")

  (ok
   (not (a1::lexicographic/symbol-then-package-< :b :a))
   "not ( :b lexicographic/symbol-then-package-< :a )")

  ;; NOTE: We don't test uninterned symbols (which is actually super
  ;; hard because they are incomparable). There doesn't seem to be a
  ;; way to compare them meaningfully since each uninterned symbol is
  ;; treated as if it is in it own package (with no name).

  )


(finalize) ;; toplevel closer for the plan
