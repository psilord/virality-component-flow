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
      "Test rule-db rules on sorting classes & groups")

  (finalize))


(subtest "rule-db/sorting-classes-syntactically-well-formed"
  (plan 17)
  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '())
            'a1::sorting-class/bad-sorting-classes-form
            "Ensure an empty raw-db is handled.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo junk ())))
            'a1::sorting-class/bad-parents-form
            "Ensure bad parents form is handled.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (good-0 42 good-1) ())))
            'a1::sorting-class/bad-parent-token
            "Ensure a bad parent token is handled.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (good-0 nil good-1) ())))
            'a1::sorting-class/bad-parent-token
            "Ensure a bad parent nil token is handled.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) junk)))
            'a1::sorting-class/bad-columns-form
            "Ensure bad columns form junk is handled.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) (nil))))
            'a1::sorting-class/bad-column-token
            "Ensure a bad column nil token is handled.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) (42))))
            'a1::sorting-class/bad-column-token
            "Ensure a bad column 42 token is handled.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) ((42 :comparator < :default 0)))))
            'a1::sorting-class/bad-column-token
            "Ensure a bad column 42 in full col form is handled.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) ((a . 23)))))
            'a1::sorting-class/bad-column-form
            "Ensure a bad not proper list column form is handled.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) ((a)))))
            'a1::sorting-class/bad-column-options-form
            "Ensure a full column form has all required options [0].")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) ((a 1 2 3 4 5 6 7)))))
            'a1::sorting-class/bad-column-options-form
            "Ensure a full column form has all required options [1].")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) ((a 9 8 7 6)))))
            'a1::sorting-class/missing-column-comparator
            "Ensure a full column option has a :comparator / value form.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) ((a :comparator <  7 6)))))
            'a1::sorting-class/missing-column-default
            "Ensure a full column option has a :default / value form.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) ((a :comparator nil :default 0)))))
            'a1::sorting-class/bad-column-comparator-value
            "Ensure a full column comparator is not nil.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) ((a :comparator #'< :default 0)))))
            'a1::sorting-class/bad-column-comparator-value
            "Ensure a full column comparator is not a sharpsign quote.")

  (is-error (a1::rule-db/sorting-classes-syntactically-well-formed
             '((foo (parent) ((a :comparator '< :default 0)))))
            'a1::sorting-class/bad-column-comparator-value
            "Ensure a full column comparator is not a quoted item.")

  (ok (a1::rule-db/sorting-classes-syntactically-well-formed
       '((sort/class () ((p :comparator < :default 0)
                         (q :default 0 :comparator >)))
         (foo (sort/class) ((a :comparator < :default 0)
                            p
                            q))
         (bar (sort/class) ((b :comparator < :default 0)
                            p q))
         (qux (foo bar) (a b p q))))
      "A correct form passes. [0]")

  (finalize))




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

  (finalize)
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

  (finalize)

  )


(finalize) ;; toplevel closer for the plan
