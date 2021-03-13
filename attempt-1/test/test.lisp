(in-package #:attempt-1.test)

;; :test-file skeleton.

(setf *default-reporter* :list) ;; Fix my theme so :list works.
(setf *enable-colors* nil)
(setf prove:*debug-on-error* t)


(defparameter *ok-full-sorting-class-form*
  '((v:sort/base () ((v::component-type :comparator < :default 0)
                     (v::instance-id :default 0 :comparator >)))
    (foo (v:sort/base) ((a :comparator < :default 0)
                        v::component-type
                        v::instance-id))
    (bar (v:sort/base) ((b :comparator < :default 0)
                        v::component-type
                        v::instance-id))
    (qux (foo bar) (a b v::component-type v::instance-id))))

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
       *ok-full-sorting-class-form*)
      "A correct form passes. [0]")

  (finalize))

(subtest "rule-db/validate-parent-count"
  (plan 3)

  (is-error (a1::rule-db/validate-parent-count
             '((foo () (a b c))))
            'a1::sorting-class/must-not-be-root
            "Ensure a sorting class that is not sort/base has parents.")

  (is-error (a1::rule-db/validate-parent-count
             '((v:sort/base (foo) (a b c))))
            'a1::sorting-class/sort-base-is-not-root
            "Ensure SORT/BASE is the root of the sorting class hierarchy.")

  (ok (a1::rule-db/sorting-classes-syntactically-well-formed
       *ok-full-sorting-class-form*)
      "A correct form passes. [0]")

  (finalize))

(subtest "rule-db/no-duplicate-parents"
  (plan 2)

  (is-error (a1::rule-db/no-duplicate-parents
             'foo '(bar bar) '(a c v d))
            'a1::sorting-class/no-duplicate-parents
            "A sorting class may not have duplicate parents.")

  (ok (a1::rule-db/no-duplicate-parents
       'foo '(bar qux) '(a c v d))
      "Ensure a good sorting class has no duplicate parents.")

  (finalize))

(subtest "rule-db/no-duplicate-sorting-columns"
  (plan 2)

  (is-error (a1::rule-db/no-duplicate-sorting-columns
             'foo '(bar) '(a c c b))
            'a1::sorting-class/no-duplicate-sorting-columns
            "A sorting class may not have duplicate columns.")

  (ok (a1::rule-db/no-duplicate-sorting-columns
       'foo '(bar qux) '(a c v d))
      "Ensure a good sorting class has no duplicate columns.")

  (finalize))

(subtest "rule-db/no-inheritance-cycles"
  (plan 3)

  (let* ((ok-graph (a1::make-inheritance-graph *ok-full-sorting-class-form*))
         ;; short cycle
         (bad0-raw-db '((v:sort/base () ())
                        (foo (v:sort/base foo) ())))
         ;; long cycle
         (bad1-raw-db '((foo (feh) ())
                        (bar (foo) ())
                        (qux (bar) ())
                        (feh (qux) ())))
         (bad0-graph (a1::make-inheritance-graph bad0-raw-db))
         (bad1-graph (a1::make-inheritance-graph bad1-raw-db)))

    (is-error (a1::rule-db/no-inheritance-cycles bad0-graph)
              'a1::sorting-class/no-inheritance-cycles
              "A sorting class system may not have inheritance cycles [0].")

    (is-error (a1::rule-db/no-inheritance-cycles bad1-graph)
              'a1::sorting-class/no-inheritance-cycles
              "A sorting class system may not have inheritance cycles [1].")

    (ok (a1::rule-db/no-inheritance-cycles ok-graph)
        "Check that a good inheritance graph didn't have cycles."))

  (finalize))

(subtest "rule-db/sort/base-is-the-only-root"
  (plan 3)

  (let* ((ok-graph (a1::make-inheritance-graph *ok-full-sorting-class-form*))
         (bad0-raw-db '((v:sort/base () ())
                        (foo () ())))
         (bad1-raw-db '((foo () ())
                        (bar (foo) ())
                        (qux (foo) ())
                        (feh (bar qux) ())))
         (bad0-graph (a1::make-inheritance-graph bad0-raw-db))
         (bad1-graph (a1::make-inheritance-graph bad1-raw-db)))

    (is-error (a1::rule-db/sort/base-is-the-only-root bad0-graph)
              'a1::sorting-class/too-many-roots
              "A sorting class system should have a single root.")

    (is-error (a1::rule-db/sort/base-is-the-only-root bad1-graph)
              'a1::sorting-class/wrong-root
              "A sorting class system must have sort/base as a root.")

    (ok (a1::rule-db/sort/base-is-the-only-root ok-graph)
        "Check that sort/base is properly the only root."))

  (finalize))

(subtest "rule-db/no-missing-parent-declarations"
  (plan 2)

  (is-error (a1::rule-db/no-missing-parent-declarations
             '((foo (bar) ())))
            'a1::sorting-class/undefined-sorting-classes
            "Sorting classes (used as parents) must be defined.")

  (ok (a1::rule-db/no-missing-parent-declarations
       *ok-full-sorting-class-form*)
      "Check that correct parents aren't identified as missing.")

  (finalize))

(subtest "rule-db/sorting-class-name-unique"
  (plan 2)

  (is-error (a1::rule-db/sorting-class-name-unique
             '((foo (bar) ())
               (foo (qux) ())))
            'a1::sorting-class/duplicate-sorting-classes
            "Ensure sorting classes don't have duplicate names.")

  (ok (a1::rule-db/sorting-class-name-unique
       *ok-full-sorting-class-form*)
      "Check that correct sorting classes aren't identified as duplicate.")

  (finalize))

(subtest "rule-db/valid-column-inheritance"
  (plan 3)

  (let* ((ok-graph (a1::make-inheritance-graph *ok-full-sorting-class-form*))
         (bad0-raw-db
           '((v:sort/base () ((v::component-type :comparator < :default 0)
                              (v::instance-id :comparator < :default 0)))
             (foo (v:sort/base) ((a :comparator < :default 0)
                                 v::component-type
                                 v::instance-id))
             ;; drop a column, which generates the error
             (bar (foo) (v::component-type v::instance-id))))
         (bad1-raw-db
           ;; these one are the duplicate column definitions.
           '((foo () ((a :comparator < :default 0)))
             (bar () ((a :comparator < :default 0)))))
         (bad0-graph (a1::make-inheritance-graph bad0-raw-db))
         (bad1-graph (a1::make-inheritance-graph bad1-raw-db)))

    (is-error (a1::rule-db/valid-column-inheritance bad0-graph)
              'a1::sorting-class/incomplete-column-specification
              "Ensure mismatched sorting columns are discovered.")

    (is-error (a1::rule-db/valid-column-inheritance bad1-graph)
              'a1::sorting-class/duplicate-sorting-column-definitions
              "Ensure duplicate sorting columns defs are discovered.")

    (ok (a1::rule-db/valid-column-inheritance ok-graph)
        "Check that correct sorting class columns don't produce error."))

  (finalize))

(subtest "rule-db/all-column-pairs-preserve-order"
  (plan 2)

  (u:mvlet*
      ((bad0-raw-db
        '((v:sort/base () ((v::component-type :comparator < :default 0)
                           (v::instance-id :comparator < :default 0)))
          (foo (v:sort/base) ((a :comparator < :default 0)
                              v::component-type
                              v::instance-id))
          ;; swaps a column, which generates the error
          (bar (foo) (a v::instance-id v::component-type))))
       (bad0-graph bad0-start-vertex (a1::make-inheritance-graph bad0-raw-db))
       (ok-graph ok-start-vertex
                 (a1::make-inheritance-graph *ok-full-sorting-class-form*)))

    (is-error (a1::rule-db/all-column-pairs-preserve-order
               bad0-graph bad0-start-vertex)
              'a1::sorting-class/bad-column-ordering
              "Ensure misordered references to sorting columns are found.")

    (ok (a1::rule-db/all-column-pairs-preserve-order ok-graph ok-start-vertex)
        "Check that correct sorting class columns are ordered correctly."))

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
