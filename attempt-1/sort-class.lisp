(in-package #:attempt-1)

;; in core V code
(defclass sort/base ()
  ;; TODO: Don't forget to add P[component-type] and I[instance-id] as right
  ;; most columns for sorting.

  ;; SBCL will warn if a slot with the same name but a different package
  ;; exists. So, we name ours a little funny to reduce the chance it will
  ;; conflict with a gamedev slotname.
  ((%100v-sorting-type :reader sorting-type
                       ;; What key do we put here? sort class name?
                       :initform 'sort/base)
   ;; hash table of column names to values.
   (%100v-column-values :reader column-values
                        ;; Key is column name, Value is column value.
                        :initform (u:dict #'eq))))

(defun column-value (instance column-name)
  (multiple-value-bind (value present-p)
      (u:href (column-values instance) column-name)
    (if present-p
        value
        (setf (column-value instance column-name)
              '<code-to-get-column-default-from-core>))))

(defun (setf column-value) (new-val instance column-name)
  ;; TODO: More complicated to rip object out of containing tree and reinsert
  ;; it. Must be derfered to "end of frameish" time so it happens after all the
  ;; mutation phases have completed.
  (setf (u:href (column-values instance) column-name)
        new-val))






;; How sorting classes are implemented and works.

(defun stats (shash edge-table col-assignment)
  (let ((total-columns (hash-table-count shash)))
    (format t "number of columns: ~A~%" total-columns)

    (u:do-hash (col cnt shash)
      (format t "Column: ~(~S~) : ~D~%" col cnt))

    (format t "Edge-list:~%~{ ~A~%~}"  (u:hash->alist edge-table))

    (format t "column assignments :~%~{ ~A~%~}"
            (sort (u:hash->alist col-assignment) #'< :key #'cdr))

    ))

(defun linearize (db)
  (let ((db (copy-tree db))
        (total-columns 0)
        (shash (u:dict #'eq)))
    ;; 1. Compute total number of columns & column frequency(?)
    (loop :for (sc cols) :in db
          :do (loop :for col :in cols
                    :do (u:ensure-gethash col shash 0)
                        (incf (u:href shash col))))
    (setf total-columns (hash-table-count shash))

    ;; 2. Sort sorting class by 1) number of columns, then 2) lexical type
    (let ((db (sort db (lambda (left right)
                         (destructuring-bind (lname lcols) left
                           (destructuring-bind (rname rcols) right
                             (cond
                               ((= (length lcols) (length rcols))
                                (string< lname rname))

                               ((> (length lcols) (length rcols))
                                T)

                               (t
                                nil))))))))

      ;;(format t "db =~%~{~A~%~}" db)

      ;; 3a. Assemble the edge table.
      ;; Here we construct a "LEFT is to the left of RIGHT" table
      (let ((rev-cols (map 'vector (lambda (x) (reverse (second x))) db))
            (edge-table (u:dict #'eq)))
        ;;(:printv rev-cols)
        (u:while (notevery #'null rev-cols)
          (loop :for idx :below (length rev-cols)
                :when (aref rev-cols idx)
                  :do
                     (let ((right (pop (aref rev-cols idx)))
                           (left (first (aref rev-cols idx))))
                       (pushnew right (u:href edge-table left) :test #'eq))))

        ;; 3b. Reverse the edge-table value lists.
        (u:do-hash-keys (key edge-table)
          (u:reversef (u:href edge-table key)))


        ;; 4. Mark the nodes with a column number in a depth first search of
        ;; the edge table. We stop searching when we hit a node that contains
        ;; a column number and assign a increasing number on the way out.
        ;; Also stop recursing when we find a node that is not in the table,
        ;; which means it is a root node. NOTE: There should be only ONE root
        ;; node in our sorting mixing use case. The root node should always
        ;; be the serial-number for the sorting system.
        (let ((col-assignment (u:dict #'eq))
              (last-assigned-col-number 0))
          (labels ((get-col-number ()
                     (prog1 last-assigned-col-number
                       (incf last-assigned-col-number)))

                   (mark (node)

                     ;; base case.
                     (when (integerp (u:href col-assignment node))
                       (return-from mark))


                     (let ((rights (u:href edge-table node)))
                       (dolist (right rights)
                         (mark right))

                       (when node
                         (setf (u:href col-assignment node)
                               (get-col-number))

                         ;; assert that we've built a sound tree.
                         (dolist (right rights)
                           ;; TODO: If this fails, ensure to print out the
                           ;; whole of the test case so we can find out why.
                           (assert (> (u:href col-assignment node)
                                      (u:href col-assignment right))))))))

            (mark nil)

            #++(stats shash edge-table col-assignment)

            ;; We treat column zero as being on the RIGHT side of the table.
            (sort (u:hash->alist col-assignment) #'> :key #'cdr)

            ))))))

;; TODO: add this
(defun make-partition (m n)
  (let ((p (make-array m :initial-element nil)))
    (loop :for i :below n
          :do (push i (aref p (random m))))
    ;; TODO: Fix later.
    (coerce p 'list)))

(defun gen-name (table)
  (let ((name (string-upcase
               (u:random-elt net.mfiano.lisp.algae.rng::+dictionary+))))
    (if (u:href table name)
        (gen-name table)
        (let ((sym (make-symbol name)))
          (setf (u:href table name) t)
          sym))))



(defun select-columns (edges partition node-count bit-positions)
  (let ((node-bits (coerce (loop :repeat node-count
                                 :collect (make-array (length bit-positions)
                                                      :element-type 'bit
                                                      :initial-element 0))
                           'vector))
        (parents (make-array node-count :initial-element nil)))
    (loop for i below node-count
          for bits = (pop partition)
          for children = (gethash i edges)
          do (loop for b in bits do (setf (aref (aref node-bits i) b) 1))
             (loop for c in children
                   do (setf (aref node-bits c)
                            (bit-ior (aref node-bits i)
                                     (aref node-bits c)))
                      (push i (aref parents c))))

    (map 'list (lambda (x y)
                 (list y (bitvector->bit-ordering x bit-positions)))
         node-bits parents)))

(defun bitvector->bit-ordering (bitvector list)
  (loop :with v = (coerce list 'vector)
        :for b :across bitvector
        :for i :from 0
        :when (plusp b)
          :collect (aref v i)))


(defun gen-db (node-count column-count)
  (let* ((words (u:dict #'equal))
         (bit-positions (u:iota column-count))
         (bit-partition (make-partition node-count column-count))
         (class-names (coerce (loop :repeat node-count
                                    :collect (gen-name words))
                              'vector))
         (column-names (coerce (loop :repeat column-count
                                     :collect (gen-name words))
                               'vector)))

    ;; we always want sort/base to be first and with nil parents.
    ;; This makes our real use case easier to test.
    (setf (aref class-names 0) 'sort/base)

    (labels ((gen-dag (node-num)
               (let ((edges (u:dict #'eql)))
                 (loop :for source :from 1 :below node-num
                       :do (loop :repeat (1+ (random 10))
                                 :for target = (random source)
                                 :do (pushnew source (u:href edges target))))
                 edges)))

      ;; Returns bit vector indexed at integer node
      (let ((columns-per-node (select-columns (gen-dag node-count)
                                              bit-partition
                                              node-count
                                              bit-positions)))
        (loop :for (parents cols) :in columns-per-node
              :for i :from 0
              :collect (list (aref class-names i)
                             (loop :for p :in parents
                                   :collect (aref class-names p))
                             (loop :for c :in cols
                                   :collect (aref column-names c))))))))

;; External validation

;; NOTE: Once an ordering between two columns has been defined, that order must
;; be true for ALL sorting classes everywhere that use those two columns.
;; If any derived sorting class has a conflicting order for two sorting
;; columns, it is an error.

;;;; raw-db validation pass (attempt to run in this order)

;; [x] rule-db/sorting-class-syntactically-well-formed
;;     <acts upon a single sorting-class form>
;; Each sorting class must have a
;;  name: a single symbol
;;  parents: a list of symbols, nil is ok, but not as a parent member.
;;  columns: a list of items which are a single symbol or (symbol symbol)
;;  NOTE: the (sym1 sym2) form means sym2 is a runtime looked up function
;;        symbol. This means we disallow (f #'f) since it turns into
;;        (f (function f)). We also believe that a fixed function specified
;;        with FUNCTION leads to a case where if the user redefines that
;;        function at runtime, it will NOT update the sorting class, and will
;;        lead to confusion. We (the devs) can optimize looking up the function
;;        ourselves at the end of a frame if we need to speed it up, etc.

;; [x] rule-db/validate-parent-count
;;     The parents for a sorting class that is not sort/base cannot be nil.
;;     The parents for sort/base must be nil.

;; [x] rule-db/sort-class-may-not-be-its-own-parent
;;     A sorting class cannot be its own parent in a sorting class form

;; [ ] rule-db/no-forward-parent-declarations
;;     <acts upon a the full raw-db of sorting-class forms>
;;     A raw-db sorting-class cannot use a parent sorting-class which
;;     hasn't been seen before.

;; [ ] rule-db/column-definitions-and-references-well-formed
;;     <acts upon a the full raw-db of sorting-class forms>
;;     When a column is first defined in a sorting class hierarchy, it REQUIRES
;;     a comparator function and default to be paired with it. It can only be
;;     defined once.
;;
;;     When a column previously defined in the sorting hierarchy is referenced,
;;     it MUST NOT supply a comparator function or default.

;; [ ] rule-db/canonicalize-for-linearization
;;     Canonicalize the raw-db to remove the column sorting function names.
;;     It strictly means removing the function comparators and making the
;;     columns a pure symbol name.

;; [ ] rule-db/valid-column-inheritance
;;     Ensure that all columns in a particular raw-db came from either itself,
;;     or some parent in the hierarchy.

;; [ ] rule-db/no-duplicate-parents
;;     Each of the sorting class parents must be unique in the parent list.
;;     No NIL/T, or keywords, in the parents list.

;; [ ] rule-db/sorting-class-unique
;;     Each sorting class name in a raw-db must be a unique name.
;;     Duplicate column names that are in different packages are allowed.

;; [ ] rule-db/all-column-pairs-preserve-order
;;     All pairs of columns for each sorting-class must preserve their
;;     order in all classes that used those columns.

;; And then, if all the above passes, we can pass it to LINEARIZE.


;;;; Raw-db type rules.
(defun rule-db/sorting-class-syntactically-well-formed (raw-db)
  (unless (listp raw-db)
    (error "raw-db is not a list"))

  (labels ((invalid-symbol (item)
             ;; a symbol that cannot be a certain subset of symbols.
             (and (symbolp item)
                  (or (keywordp item)
                      (eq item nil)
                      (eq item t))))

           (process-sorting-class (spec)
             (unless (listp spec)
               (error "spec is not a list"))

             (destructuring-bind (&optional sc parents cols) spec
               (when (invalid-symbol sc)
                 (error "spec name is not a valid symbol"))
               (unless (listp parents)
                 (error "spec parents is not a cons"))
               (unless (consp cols)
                 (error "spec columns is not a cons"))

               (dolist (parent parents)
                 (when (not (symbolp parent))
                   (error "spec parent is not a symbol"))
                 (when (invalid-symbol parent)
                   (error "spec parent is not a valid symbol")))

               (dolist (col cols)
                 (cond
                   ((symbolp col)
                    (when (invalid-symbol col)
                      (error "spec col is not a proper symbol.")))
                   ((consp col)
                    (destructuring-bind (&optional col-name . comparator) col
                      (when (invalid-symbol col-name)
                        (error "spec col name in compound form is wrong."))
                      (cond
                        ((= (length comparator) 1)
                         (when (invalid-symbol (first comparator))
                           (error "spec col comparator is invalid."))
                         t)
                        (t
                         (error "spec col compound form is invalid."))))))))))

    (dolist (spec raw-db)
      (process-sorting-class spec)))
  t)

(defun process-rule (func raw-db)
  (loop :for (sc-name parents . colnames) :in raw-db
	:do (funcall func sc-name parents colnames)))

(defun rule-db/validate-parent-count (raw-db)
  (process-rule
   (lambda (sc-name parents colnames)
     (declare (ignore colnames))
     (if (eq sc-name 'sort/base)
	 (when parents
	   (error "Sorting class SORT/BASE may not have parents: ~A"
		  parents))
	 (unless parents
	   (error "Sorting class ~A: parents cannot be NIL." sc-name))))
   raw-db)
   t)

(defun rule-db/sort-class-may-not-be-its-own-parent (raw-db)
  (process-rule
   (lambda (sc-name parents colnames)
     (declare (ignore colnames))
     (when (member sc-name parents)
       (error "The sorting class ~A cannot be its own parent in the parent list: ~A"
	      sc-name parents)))
   raw-db)
  t)





#|
API for the =meta/sorting-classes= hash table.
TODO: Possibly convert into an explicit metadata CLOS class.

KEY -> VALUE in =meta/sorting-classes= hash table.

known-sorting-classes -> HASH_TABLE[Key: sorting-class-sym, Value: T]

|#


(defun insert-known-sorting-class (sorting-class-name)
  (symbol-macrolet ((known-classes
                      (u:href =meta/sorting-classes= 'known-sorting-classes)))
    (u:unless-found (table known-classes)
      (setf known-classes (u:dict #'eq)))

    (setf (u:href known-classes sorting-class-name) t)))

(defun lookup-known-sorting-class (sorting-class-name)
  (symbol-macrolet ((known-classes
                      (u:href =meta/sorting-classes= 'known-sorting-classes)))
    (u:unless-found (table known-classes)
      (setf known-classes (u:dict #'eq)))

    (u:href known-classes sorting-class-name)))


(defun rule-db/no-forward-parent-declarations (raw-db)
  ;; Look up in the metadata for is these parents have been seen before.
  ;; If so, all is good, if not, it is a forward declaration.
  ;; NOTE: This assume rule-db/parents-must-be-non-nil has happened.
  (symbol-macrolet ((known-classes
                      (u:href =meta/sorting-classes= 'known-sorting-classes)))
    (loop :for (sc-name parents . colnames) :in raw-db
          :do (dolist (parent parents)
                (u:unless-found (parent-class (u:href known-classes parent))
                  (error "Sorting-class ~A must be previously defined."
                         parent))))))



;;;; linearization validation pass:

;; [x] rule-lin/no-duplicated-columns
;;     No duplicated column names in linearization.

;; [x] rule-lin/no-duplicated-indices
;;     No duplicated column indices in linearization.

;; [x] rule-lin/only-valid-columns-exist
;;     All columns in each sorting-class must exist in the final ordering.
;;     No extra columns, no missing columns.

;; [x] rule-lin/sorting-class-column-order-preserved
;;     The order of each sorting-class column must be preserved in the
;;     linearization.

;; [x] rule-lin/no-index-holes-start-from-zero
;;     No holes in the integer indexing starting from 0 to max column number.

;;;; linearization type rules.
(defun rule-lin/no-duplicated-columns (linearization)
  (= (length linearization)
     (length (remove-duplicates linearization :key #'car))))

(defun rule-lin/no-duplicated-indices (linearization)
  (= (length linearization)
     (length (remove-duplicates linearization :key #'cdr))))

(defun rule-lin/only-valid-columns-exist (linearization all-columns)
  (and (null (set-difference (mapcar #'first linearization) all-columns))
       (= (length linearization) (length all-columns))))

(defun rule-lin/sorting-class-column-order-preserved (linearization raw-db)
  (let ((x (loop :for (name parents columns) :in raw-db
                 :collect (mapcar (lambda (col)
                                    (cdr (assoc col linearization)))
                                  columns))))
    (every (lambda (col-indices)
             (apply #'> col-indices))
           x)))

(defun rule-lin/no-index-holes-start-from-zero (linearization)
  (equal (mapcar #'cdr (sort (copy-seq linearization) #'< :key #'cdr))
         (u:iota (length linearization))))





(defmacro define-sorting-classes (name () &body body)
  (declare (ignore name body))
  nil)

(defmacro define-column-sorter (sortclass new-func &body body)
  (declare (ignore sortclass new-func body))
  nil)

(defun override-column-sorter (column-name new-func &rest args)
  (declare (ignore column-name new-func args))
  nil)




;; in core V for default sorting-classes used by V supplied components.
(define-sorting-classes virality-core ()
  ;;(sort/numeric (sort/base) ((sorting-value sorting-value-sort-hook)))
  (sort/render-layer (sort/base) ((render-layer render-sort-hook))))

;; When assigning comaprator functions, if 'sym-< is not defined, then set it
;; to #'< and assume integers. Otherwse, use fdefinition of the sym-< symbol.
(define-component render (sort/render-layer) ())


;; Additional content related to C-c C-c and C-c C-k and duplicate forms of
;; the same define-sroting-classes macro.
;;
;; Prerequisites:
;;  Each define-sorting-class has a "group name" and the sorting classes are
;;   bound to a group name (for organizational purposes).
;;  The inheritance structure is aware of the sorting-class group boundaries.
;;
;; On C-c C-c:
;;  Game Running: *core-debug* is boundp
;;   Remove metadata for sorting class group, but keep broken edges to other
;;   groups. check that self insertion is valid back into metadata. Check that
;;   sorting class group which inherit from this group are still valid. If not
;;   blow up and tell use to restart image. :) Otherwise think a lot.
;;  Game Not Running:
;;   Do the same thing as "Game Running", but emit a style warning if
;;   the reinsertion of the data was successful. If the result of the insertion
;;   is bad then complain loudly to the user. Encode the failure into the
;;   metadata which prevents the game from starting until it is resolved by
;;   additional C-c C-c/C-k actions which may result in a valid state.

;; Concept to think about. Put into trello. Do we want to bypass the
;; live-recompilation queue whent he debugger is open on a stopped game. Or can
;; we provide the user a choice? (Like, the user does C-c C-c a bunch of
;; times on DSL forms, and then when they continue, the game IMMEDIATELY
;; repsents them with another debugger repl where they can indicate if they
;; want their change (where possible) to happen right now or go into the live
;; recompilation queue to be done in the flow at the apprpriate time.



;; Refactoring the rules:
;; There are three styles of rules dealing with the raw-db form:
;; 1. "Each sorting class spec form. E.G. (foo (sort/base) (f d k))
;; 2. "The sorting class group form E.G. ((x (sort/base) (f g)) (y (x) (m n)))
;; 3. "The entire sorting-class metadata." E.G. all group forms together.
;;
;; Some rules are apprpriate for each of these, some may need to be broken up
;; into the right section.
