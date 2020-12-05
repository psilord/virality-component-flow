(in-package #:attempt-1)

;; in core V code
(defclass sort/base ()
  ;; TODO: Don't forget to add P[component-type] and I[instance-id] as right
  ;; most columns for sorting.

  ;; SBCL will warn if a slot with the same name but a different package
  ;; exists. So, we name ours a little funny to reduce the chance it will
  ;; conflict with a gamedev slotname.
  ((sorting-class-type :reader sorting-class-type
                       ;; Holds the sort class symbol name in the correct
                       ;; package
                       :initform 'sort/base)
   ;; hash table of column names to values.
   (%100v-column-values :reader column-values
                        ;; Key is column name, Value is column value.
                        :initform (u:dict #'eq))))

;; NOTE: for sort/base's p column (the package qualified component name symbol)
;; and the i column (the integer serial num), ensure that p is sorted by either
;; package/symbol or symbol/package but not just by its symbol name alone.

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






(defun lexicographic/package-then-symbol-< (left right)
  (let* ((left-package-name (package-name (symbol-package left)))
         (right-package-name (package-name (symbol-package right)))
         (left-name (symbol-name left))
         (right-name (symbol-name right)))

    (let ((result
            (if (string= left-package-name right-package-name)
                (string< left-name right-name)
                (string< left-package-name right-package-name))))
      ;; Crunch it to boolean instead of geneeralized boolean.
      ;; Make prove tests more understandable.
      (if result t nil))))

(defun lexicographic/symbol-then-package-< (left right)
  (let ((left-package-name (package-name (symbol-package left)))
        (right-package-name (package-name (symbol-package right)))
        (left-name (symbol-name left))
        (right-name (symbol-name right)))

    (let ((result
            (if (string= left-name right-name)
                (string< left-package-name right-package-name)
                (string< left-name right-name))))
      ;; Crunch it to boolean instead of geneeralized boolean.
      ;; Make prove tests more understandable.
      (if result t nil))))

;; How sorting classes are implemented and works.

;; private human readable does linearize work validty output.
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
        (shash (u:dict #'eq)))
    ;; 1. Compute total number of columns & column frequency(?)
    (loop :for (sc cols) :in db
          :do (loop :for col :in cols
                    :do (u:ensure-gethash col shash 0)
                        (incf (u:href shash col))))

    ;; 2. Sort sorting class by 1) number of columns, then 2) by symbol name,
    ;;    3) package name
    (let ((db (sort db (lambda (left right)
                         (destructuring-bind (lname lcols) left
                           (destructuring-bind (rname rcols) right
                             (cond
                               ((= (length lcols) (length rcols))
                                (lexicographic/symbol-then-package-<
                                 lname rname))

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


;; External validation

;; NOTE: Once an ordering between two columns has been defined, that order must
;; be true for ALL sorting classes everywhere that use those two columns.
;; If any derived sorting class has a conflicting order for two sorting
;; columns, it is an error.

;; We _MUST_ have a group name for DEFINE-SORTING-CLASSES. This is so that we
;; can differentiate between when a user defines a group with a different body
;; containing a sorting class with a different parent than the previous
;; definition, and two distinct groups containing the same sorting class with a
;; different parent for each group (invalid).

;;;; raw-db validation pass (attempt to run in this order)

;;;;
;;;; Rules to typecheck an individual sorting class form:
;;;;

;; . - test written
;; / - unit test written
;; x - actually in the code flow in the proper place.

;; [/] rule-db/sorting-classes-syntactically-well-formed [raw-db as a whole]
;; [/] rule-db/sorting-class-syntactically-well-formed [individual sc form]
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

;; [/] rule-db/validate-parent-count
;;     The parents for a sorting class that is not sort/base cannot be nil.
;;     The parents for sort/base must be nil.

;; [/] rule-db/sort-class-may-not-be-its-own-parent
;;     A sorting class cannot be its own parent in a sorting class form

;; [.] rule-db/no-duplicate-parents
;;     Each of the sorting class parents must be unique in the parent list.

;; [.] rule-db/no-duplicate-sorting-columns
;;     Each sorting class definition can only have a set of symbols for its
;;     columns references or definitions that are all distinct symbols.

;;;;
;;;; Rules to typecheck an individual sorting class form wrt the entire
;;;; currently known set of sorting-classes. Many of these need to run at
;;;; RUNTIME though some may be able to be used at macro expansion time.
;;;;

;; [ ] rule-db/no-inheritance-cycles
;;     Given the inheritance graph, there cannot be any cycles.

;; [.] rule-db/no-missing-parent-declarations
;;     A raw-db sorting-class cannot use a parent sorting-class which
;;     doesn't exist in the whole of the metadata.
;;     Since we're checking the entire DB at runtime, it should be fully
;;     completed by then.

;; [ ] rule-db/column-definitions-and-references-well-formed
;;     When a column is first defined in a sorting class hierarchy, it REQUIRES
;;     a comparator function and default to be paired with it. It can only be
;;     defined once.
;;
;;     When a column previously defined in the sorting hierarchy is referenced,
;;     it MUST NOT supply a comparator function or default.

;; [ ] rule-db/valid-column-inheritance
;;     Ensure that all columns in a particular raw-db came from either itself,
;;     or its direct(s) parent in the hierarchy.

;; [.] rule-db/sorting-class-name-unique
;;     If two sorting classes are defined, their sorting class names must be
;;     distinct symbols.

;; [ ] rule-db/all-column-pairs-preserve-order
;;     All pairs of columns for each sorting-class must preserve their
;;     order in all classes that used those columns.

;; ------------------------------

(defun junk()
  '((sort/base () ((z1 :comparator '< :default nil)
                   (z0 :comparator '< :default -1)))
    (foo (sort/base) ((a :comparator '< :default 0)
                      (b :comparator '< :default 0)
                      z1
                      z0))
    (bar (sort/base) ((c :comparator '< :default 0)
                      (d :comparator '< :default 0)
                      z1
                      z0))
    (qux (foo bar) (a
                    (e :comparator '< :default 0)
                    b
                    c
                    (f :comparator '< :default 0)
                    d
                    z1
                    z0))
    (feh (bar qux foo) (a
                        e
                        b
                        (g :comparator '< :default 0)
                        c
                        f
                        d
                        (h :comparator '< :default 0)
                        z1
                        z0))))

;; Removal of extraneous ancestors of two nodes in a DAG requires a
;; least common ancestor algorithm. Finding them is done like this:
;; https://en.wikipedia.org/wiki/Tarjan%27s_off-line_lowest_common_ancestors_algorithm

;; sort/base -> nil
;; foo -> sort/base
;; bar -> sort/base
;; qux -> (foo bar)
;; feh -> (qux foo)

;; qux POT-LEAF
;; foo POT-LEAF

;; from feh:
;; (bar sort/base nil)
;; qux -> foo -> sort/base -> nil
;; qux -> bar -> sort/base -> nil
;; foo -> sort/base -> nil

(defun rule-db/sorting-class-name-unique (raw-db)
  (let ((tbl (u:dict #'eq))
        (duplicates nil))
    (process-sorting-classes
     (lambda (sc parents cols)
       (declare (ignore parents cols))
       (if (u:href tbl sc)
           (incf (u:href tbl sc))
           (setf (u:href tbl sc) 1)))
     raw-db)

    (u:do-hash (sc-name refcount tbl)
      (when (> refcount 1)
        (pushnew sc-name duplicates :test #'eq)))

    (when duplicates
      (error "A sorting class name may not be defined more than once.~%~%~
Duplicate sorting classes: ~A"
             duplicates)))
  t)

(defun doit10 () ;; rule-db/sorting-class-name-unique
  (let ((raw-db
          '((foo (hhh) (a b c))
            (fro () (d e f))
            (qux (foo) (a b c g))
            (hhh (bar) (d e f h))
            (fro (foo bar) (a b c d e f p o l))
            (fro (qux) (a b c g k))
            (hhh () (z x y)))))
    (rule-db/sorting-class-name-unique raw-db)))

;; definition 1
(defun rule-db/no-missing-parent-declarations (raw-db)
  (let ((sc-tbl (u:dict #'eq))
        (missing-tbl (u:dict #'eq)))

    (process-sorting-classes
     (lambda (sc parents cols)
       (declare (ignore parents cols))
       (setf (u:href sc-tbl sc) sc))
     raw-db)

    (process-sorting-classes
     (lambda (sc parents cols)
       (declare (ignore cols))
       (dolist (parent parents)
         (unless (u:href sc-tbl parent)
           (pushnew parent (u:href missing-tbl sc) :test #'eq))))
     raw-db)

    (let ((collected-problems nil))
      (u:do-hash (sorting-class missing-parents missing-tbl)
        (push (list sorting-class missing-parents) collected-problems))
      (when collected-problems
        (error "Some sorting classes have missing parents:~%~{ ~A~^~%~}"
               collected-problems)))
    t))


(defun doit9 () ;; rule-db/no-missing-parent-declarations
  (let ((raw-db
          '((foo (hhh) (a b c)) ;; example of forward declare being ok.
            (bar () (d e f))
            (qux (foo) (a b c g))
            (feh (bar) (d e f h))
            (fro (foo bar) (a b c d e f p o l))
            (ggg (lll ppp) (p p p))
            (iii (mmm fro) (p p p))
            (hhh () (z x y)))))
    (rule-db/no-missing-parent-declarations raw-db)))




;; And then, if all the above passes, we can pass it to LINEARIZE.

(defun process-sorting-classes (func raw-db)
  "Execute FUNC on each destructured sorting-class form in RAW-DB collecting
the results and then return the result list."
  (loop
    :for spec :in raw-db
    :do (unless (listp spec)
          (error "process-rules: sorting-class form is not a list"))
    :collect (destructuring-bind (sc-name parents colnames) spec
               (funcall func sc-name parents colnames))
      :into result
    :finally (return (nreverse result))))

(u:eval-always
  (defun valid-sorting-class-token (token)
    "Check to see if TOKEN is a symbol that has a home package and the package
is not :common-lisp or :keyword."
    (and (symbolp token)
         (notany #'identity
                 (member (symbol-package token)
                         (mapcar #'find-package
                                 '(nil :common-lisp :keyword)))))))

;;;; Raw-db type rules.
(defun rule-db/sorting-class-syntactically-well-formed (sc parents cols)
  (unless (valid-sorting-class-token sc)
    (error "spec name is not a valid symbol"))
  (unless (listp parents)
    (error "spec parents is not a cons"))
  (unless (consp cols)
    (error "spec columns is not a cons"))

  (dolist (parent parents)
    (when (not (symbolp parent))
      (error "spec parent is not a symbol"))
    (unless (valid-sorting-class-token parent)
      (error "spec parent is not a valid symbol")))

  (dolist (col cols)
    (cond
      ((symbolp col)
       (unless (valid-sorting-class-token col)
         (error "spec col is not a proper symbol.")))
      ((consp col)
       (destructuring-bind (&optional col-name . comparator) col
         (unless (valid-sorting-class-token col-name)
           (error "spec col name in compound form is wrong."))
         (cond
           ((= (length comparator) 1)
            (unless (valid-sorting-class-token (first comparator))
              (error "spec col comparator is invalid."))
            t)
           (t
            (error "spec col compound form is invalid.")))))))
  t)

(defun rule-db/sorting-classes-syntactically-well-formed (raw-db)
  (process-sorting-classes
   #'rule-db/sorting-class-syntactically-well-formed raw-db)
  t)

(defun rule-db/validate-parent-count (raw-db)
  (process-sorting-classes
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
  (process-sorting-classes
   (lambda (sc-name parents colnames)
     (declare (ignore colnames))
     (when (member sc-name parents)
       (error "The sorting class ~A cannot be its own parent in the parent list: ~A"
              sc-name parents)))
   raw-db)
  t)

;; TODO: Put into prove.
(defun rule-db/no-duplicate-parents (sc parents cols)
  (declare (ignore cols))
  (let ((tbl (u:dict #'eq))
        (duplicates nil))
    (dolist (parent parents)
      (if (u:href tbl parent)
          (incf (u:href tbl parent))
          (setf (u:href tbl parent) 1)))
    (dolist (parent parents)
      (let ((reference-count (u:href tbl parent)))
        (when (> reference-count 1)
          (pushnew parent duplicates :test #'eq))))
    (when duplicates
      (error "Sorting class ~A may not have duplicate parents.~%~%~
              Parents: ~A.~%~
              Duplicate parents: ~A"
             sc parents (reverse duplicates))))
  t)

;; NOTE: Assumes canonicalized sorting-class form.
(defun rule-db/no-duplicate-sorting-columns (sc parents cols)
  (declare (ignore parents))
  (let ((tbl (u:dict #'eq))
        (duplicates nil))
    (dolist (col cols)
      (if (u:href tbl col)
          (incf (u:href tbl col))
          (setf (u:href tbl col) 1)))
    (dolist (col cols)
      (let ((reference-count (u:href tbl col)))
        (when (> reference-count 1)
          (pushnew col duplicates :test #'eq))))
    (when duplicates
      (error "Sorting class ~A may not have duplicate cols.~%~%~
              Cols: ~A.~%~
              Duplicate cols: ~A"
             sc cols (reverse duplicates))))
  t)








#|
API for the =meta/sorting-classes= hash table.
TODO: Possibly convert into an explicit metadata CLOS class.

KEY -> VALUE in =meta/sorting-classes= hash table.

known-sorting-classes -> HASH_TABLE[Key: sorting-class-sym, Value: T]

|#

;; don't use
(defun insert-known-sorting-class (sorting-class-name)
  (symbol-macrolet ((known-classes
                      (u:href =meta/sorting-classes= 'known-sorting-classes)))
    (u:unless-found (table known-classes)
      (setf known-classes (u:dict #'eq)))

    (setf (u:href known-classes sorting-class-name) t)))

;; don't use
(defun lookup-known-sorting-class (sorting-class-name)
  (symbol-macrolet ((known-classes
                      (u:href =meta/sorting-classes= 'known-sorting-classes)))
    (u:unless-found (table known-classes)
      (setf known-classes (u:dict #'eq)))

    (u:href known-classes sorting-class-name)))

;; Representation of the meta data of a sorting class
(u:eval-always
  (defclass sorting-class-group-descriptor ()
    ((%name :accessor name
            :initarg :name)
     (%user-form :accessor user-form
                 :initarg :user-form)))

  (defun make-sorting-class-group-descriptor (&rest init-args)
    (apply #'make-instance 'sorting-class-group-descriptor init-args)))

(defun duplicate/=meta-sorting-classes= ()
  "While this copy stops at non-cons instances of things, the
DEFINE-SORTING-CLASSES DSL doesn't currently allow such things."
  (mapcar (lambda (descriptor)
            (make-sorting-class-group-descriptor
             :name (name descriptor)
             :user-form (copy-tree (user-form descriptor))))
          =meta/sorting-classes=))

;; The two columnnames representing sort/base's rightmost columns that the
;; user may never specify themselves.
;; v::component-type v::serial-number

(defun canonicalize-sorting-class-for-linearization (sc-name parents cols)
  "Assume the sorting class spec is syntactically well formed.
Return the sorting-class with the sorter functions and defaults removed."
  (list sc-name
        (mapcar (lambda (col)
                  (cond
                    ((symbolp col)
                     col)
                    ((listp col)
                     (car col))
                    (t
                     (error
                      "canonicalize-sorting-class-for-linearization: ~
                             The sorting class was not well formed!: ~A"
                      (list sc-name parents cols)))))
                cols)))

(defun canonicalize-sorting-classes-for-linearization (raw-db)
  "Take in a raw-db and remove all comparators and defaults from the sorting
columns. This makes it appropriate for the linearization algorithm."
  (process-sorting-classes
   #'canonicalize-sorting-class-for-linearization
   raw-db))


;; TODO
(defun canonicalize-sorting-classes (raw-db)
  "Take a RAW-DB and remove all parents from each sorting class that are
ancestors but not direct parents."
  (declare (ignore raw-db))
  nil)



(defun append-internal-sorting-columns (body)
  "Add the internal sorting columns of COMPONENT-TYPE and INSTANCE-ID
on the right most side of each sorting class column set. But don't do it
on V:sort/base."
  (loop :for (sc-name parents cols) :in body
        :collect (if (eq sc-name 'sort/base)
                     (list sc-name parents cols)
                     (list sc-name parents (append cols '(component-type
                                                          instance-id))))))

(defun process-sorting-classes-mutation (name body)
  (unless (symbolp name)
    (error "Cannot define a sorting class group name with non symbol: ~S"
           name))

  (let* ((body/added-internal-columns
           (append-internal-sorting-columns body))
         (group-p (find name =meta/sorting-classes= :key #'name))
         (group (or group-p (make-sorting-class-group-descriptor)))

         ;; Make a copy for validation purposes
         (copy/metadata (duplicate/=meta-sorting-classes=))
         (copy/group-p (find name copy/metadata :key #'name))
         (copy/group (or copy/group-p (make-sorting-class-group-descriptor))))

    ;; TODO: Validate the incoming change BEFORE we actually update the meta
    ;; list.  Instead of doing a transaction-like algorithm with the real
    ;; metadata list, we "deep copy" the metadata list and attempt to adjust
    ;; it. If the adjustment works, then we know it'll work for the real one
    ;; and just do the change for the real one.

    ;; TODO: Keep Going on the validation code.

    ;; Make the change on the duplicate list.
    (reinitialize-instance copy/group
                           :name name
                           :user-form body/added-internal-columns)
    (unless copy/group-p
      (u:appendf copy/metadata (list copy/group)))

    ;; Validate the duplicate list that everything is ok.
    ;; Error on failure, which prevents the rest of the function from
    ;; executing.

    ;; AND THEN
    ;; If the duplicated list was actually ok, then perform the change for
    ;; real.
    (reinitialize-instance group
                           :name name
                           :user-form body/added-internal-columns)
    (unless group-p
      (u:appendf =meta/sorting-classes= (list group)))

    ;; create a list of all sorting classes so far. Unused right now.
    (u:mappend #'user-form =meta/sorting-classes=)

    t))

(u:eval-always
  (defun compute-sorting-class-defclass-forms (body)
    ;; NOTE: If v::*core-debug* is bound, this should expand into a
    ;; packet that inserts itself into the recompilation queue.

    ;; If v::*core-debug* is not bound, it should just expand into the
    ;; literal list of defclass forms for the sorting classes (we know
    ;; the game isn't running or we're compilng freshly).

    ;; Currently, we only implement the second option above.
    (remove-if #'null
               (loop :for (name parents columns) :in body
                     :collect (when (and (valid-sorting-class-token name)
                                         (listp parents))
                                ;; TODO: probably not complete of a
                                ;; representation yet. Need sort/base, etc.
                                `(defclass ,name ,parents ()
                                   (:default-initargs
                                    :sorting-class-type ',name)
                                   ))))))

(defmacro define-sorting-classes (name () &body body)
  `(progn
     ;; If this expansion executing at compile-time discovers that the mutation
     ;; is going to fail (without having performed any of the mutation in the
     ;; actual metadata) then it will ERROR. This will prevent the rest of this
     ;; PROGN from exceuting, preventing the defclass forms from being
     ;; evaluated.
     (process-sorting-classes-mutation ',name ',body)

     ,@(compute-sorting-class-defclass-forms body)
     ))

;; in core V for default sorting-classes used by V supplied components.
(define-sorting-classes virality-core ()
  ;; For this class, both of these columns shall have defaults that are
  ;; never actually used since the initializer function for the sorting
  ;; class will fill in the right vlaues automatically upon construction.
  (sort/base () ((component-type :sorter lexicographic/package-then-symbol-<
                                 :default :no-instance-shall-have-this-value)
                 (instance-id :sorter < :default -1)))

  (sorting-class/render (sort/base) ((render-layer :sorter < :default 0)))

  ;; TODO: Figure out what is relevant below for default V supplied sorting
  ;; classes.

  ;; TODO: the commented out form below doesn't fail any parse currently.
  ;; We need to finish the syntax/typ[e checking of thse forms.
  ;;(sort/numeric (sort/base) ((sorting-value sorting-value-sort-hook)))

  ;; TODO: Decide what should actually be in this sorting class group for
  ;; virality.
  #++(sort/render-layer (sort/base) ((render-layer :sorter render-sort-hook
                                                   :default :default))))



;; Reify metadata into core data structure

;; 1. init functions for each sorting-class
;;   INIT-SORTING-CLASS ->
;;   set all available columns for that type to their defaults in hash table.
;; 2. Build a table keyed by left and right sorting-class types whose value
;;    is an optimized function that performs the actual compare.

;;

;; Pull out (column-name default-value) pairs.
(defun isolate-defined-columns (sc-name parents cols)
  (declare (ignorable sc-name parents))
  (loop :for col-spec :in cols
        :when (consp col-spec)
          :collect (destructuring-bind (name &key sorter default) col-spec
                     (list name sorter default))))

(defun all-pairs (lst)
  (let ((result nil))
    (u:map-combinations (lambda (x) (push x result)) lst :length 2)
    (nreverse result)))

;; engine init-code
(defun init-sc-compare-table (sorting-class-info)
  "Walks the metadata for sorting classes and produces the optimized
comparators for each pair of sorting classes."
  ;; 1. grovel over =meta/sorting-classes= table
  ;; 2. build optimized compare func table.
  ;; hash table: key (type1 type2) -> comparator
  ;;             key (type2 type1) -> Maybe (not comparator)? Validate when
  ;; we start whtis code up again.

  (let* ((sorting-classes
           (mapcar #'first (canon-sorting-specs sorting-class-info)))
         (all-sorting-class-pairs (all-pairs sorting-classes)))

    ;; TODO: COMPLETE ME to create the optimized function comparators for
    ;; each sorting/class type pair.

    all-sorting-class-pairs))


;; engine init code
(defun init-sorting-class-info-table (core)
  "Reifies metadata for sorting class column defaults into the
sorting-class-info object in core."
  ;; 1. grovel over metadata and build column default table for core.
  (let* ((sorting-class-info (sorting-class-info core))
         (sc-col-default-table (defaults sorting-class-info))
         (sc-col-sorter-table (sorters sorting-class-info))
         (sorting-column-defaults ()))
    (loop :for group-descriptor :in =meta/sorting-classes=
          :do (process-sorting-classes
               (lambda (sc parents cols)
                 (loop :for dc :in (isolate-defined-columns sc parents cols)
                       :do (push dc sorting-column-defaults)))
               (user-form group-descriptor)))

    ;; 2a. hash table key: sorting column name, value: default value.
    ;; 2b. hash table key: sorting column name, value: sorter-func.
    (dolist (default-form sorting-column-defaults)
      (destructuring-bind (column-name sorter-func default-value) default-form
        (setf (u:href sc-col-default-table column-name) default-value
              (u:href sc-col-sorter-table column-name) sorter-func)))

    ;; 3. Construct linearization and store it.
    (let ((all-sorting-class-specs
            (loop :for group-descriptor :in =meta/sorting-classes=
                  :append (user-form group-descriptor))))

      ;; TODO: Run all of the rule-db/* that require the full
      ;; all-sorting-class-specs data structure to typecheck everything.
      (assert
       (rule-db/no-missing-parent-declarations all-sorting-class-specs))

      (u:mvlet* ((ascs-canonizalized
                  (canonicalize-sorting-classes-for-linearization
                   all-sorting-class-specs))
                 (linearization (linearize ascs-canonizalized)))

        (setf (linearization sorting-class-info) linearization
              (canon-sorting-specs sorting-class-info) ascs-canonizalized)

        ;; 4. Compute optimized comparators for each pair of sorting classes.
        (init-sc-compare-table sorting-class-info)))))



;; TODO: Build data structure for comparing two instances of types.
;; We walk down two class's arrays until there is a column mismatch or the
;; values of the columns indicated in matching ones are differnent.
;; NOTE: Be careful with EQ comparison.

;; Sbase-> #((p . 1) (i . 0))
;; Foo  -> #((z . 6) (p . 1) (i . 0))
;; Bar  -> #((a . 7) (z . 6) (b . 5) (c . 4) (p . 1) (i . 0))
;; Feh  -> #((r . 11) (s . 10) (t . 9) (z . 6) (l . 2) (p . 1) (i . 0))
;; Meh  -> #((r . 11) (s . 10) (t . 9) (h . 8) (z . 6) (l . 2) (p . 1) (i . 0))
;; Qux  -> etc





;; How to compare two sorting class instances.
(defun sc-compare (sc-left sc-right)
  (let ((key (cons (sorting-class-type sc-left)
                   (sorting-class-type sc-right))))
    ;; TODO: Complete me.
    (declare (ignore key))
    #++ (funcall (u:href compare-func-table key) sc-left sc-right)))

;; ordered-list of sc-descrptors in order of compilation.

;; Note about group rules:
;;


;;;; linearization validation pass:

;; [.] rule-lin/no-duplicated-columns
;;     No duplicated column names in linearization.

;; [.] rule-lin/no-duplicated-indices
;;     No duplicated column indices in linearization.

;; [.] rule-lin/only-valid-columns-exist
;;     All columns in each sorting-class must exist in the final ordering.
;;     No extra columns, no missing columns.

;; [.] rule-lin/sorting-class-column-order-preserved
;;     The order of each sorting-class column must be preserved in the
;;     linearization.

;; [.] rule-lin/no-index-holes-start-from-zero
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






(defmacro define-column-sorter (sortclass new-func &body body)
  (declare (ignore sortclass new-func body))
  nil)

(defun override-column-sorter (column-name new-func &rest args)
  (declare (ignore column-name new-func args))
  nil)





;; When assigning comaprator functions, if 'sym-< is not defined, then set it
;; to #'< and assume integers. Otherwse, use fdefinition of the sym-< symbol.
;; (define-component render (sort/render-layer) ())


;; Additional content related to C-c C-c and C-c C-k and duplicate forms of
;; the same define-sorting-classes macro.
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
