(in-package #:attempt-1)

;; in core V code
(defclass sort/base ()
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
      (process-sorting-class spec))))

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


(defmacro define-component (name (&rest parents) &body body)
  (declare (ignore name parents body))
  nil)

(defun override-column-sorter (column-name new-func &rest args)
  (declare (ignore column-name new-func args))
  nil)

;; in core V for default sorting-classes used by V supplied components.
(define-sorting-classes virality-core ()
     (sort/render-layer (sort/base) ((render-layer render-sort-hook))))

;; When assigning comaprator functions, if 'sym-< is not defined, then set it
;; to #'< and assume integers. Otherwse, use fdefinition of the sym-< symbol.
(define-component render (sort/render-layer) ())
