(in-package #:attempt-1.test)

;; engine-tests

;; bind this to T in order to see the output in the repl.
(defvar *test-stream* nil)

;; A package where we dump sorting class names and columns for gen-db. They
;; always need a home package and cannot be uninterned.
(defpackage #:test-sorting-class-names)

(defmacro assert-validity-rules (raw-db &body rules)
  "Assert a sequence of predicates. Do not short circuit. If it fails,
then print out the result of which form failed."
  (u:with-gensyms (evals failedp)
    `(let ((,failedp t))
       (unwind-protect
            (let ((,evals (list
                           ,@(loop :for rule :in rules
                                   :collect `(list ',rule ,rule)))))
              (setf ,failedp (some #'null (mapcar #'second ,evals)))

              (when ,failedp
                (format *test-stream* "Failed!~%~{ ~A~%~}" ,evals))

              (not ,failedp))
         (when ,failedp
           (format *test-stream* "Errored!~% raw-db: ~A~%" ,raw-db))))))



(defun doit2 ()
  (let* ((dll (dll:make-list))
         ;; Keep track of these nodes for now.
         (cur-0 nil)
         (cur-1 nil))
    (flet ((emit-dll (edl msg)
             (format *test-stream*
                     "dlist: ~A~% HEAD~%~{  ~(~S~)~%~} HORIZON~%~%"
                     msg
                     (dll:list-values edl))))

      ;; TODO: Cursors should be held in a hash table for easy access in quack.

      (emit-dll dll "Empty")

      (setf cur-0 (dll:insert dll '(0 . :cursor) :where :before))
      (emit-dll dll "Insert before :head Cursor-0")

      (setf cur-1 (dll:insert dll '(1 . :cursor) :where :after
                                                 :target (dll:tail dll)))
      (emit-dll dll "Insert after :tail Cursor-1")

      (dll:insert dll :0-zero :where :before :target cur-0)
      (emit-dll dll "Queue before Cursor-0: :0-zero")

      (dll:insert dll :0-one :where :before :target cur-0)
      (emit-dll dll "Queue before Cursor-0: :0-one")

      (dll:insert dll :1-zero :where :before :target cur-1)
      (emit-dll dll "Queue before Cursor-1: :1-zero")

      (dll:insert dll :1-one :where :before :target cur-1)
      (emit-dll dll "Queue before Cursor-1: :1-one")

      (dll:insert dll :1-two :where :before :target cur-1)
      (emit-dll dll "Queue before Cursor-1: :1-two")

      (format *test-stream*
              "There are ~A elements in the dll.~%~%" (dll:length dll))

      (format *test-stream*
              "Processing dll like a queue from head to horizon...~%")
      (format *test-stream*
              "  AT HEAD~%")
      (loop :until (zerop (dll:length dll))
            :for node = (dll:head dll)
            :for idx :from 0
            :do (dll:delete dll (dll:head dll))
                (format *test-stream*
                        "   Processed node: [~A]: ~(~S~)~%" idx node))
      (format *test-stream*
              "  AT HORIZON~%")))
  t)







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
	;; The names must be in a home package.
        (let ((sym (intern (string-upcase name) :test-sorting-class-names)))
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
    (setf (aref class-names 0) 'v::sort/base)

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








(defun doit3 (&optional (node-count 3) (column-count 3) (verbose nil))
  (let* ((raw-db (mapcar (lambda (x)
                           (list (first x) (second x)
                                 (append (third x) '(p i))))
                         (gen-db node-count column-count)))
         #++(raw-db '((sbase () (p i))
                      (foo (sbase) (z p i))
                      (qux (foo) (d e z f p i))
                      (feh (sbase) (r s k l p i))
                      (meh (feh foo) (r s k h z l p i))
                      (bar (foo) (a z b c p i))))
         (all-columns (remove-duplicates
                       (u:flatten (mapcar #'third raw-db))))
         (db (a1::canonicalize-sorting-classes-for-linearization raw-db))
         )

    (unless (assert-validity-rules raw-db
              (a1::rule-db/sorting-classes-syntactically-well-formed raw-db)
              (a1::rule-db/validate-parent-count raw-db))
      (return-from doit3 nil))

    (let* (
	   (linearization (a1::linearize db)))

      (when verbose
        (format *test-stream*
                "There are ~a named columns.~%" (length all-columns))
        (format *test-stream*
                "*** db:~%~{~A~%~}--> linearization:~%~{~A~%~}"
                raw-db linearization))

      ;; Typecheck
      (unless (or (a1::rule-lin/no-duplicated-columns linearization)
                  (a1::rule-lin/no-duplicated-indices linearization)
                  (a1::rule-lin/only-valid-columns-exist
                   linearization all-columns)
                  (a1::rule-lin/sorting-class-column-order-preserved
                   linearization raw-db)
                  (a1::rule-lin/no-index-holes-start-from-zero linearization))

        (error "Linearization typecheck failed."))

      linearization)))

(defun doit4 (&optional (n 128))
  (loop :for i :below n
        :always (doit3 (+ 5 (random 5)) (+ 10 (random 10)))))



(defun doit5 ()
  (let* ((raw-db '((v::sort/base () (p i))
                   (foo (v::sort/base) (z p i))
                   (qux (foo) (d e z f p i))
                   (feh (v::sort/base) (r s k l p i))
                   (meh (feh foo) (r s k h z l p i))
                   (bar (foo) (a z b c p i)))))

    (assert-validity-rules raw-db
      (a1::rule-db/sorting-classes-syntactically-well-formed raw-db)
      (a1::rule-db/validate-parent-count raw-db)
      (a1::rule-db/sort-class-may-not-be-its-own-parent raw-db)
      #++(a1::rule-db/no-forward-parent-declarations raw-db))))
