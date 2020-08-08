(in-package #:attempt-1)

;; engine-tests


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
                  (format t "Failed!~%~{ ~A~%~}" ,evals))

              (not ,failedp))
         (when ,failedp
           (format t "Errored!~% raw-db: ~A~%" ,raw-db))))))




(defun doit2 ()
  (let* ((dll (dll:make-list))
         ;; Keep track of these nodes for now.
         (cur-0 nil)
         (cur-1 nil))
    (flet ((emit-dll (edl msg)
             (format t "dlist: ~A~% HEAD~%~{  ~(~S~)~%~} HORIZON~%~%"
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

      (format t "There are ~A elements in the dll.~%~%" (dll:length dll))

      (format t "Processing dll like a queue from head to horizon...~%")
      (format t "  AT HEAD~%")
      (loop :until (zerop (dll:length dll))
            :for node = (dll:head dll)
            :for idx :from 0
            :do (dll:delete dll (dll:head dll))
                (format t "   Processed node: [~A]: ~(~S~)~%" idx node))
      (format t "  AT HORIZON~%"))))

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
         (db (mapcar (lambda (x)
                       (list (first x) (third x)))
                     raw-db))
         )

    (unless (assert-validity-rules raw-db
	      (rule-db/sorting-class-syntactically-well-formed raw-db)
	      (rule-db/validate-parent-count raw-db))
      (return-from doit3 nil))

    (let ((linearization (linearize db)))

      (when verbose
        (format t "There are ~a named columns.~%" (length all-columns))
        (format t "*** db:~%~{~A~%~}--> linearization:~%~{~A~%~}"
                raw-db linearization))

      ;; Typecheck
      (unless (or (rule-lin/no-duplicated-columns linearization)
                  (rule-lin/no-duplicated-indices linearization)
                  (rule-lin/only-valid-columns-exist linearization all-columns)
                  (rule-lin/sorting-class-column-order-preserved linearization
                                                                 raw-db)
                  (rule-lin/no-index-holes-start-from-zero linearization))

        (error "Linearization typecheck failed."))

      linearization)))

(defun doit4 (&optional (n 128))
  (loop :for i :below n
	:do (when (zerop (mod i 1024))
	      (format t ".")
	      (finish-output))
	:always (doit3 (+ 5 (random 5)) (+ 10 (random 10)))
	:finally (terpri)))



(defun doit5 ()
  (let* ((raw-db '((sort/base () (p i))
                   (foo (sort/base) (z p i))
                   (qux (foo) (d e z f p i))
                   (feh (sort/base) (r s k l p i))
                   (meh (feh foo) (r s k h z l p i))
                   (bar (foo) (a z b c p i)))))

    (assert-validity-rules raw-db
      (rule-db/sorting-class-syntactically-well-formed raw-db)
      (rule-db/validate-parent-count raw-db)
      (rule-db/sort-class-may-not-be-its-own-parent raw-db)
      #++(rule-db/no-forward-parent-declarations raw-db))))
