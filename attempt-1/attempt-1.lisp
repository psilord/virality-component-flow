;;;; attempt-1.lisp

(in-package #:attempt-1)

(defmacro with-quack-registers (registers instance &body body)
  `(with-accessors ,(mapcar (lambda (reg) (list reg reg)) registers)
       ,instance
     ,@body))

(defgeneric enable (kernel))
(defgeneric disable (kernel))
(defgeneric attach (actor component))
(defgeneric detach (actor component))
(defgeneric destroy (kernel))

(defclass kernel ()
  ((%context :accessor context
             :initarg :context
             :initform nil)
   (%state :accessor state
           :initarg :state
           :initform :unknown)
   (%ttl-p :accessor ttl-p
           :initarg :ttl-p
           :initform nil)
   (%ttl :accessor ttl
         :initarg :ttl
         :initform 0)))


(defclass actor (kernel)
  ((%components :reader components
                :initform (u:dict #'eq))
   (%components-by-type :reader %components-by-type
                        :initform (u:dict #'eq))))

(defclass component (kernel)
  ((%type :reader component-type
          :initarg :type)
   (%actor :accessor actor
           :initarg :actor
           :initform nil)
   (%initializer :accessor initializer
                 :initarg :initializer
                 :initform nil)))

(defclass core ()
  ((%scene-tree :reader scene-tree)
   (%context :accessor context
             :initarg :context
             :initform nil)
   (%tables :reader tables
            :initform nil)
   (%quack :accessor quack
           :initarg :quack
           :initform (make-quack))))

(defclass context ()
  ((%core :reader core
          :initarg :core)))

(defun make-context (core)
  (setf (slot-value core '%context) (make-instance 'context :core core)))

(defun make-core ()
  (let ((core (make-instance 'core)))
    (make-context core)
    core))


(defclass transform (component)
  ((%parent :accessor parent
            :initarg :parent
            :initform :universe)
   (%children :accessor children
              :initarg :children
              :initform nil)

   ;; Used for compute-physics
   (%data :accessor data
          :initarg :data
          :initform 0)))

(defun make-transform ()
  (make-instance 'transform))

(defun add-child (parent-transform child-transform)
  (pushnew child-transform (children parent-transform))
  (setf (parent child-transform) parent-transform))

(defun remove-child (parent-transform child-transform)
  (setf (children parent-transform)
        (remove-if (lambda (x) (eq x child-transform))
                   (children parent-transform))

        (parent child-transform) nil))

;; -----------------------------------------------------------------------

(defclass cursor-context ()
  ((%cursors :accessor cursors
             :initarg :cursors
             ;; key is keyword symbol, value is dlist cursor node.
             :initform (u:dict #'eq))))

(u:define-printer (cursor-context strm)
  (format strm "~(~S~)" (u:hash->plist (cursors cursor-context))))

(defun make-cursor-context (&rest cursors)
  ;; cursors are nodes (in some data structure) containing op/cursor
  ;; instances.
  (let ((ctx (make-instance 'cursor-context)))

    ;; TODO: A HACK to define a known cursor API, this must be made true later
    (setf (u:href (cursors ctx) :continuation) (first cursors))

    (dolist (cursor cursors)
      (setf (u:href (cursors ctx) (name cursor)) cursor))
    ctx))

(defun lookup-cursor (cursor-context name)
  ;; TEMPORARY FIX: For now, we check a constant list of cursor names
  (assert
   (find name '(:prologue :end-of-user-frame :recompilation :end-of-frame
                :mut-prefabs :mut-parenting :mut-aded :mut-destroy
                :continuation)))
  (u:href (cursors cursor-context) name))

(defun add-cursor (cursor-context cursor)
  (setf (u:href (cursors cursor-context) (name cursor)) cursor))

(defun remove-cursor (cursor-context cursor)
  (remhash (name cursor) (cursors cursor-context)))

;; -----------------------------------------------------------------------

(defclass op ()
  (;; Upon what does this operation perform its work?
   ;; It can be a Register (like Garden, Purgatory, Nursery) or
   ;; an instance of: Actor or Component
   (%domain :accessor domain
            :initarg :domain)
   (%modifier :accessor modifier
              :initarg :modifier
              :initform (constantly t))
   (%cursor-context :accessor cursor-context
                    :initarg :cursor-context
                    :initform nil)))
(defun op-p (op)
  (typep op 'op))


;; SR Flags affected: NONE
(defclass op/cursor (op)
  ((%location :accessor location
              :initarg :location)
   (%name :reader name
          :initarg :name)))
(defun op/cursor-p (op)
  (typep op 'op/cursor))

(u:define-printer (op/cursor strm)
  (format strm "~(~S~)" (name op/cursor)))

;; SR FLAGS AFFECTED: moe-p: T
(defclass op/bundle (op)
  ((%bundle :accessor bundle
            :initarg :bundle)))
(defun op/bundle-p (op)
  (typep op 'op/bundle))

(u:define-printer (op/bundle strm)
  (format strm "~(~S~) ~(~S~)"
          (domain op/bundle) (bundle op/bundle)))

;; SR Flags affected: moe-p: T / NIL depending on behavior.
(defclass op/construct-mutation-phase (op)
  ((%behavior :accessor behavior
              :initarg :behavior
              :initform :force))) ;; :force or :sense
(defun op/construct-mutation-phase-p (op)
  (typep op 'op/construct-mutation-phase))

(u:define-printer (op/construct-mutation-phase strm)
  (format strm "~(~S~)" (behavior op/construct-mutation-phase)))

;; SR Flags affected: moe-p: NIL
;; TODO: possibly this is unneeded
(defclass op/clear-mutation-phase (op) ())
(defun op/clear-mutation-phase-p (op)
  (typep op 'op/clear-mutation-phase))

;; SR Flags affected: NONE (so far)
(defclass op/compute-physics (op) ())
(defun op/compute-physics-p (op)
  (typep op 'op/compute-physics))

;; SR FLAGS AFFECTED: moe-p: T
(defclass op/compute-and-emit-collisions (op) ())
(defun op/compute-and-emit-collisions-p (op)
  (typep op 'op/compute-and-emit-collisions))

;; SR Flags affected: NONE
(defclass op/recompilations (op) ())
(defun op/recompilations-p (op)
  (typep op 'op/recompilations))

;; SR FLAGS AFFECTED: moe-p: T
(defclass op/make-component (op) ())
(defun op/make-component-p (op)
  (typep op 'op/make-component))

;; SR FLAGS AFFECTED: moe-p: T
(defclass op/make-actor (op) ())
(defun op/make-actor-p (op)
  (typep op 'op/make-actor))

;; SR FLAGS AFFECTED: moe-p: T
(defclass op/make-prefab (op) ())
(defun op/make-prefab-p (op)
  (typep op 'op/make-prefab))


;; TODO: Candidate to change all below to op/bundle
(defclass op/enable (op)
  ((%data :accessor data
          :initarg :data)))
(defun op/enable-p (op)
  (typep op 'op/enable))

(defclass op/disable (op)
  ((%data :accessor data
          :initarg :data)))
(defun op/disable-p (op)
  (typep op 'op/disable))

(defclass op/attach (op)
  ((%data :accessor data
          :initarg :data)))
(defun op/attach-p (op)
  (typep op 'op/attach))

(defclass op/detach (op)
  ((%data :accessor data
          :initarg :data)))
(defun op/detach-p (op)
  (typep op 'op/detach))



(defun make-op (op-type &rest args)
  (apply #'make-instance op-type args))

;; -----------------------------------------------------------------------

(defclass status ()
  (;; status flags and other similar data caused by executing ops.
   (%destroy-requested-p :accessor destroy-requested-p
                         :initarg :destroy-requested-p
                         :initform nil)
   (%reap-p :accessor reap-p
            :initarg :reap-p
            :initform nil)

   ;; Mutable Operation Expected
   ;; This means that an operation that will cause a future change to the
   ;; actors/components/scene tree has been put into the ops.
   (%moe-p :accessor moe-p
           :initarg :moe-p
           :initform nil)))

(u:define-printer (status strm)
  (format strm "des-req-p: ~A, reap-p: ~A, moe-p: ~A"
          (destroy-requested-p status)
          (reap-p status)
          (moe-p status)))

(defun make-status (&rest args)
  (apply #'make-instance 'status args))


;; -----------------------------------------------------------------------

(defclass act/comp-db ()
  ((%actors :reader actors
            :initform (u:dict #'eq))
   ;; TODO: Thees components need a real think about how to organize them.
   (%components :reader components
                :initform (u:dict #'eq))))

(defclass nursery (act/comp-db) ())

(u:define-printer (nursery strm)
  (format strm "actors: ~S components: ~S"
          (u:hash->plist (actors nursery))
          (u:hash->plist (components nursery))))



(defun make-nursery ()
  (make-instance 'nursery))

;;; Operations

;; Registers:
;; FC (frame Context: :end-of-frame, ...)
;; OP (executing op)
;; CC (executing operation's cursor context)
;;
;; N (Nursury) | Maybe RO for Registering Objects
;; G (Garden) | Maybe SO for Stable Objects
;; P (Purgatory) | Maybe DO for Deregistering Objects
;;
;; SR (status register, actually a clos instance, hold failed ops, number of
;; ops processed, profiling information for each op/bundle/etc number of ops
;; generated in the frame and what ops classes generated them, etc, etc)
;;
;; CR (Config register, "should I profile", "write frames to log", etc) Allow
;; an op/config that maybe the user can even invoke if they wanted to turn on
;; and off logging between operations in their code, etc, etc.
;;
(defclass quack ()
  ((%core :accessor core)
   ;; registers
   (%sr :accessor sr :initform nil) ;; status flags
   (%op :accessor op :initform nil) ;; operation currently executing
   (%cc :accessor cc :initform nil) ;; cursor context for executing operation
   (%mc :accessor mc :initform nil) ;; current mutation phase cursor context
   (%fc :accessor fc :initform nil) ;; frame cursor context
   (%nu :accessor nu :initform nil) ;; nursery

   ;; DList of ops to execute.
   (%ops :accessor ops
         :initform (dll:make-list))))

(u:define-printer (quack strm)
  (format strm "Ops: ~(~S~)" (ops quack)))

(defun make-quack ()
  (make-instance 'quack))

(defun init-sr-register (quack)
  (with-quack-registers (sr) quack
    (setf sr (make-status))
    quack))

(defun init-nu-register (quack)
  (with-quack-registers (nu) quack
    (setf nu (make-nursery))
    quack))

(defun init-fc-register (quack)
  ;; NOTE: We need this because we need valid initial cursor contexts that must
  ;; exist BEFORE any op is actually added to quack. Otherwise, we won't know
  ;; where to add the op into the OPS data structure!
  (with-quack-registers (ops fc) quack
    (assert (zerop (dll:length ops)))

    (let* ((cursor-prologue (make-op 'op/cursor :name :prologue))
           (cursor-eouf (make-op 'op/cursor :name :end-of-user-frame))
           (cursor-recomp (make-op 'op/cursor :name :recompilation))
           (cursor-eof (make-op 'op/cursor :name :end-of-frame))
           (new-fc (make-cursor-context cursor-eof cursor-eouf cursor-prologue
                                        cursor-recomp)))

      (setf
       (location cursor-prologue)
       (dll:insert ops cursor-prologue :where :before
                                       :target (dll:head ops))

       (location cursor-eouf)
       (dll:insert ops cursor-eouf :where :after
                                   :target (location cursor-prologue))

       (location cursor-recomp)
       (dll:insert ops cursor-recomp :where :after
                                     :target (location cursor-eouf))

       (location cursor-eof)
       (dll:insert ops cursor-eof :where :after
                                  :target (location cursor-recomp))

       fc new-fc)

      quack)))


;; NOTE: make cursor pool so we can reuse them without GC as much.

(defun emit-ops (edl msg)
  (format t "QUACK OPS: ~A~% HEAD~%~{  ~(~S~)~%~} HORIZON~%~%"
          msg
          (dll:list-values edl)))


(defun doit ()
  (let* ((core (make-core))
         (quack (quack core)))

    ;; Do ONCE--cannot be an operation.
    (setf (core quack) core)
    (init-sr-register quack)
    (init-nu-register quack)

    ;; Prolly do the below each frame.

    ;; Need a suitable frame cursor context immediately, can't be an op.
    ;; Well it could be, but it would always insert :before head....
    ;; START FRAME LOOP
    (init-fc-register quack)

    (op/recompilations quack)

    ;; The below operations need a mutation context where their side effects
    ;; will go when we get to processing them.
    (op/construct-mutation-phase quack :force)

    (op/compute-physics quack)
    (op/compute-and-emit-collisions quack)
    (op/bundle quack :garden 'physics-update)
    (op/bundle quack :garden 'update)


    (op/bundle quack :garden 'render)


    ;; Keep Going! (Namely, see if we need to add more mutation cursors
    ;; as long as there are ops possible to use them.)

    (emit-ops (ops quack) "Initial Op Set")

    (quack-execute quack)

    ;; END FRAME LOOP

    quack))

(defun quack-execute-op (quack)
  (with-quack-registers (op fc cc sr mc nu ops) quack
    (format t "========================================================~%")
    (format t " SR: ~S~% OP: ~S~% CC: ~S~% MC: ~S~% FC: ~S~% NU: ~S~%~%"
            sr op cc mc fc nu)

    (cond ;; Could be simplified in a data driven table or defmethods.
      ((op/cursor-p op)
       (execute-op/cursor quack))

      ((op/compute-physics-p op)
       (execute-op/compute-physics quack))

      ((op/compute-and-emit-collisions-p op)
       (execute-op/compute-and-emit-collisions quack))

      ((op/recompilations-p op)
       (execute-op/recompilations quack))

      ((op/construct-mutation-phase-p op)
       (execute-op/construct-mutation-phase quack))

      ((op/clear-mutation-phase-p op)
       (execute-op/clear-mutation-phase quack))

      ((op/bundle-p op)
       (execute-op/bundle quack))

      ((op/make-actor-p op)
       (execute-op/make-actor quack))

      ((op/make-component-p op)
       (execute-op/make-component quack))

      (t
       (error "Unknown op: ~S~%" op)))

    (format t "~%-- After Op Execution:~%~%")
    (emit-ops ops "Current Op Set")
    ))

(defun quack-execute (quack)
  (let ((ops (ops quack)))
    (loop :until (zerop (dll:length ops))
          :for node = (dll:head ops)
          :for op = (dll:value node)
          :do
             (setf (op quack) op
                   (cc quack) (cursor-context op))

             (dll:delete ops node)
             (quack-execute-op quack))))

;; ----------------------------------

(defun execute-op/cursor (quack)
  (with-quack-registers (fc op) quack
    (remove-cursor fc op)
    (format t " Removed cursor: ~(~S~)~%" (name op))))

;; ----------------------------------

(defun op/recompilations (quack)
  (let ((ops (ops quack))
        (fc (fc quack))
        (op (make-op 'op/recompilations)))

    (dll:insert ops op
                :where :before
                :target (location (lookup-cursor fc :recompilation)))
    quack))

(defun execute-op/recompilations (quack)
  (declare (ignore quack))
  (format t "Execute op/recompilations~%")
  )

;; ----------------------------------

(defun op/compute-physics (quack)
  (with-quack-registers (ops fc) quack
    (let ((new-op (make-op 'op/compute-physics)))

      (dll:insert ops new-op
                  :where :before
                  :target (location (lookup-cursor fc :end-of-user-frame)))
      quack)))

(defun execute-op/compute-physics (quack)
  (declare (ignore quack))
  (format t "Execute op/compute-physics~%")
  )

;; ----------------------------------

(defun op/compute-and-emit-collisions (quack)
  (with-quack-registers (ops fc) quack
    (let ((new-op (make-op 'op/compute-and-emit-collisions)))

      (dll:insert ops new-op
                  :where :before
                  :target (location (lookup-cursor fc :end-of-user-frame)))
      quack)))

(defun execute-op/compute-and-emit-collisions (quack)
  (with-quack-registers (sr) quack
    (format t "Execute op/compute-and-emit-collisions~%")

    ;; This generates calls into the user code, we can either assume the
    ;; mutation would happen, or have one of the op insertion calls in the API
    ;; below set it. Figure it out.
    (setf (moe-p sr) T)
    ))

;; ----------------------------------

;; TODO: Some of these should understand to poke CC first, and if that's nil,
;; then poke MC, and if that's nil, then use FC. I don't know which ops need
;; to know those distinctions yet.
(defun op/bundle (quack domain bundle)
  (with-quack-registers (ops fc) quack
    (let ((new-op (make-op 'op/bundle
                           :domain domain
                           :bundle bundle)))

      (dll:insert ops new-op
                  :where :before
                  :target (location (lookup-cursor fc :end-of-user-frame)))
      quack)))

(defun execute-op/bundle (quack)
  (with-quack-registers (fc op cc sr mc ops) quack
    (format t " Executing ~S bundle~%" (bundle op))
    (let ((bundle (bundle op))
          (context (context (core quack))))
      ;; TODO: Make this data driven to pick the bundle shit out of a
      ;; table and then execute it automatically on the supplied domain.
      (case bundle
        (physics-update
         nil)
        (update
         ;; act like user code in this bundle...
         (make-actor context)
         (make-component context 'transform)
         nil)
        (render
         nil)
        (t
         (format t " -> Unknown bundle: ~S~%" bundle)))

      ;; TODO: Should this be in the API below to notify the CPU when something
      ;; was ACTUALLY inserted into the mutable state?
      (setf (moe-p sr) T))))

;; ----------------------------------

(defun op/construct-mutation-phase (quack behavior)
  (with-quack-registers (ops fc) quack
    (let ((new-op (make-op 'op/construct-mutation-phase :behavior behavior)))

      (dll:insert ops new-op
                  :where :before
                  :target (location (lookup-cursor fc :end-of-user-frame)))
      quack)))

(defun execute-op/construct-mutation-phase (quack)
  (with-quack-registers (ops op sr fc mc) quack
    (format t "Execute op/construct-mutation-phase (behavior: ~(~S~))~%"
            (behavior op))

    ;; TODO: Check of behavior is :force or :sense. If force, just
    ;; force construct another mutation context and replace the one in
    ;; the register. If :sense, only do this if the SR indicates
    ;; (mutation requiring operations) have been executed since the
    ;; last creation of the mutation-phase context.
    (when (eq (behavior op) :sense)
      (when (null (moe-p sr))
        ;; If no mutable ops had executed, then clear the MC and we're done
        ;; constructing new mutation phases.
        (setf mc nil)
        (format t "Completed mutation phases!~%")
        (return-from execute-op/construct-mutation-phase nil)))

    (op/construct-mutation-phase quack :sense)

    (let* ((cursor-prefabs (make-op 'op/cursor :name :mut-prefabs))
           (cursor-parenting (make-op 'op/cursor :name :mut-parenting))
           (cursor-aded (make-op 'op/cursor :name :mut-aded))
           (cursor-destroy (make-op 'op/cursor :name :mut-destroy))
           (new-mc (make-cursor-context cursor-prefabs cursor-parenting
                                        cursor-aded cursor-destroy)))

      ;; push all mutation cursors to the correct place in the ops
      ;; TODO: Figure out real order and if there should be more or less
      ;; cursors in the mutation context.
      (setf
       (location cursor-prefabs)
       (dll:insert ops cursor-prefabs
                   :where :before
                   :target (location (lookup-cursor fc :end-of-user-frame)))

       (location cursor-parenting)
       (dll:insert ops cursor-parenting
                   :where :before
                   :target (location (lookup-cursor fc :end-of-user-frame)))

       (location cursor-aded)
       (dll:insert ops cursor-aded
                   :where :before
                   :target (location (lookup-cursor fc :end-of-user-frame)))

       (location cursor-destroy)
       (dll:insert ops cursor-destroy
                   :where :before
                   :target (location (lookup-cursor fc :end-of-user-frame)))
       )


      ;; set the mutation register to the newly created mutation context
      (setf mc new-mc
            (moe-p sr) NIL))))


;; ----------------------------------

(defun op/clear-mutation-phase (quack)
  (with-quack-registers (ops fc) quack
    (let ((new-op (make-op 'op/clear-mutation-phase)))

      (dll:insert ops new-op
                  :where :before
                  :target (location (lookup-cursor fc :end-of-user-frame)))
      quack)))

(defun execute-op/clear-mutation-phase (quack)
  (with-quack-registers (mc) quack
    (setf mc nil)))

;; ----------------------------------

(defun op/make-actor (quack actor)
  (with-quack-registers (ops sr cc mc fc) quack
    (let ((new-op (make-op 'op/make-actor :domain actor))
          (insert-cursor
            (cond
              (cc
               (lookup-cursor cc :continuation))  ;; don't have good API
              (mc
               (lookup-cursor mc :mut-prefabs))
              (fc
               (lookup-cursor fc :end-of-user-frame))
              (t
               (error "CC MC FC are not set!")))))

      (assert insert-cursor)

      (dll:insert ops new-op
                  :where :before
                  :target (location insert-cursor))

      (setf (moe-p sr) t)
      quack)))

(defun execute-op/make-actor (quack)
  (with-quack-registers (op nu) quack
    (let* ((actor (domain op)))
      (format t "Executing op/make-actor with domain ~S~%" actor)
      (setf (u:href (actors nu) actor) actor)
      )))

;; ----------------------------------

(defun op/make-component (quack component)
  (with-quack-registers (ops sr cc mc fc) quack
    (let ((new-op (make-op 'op/make-component :domain component))
          (insert-cursor
            (cond
              (cc
               (lookup-cursor cc :continuation))  ;; don't have good API
              (mc
               (lookup-cursor mc :mut-prefabs))
              (fc
               (lookup-cursor fc :end-of-user-frame))
              (t
               (error "CC MC FC are not set!")))))

      (assert insert-cursor)

      (dll:insert ops new-op
                  :where :before
                  :target (location insert-cursor))

      (setf (moe-p sr) t)
      quack)))

(defun execute-op/make-component (quack)
  (with-quack-registers (op nu) quack
    (let ((component (domain op)))
      (format t "Executing op/make-component with domain ~S~%" component)
      (setf (u:href (components nu) component) component)
      )))

;; ----------------------------------


;; NOTE: Left two examples of operations yet to be converted that push cursors
;; which will be set up in the CC cursor-context register in addition to the FC
;; (and possibly the MC (mutation context). Also, implementing these two will
;; show the interplay between the FC, CC, and MC registers.
(defun make-op/enable (quack insertion-cursor)
  (let* ((ops (ops quack))
         (insert-location (location insertion-cursor))
         (cursor (make-op 'op/cursor :name :enable))
         (cursor-context (make-cursor-context cursor))
         (op/enable (make-op 'op/enable
                             :domain "EEE"
                             :cursor-context cursor-context)))

    (dll:insert ops op/enable :where :before :target insert-location)

    (setf (location cursor)
          (dll:insert ops cursor
                      :where :before
                      :target insert-location))
    quack))

(defun make-op/disable (quack insertion-cursor)
  (let* ((ops (ops quack))
         (insert-location (location insertion-cursor))
         (cursor (make-op 'op/cursor :name :disable))
         (cursor-context (make-cursor-context cursor))
         (op/disable (make-op 'op/disable
                              :domain "DDD"
                              :cursor-context cursor-context)))

    (dll:insert ops op/disable :where :before :target insert-location)

    (setf (location cursor)
          (dll:insert ops cursor
                      :where :before
                      :target insert-location))
    quack))






;; Component requests
(defun make-component (context component-type
                       &optional (modifier (constantly nil)))
  (let* ((quack (quack (core context)))
         (comp (make-instance component-type
                              :context context
                              :state :pre-init
                              :ttl-p nil
                              :type component-type
                              :initializer modifier)))

    (op/make-component quack comp)
    comp))

(defmethod enable ((self component))
  nil)

(defmethod disable ((self component))
  nil)

(defmethod destroy ((self component))
  nil)

;; Actor
(defun make-actor (context)
  (let ((quack (quack (core context)))
        (actor (make-instance 'actor :state :pre-init)))
    (op/make-actor quack actor)
    actor))

(defun spawn-actor (actor)
  (declare (ignore actor))
  nil)

(defun reparent-actor (actor)
  (declare (ignore actor))
  nil)

(defmethod enable ((self actor))
  nil)

(defmethod disable ((self actor))
  nil)

(defmethod attach ((self actor) (component component))
  nil)

(defmethod detach ((self actor) (component component))
  nil)

(defmethod destroy ((self actor))
  nil)

(defun make-prefab (context prefab-name)
  (declare (ignore context prefab-name))
  nil)
