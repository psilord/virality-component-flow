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
           :initform :initialize)
   (%ttl-p :accessor ttl-p
           :initarg :ttl
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
   (%tables :reader tables
            :initform nil)
   (%quack :reader quack
           :initarg :quack
           :initform nil)))

(defclass context ()
  ((%core :reader core
          :initarg :core)))

(defun make-context (core)
  (setf (slot-value core '%context) (make-instance 'context :core core)))

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

;; SR FLAGS AFFECTED: mutable-ops-executed-p: T
(defclass op/bundle (op)
  ((%bundle :accessor bundle
            :initarg :bundle)))
(defun op/bundle-p (op)
  (typep op 'op/bundle))

(u:define-printer (op/bundle strm)
  (format strm "~(~S~) ~(~S~)"
          (domain op/bundle) (bundle op/bundle)))

;; SR Flags affected: mutable-ops-executed-p: T / NIL depending on behavior.
(defclass op/construct-mutation-phase (op)
  ((%behavior :accessor behavior
              :initarg :behavior
              :initform :force))) ;; :force or :sense
(defun op/construct-mutation-phase-p (op)
  (typep op 'op/construct-mutation-phase))

;; SR Flags affected: mutable-ops-executed-p: NIL
;; TODO: possibly this is unneeded
(defclass op/clear-mutation-phase (op) ())
(defun op/clear-mutation-phase-p (op)
  (typep op 'op/clear-mutation-phase))

;; SR Flags affected: NONE (so far)
(defclass op/compute-physics (op) ())
(defun op/compute-physics-p (op)
  (typep op 'op/compute-physics))

;; SR FLAGS AFFECTED: mutable-ops-executed-p: T
(defclass op/compute-and-emit-collisions (op) ())
(defun op/compute-and-emit-collisions-p (op)
  (typep op 'op/compute-and-emit-collisions))

;; SR Flags affected: NONE
(defclass op/recompilations (op) ())
(defun op/recompilations-p (op)
  (typep op 'op/recompilations))

;; SR FLAGS AFFECTED: mutable-ops-executed-p: T
(defclass op/make-component (op) ())
(defun op/make-component-p (op)
  (typep op 'op/make-component))

;; SR FLAGS AFFECTED: mutable-ops-executed-p: T
(defclass op/make-actor (op) ())
(defun op/make-actor-p (op)
  (typep op 'op/make-actor))

;; SR FLAGS AFFECTED: mutable-ops-executed-p: T
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
   ;; TODO: Reconcile this and below into one flag
   (%mutable-ops-executed-p :accessor mutable-ops-executed-p
                            :initarg :mutable-ops-executed-p
                            :initform nil)
   ;; Did any ops (of a certain subset of ops) _complete_ execution?
   (%ops-executed-p :accessor ops-executed-p
                    :initarg :ops-executed-p
                    :initform nil)))

(u:define-printer (status strm)
  (format strm "des-req-p: ~A, reap-p: ~A, ops-exec-p: ~A, mut-ops-ex-p: ~A"
          (destroy-requested-p status)
          (reap-p status)
          (ops-executed-p status)
          (mutable-ops-executed-p status)))

(defun make-status (&rest args)
  (apply #'make-instance 'status args))




;;; Operations

;; Component requests
;; putative

(defun quack-enqueue (thing stuff)
  (declare (ignore thing stuff))
  nil)

(defun make-component (context component-type
                       &optional (modifier (constantly nil)))

  (let* (#++(core (core context))
         (component (make-instance component-type :context context))
         (op (make-op 'op/make-component
                      :component component
                      :modifier modifier)))
    (quack-enqueue (quack context) op)
    ;; NOTE: This is a reference to the component only. It is not set up. The
    ;; user can only treat this return value as a reference and must not access
    ;; any slots in it _this frame_.
    component))



;; Next time:
;; Make a stepper debugger interface to quack.
;; Ensure we don't push deregister op when no disables are in place, etc, etc?
;; New registers: Status, Nursery, Purgatory, Mutation Context
;; New status register flags: destroy-requested-p, reap-p
;; Do we need a "Next Phase" op & register?
;;   Or is putting in current op good enough?
;; Reify mutation phases into a cursor context, so the current cursor context
;; of a operation knows which _next_ phase it should dump its operations into
;; (including the known names for those cursors in the context).
;; Complete understanding of destroy operation.
;; Simulate destroy before the other similar ones like make-prefab-instance.
;; Explore enter/exit events.
;; Implement lambda operation and lambda closures for operations.
;; Possibly merge v:enable and v:enable-register in a better way.
;;
;; Actually emulate component/actor system.
;; Resolve the cursor-context and frame-cursors _key_ API. Should the
;; cursor-context be more ornate?
;; Implement a bundle with sorting on the phases.
;; Define the semantics and concrete understanding of _Domains_.
;; ENsure ordering between operations is what we believe we need.
;;
;; <psilord> Wow! I just realized we can build a gdb like interface for
;;        quack. Like, you can next over an operation, or step into one and
;;        watch it call the bundles on all the components, etc, etc,
;;        etc. "break on any phase emitting an disable" "break when attempting
;;        to enable >this< actor.   [01:09]
;;
;; <psilord> Theoretically, we can have an honest to god interrupt vector
;;        specification. Like, if ops failed to execute (in a BAD way), then
;;        call this user function that isn't a part of any actor or component.
;;                                                                      [01:12]
;; possibly merge some of this together.

;; INSPECT CODE
;;(define-bundle-order 'enable
;;    ((register (:network :audio :collision))   <- contextual bundle
;;     :pre v:default :post))
;;
;;(define-bundle-order 'enable-register <- maybe replace with contextual bundle
;;    (:collision :audio :network))
;;
;;(define-bundle-order 'disable-deregister <- maybe replace with context bundle
;;    (:network :audio :collision))
;;
;;(define-bundle-order 'disable
;;    (:pre v:default :post
;;     (deregister (:collision :network :audio)))) <- contextual bundle
;;
;;
;;(define-bundle-order 'attach
;;    ((register (:network :audio :collision)) <- contextual bundle
;;     :pre v:default :post))
;;
;;(define-bundle-order 'attach-register <- maybe replace with "context bundle"
;;    (:collision :audio :network))
;;
;;(define-bundle-order 'detach-deregister <- maybe replace w/ "context bundle"
;;    (:network :audio :collision))
;;
;;(define-bundle-order 'detach
;;    (:pre v:default :post
;;     (deregister (:collision :audio :network))) <- contextual bundle
;;
;;(define-contextual-behavior v:enable v:register :collision
;;                            ((self sphere) details)
;;  (col::deregister-collider (v:context self) self))

;;(define-contextual-behavior v:enable v:deregister :collision
;;                            ((self sphere) details)
;;  (col::deregister-collider (v:context self) self))



;;(defun v:disable (comp)
;;  (let ((context (content comp))
;;      (op/disable (make-op 'disable ......))
;;      (op/deregister (make-op 'disable-deregister ......)))
;;    (insert-op op/disable :continuation)
;;    (insert-op op/deregister :conintuation)))
;;
;;(defun v:detach (comp)
;;  (let ((context (content comp))
;;      (op/disable (make-op 'detach ......))
;;      (op/deregister (make-op 'detach-deregister ......)))
;;    (insert-op op/disable :continuation)
;;    (insert-op op/deregister :conintuation)))

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
  (;; registers
   (%fc :accessor fc :initform nil) ;; frame cursor context
   (%op :accessor op :initform nil) ;; operation currently executing
   (%cc :accessor cc :initform nil) ;; cursor context for executing operation
   (%sr :accessor sr :initform nil) ;; status flags

   (%mc :accessor mc :initform nil) ;; current mutation phase cursor context

   ;; DList of ops to execute.
   (%ops :accessor ops
         :initform (dll:make-list))))

(u:define-printer (quack strm)
  (format strm "Ops: ~(~S~)" (ops quack)))

(defun make-quack ()
  (make-instance 'quack))


(defun init-fc-register (quack)
  ;; NOTE: We need this because we need valid initial cursor contexts that must
  ;; exist BEFORE any op is actually added to quack. Otherwise, we won't know
  ;; where to add the op into the OPS data structure!
  (with-quack-registers (ops fc) quack
    (assert (zerop (dll:length ops)))

    (let* ((cursor-prologue (make-op 'op/cursor :name :prologue))
           (cursor-recomp (make-op 'op/cursor :name :recompilation))
           (cursor-eouf (make-op 'op/cursor :name :end-of-user-frame))
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

(defun init-sr-register (quack)
  (with-quack-registers (sr) quack
    (setf sr (make-status))
    quack))

;; NOTE: make cursor pool so we can reuse them without GC as much.

(defun emit-ops (edl msg)
  (format t "QUACK OPS: ~A~% HEAD~%~{  ~(~S~)~%~} HORIZON~%~%"
          msg
          (dll:list-values edl)))


(defun doit ()
  (let ((quack (make-quack)))

    ;; Do ONCE--cannot be an operation
    (init-sr-register quack)

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
  (with-quack-registers (op fc cc sr mc ops) quack
    (format t "========================================================~%")
    (format t " SR: ~S~% OP: ~S~% CC: ~S~% MC: ~S~% FC: ~S~%~%"
            sr op cc mc fc)

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

      (t
       (format t "Unknown op: ~S~%" op)
       nil))

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
    (setf (mutable-ops-executed-p sr) T)
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
    (let ((bundle (bundle op)))
      ;; TODO: Make this data driven to pick the bundle shit out of a
      ;; table and then execute it automatically on the supplied domain.
      (case bundle
        (physics-update
         nil)
        (update
         nil)
        (render
         nil)
        (t
         (format t " -> Unknown bundle: ~S~%" bundle)))

      ;; TODO: Should this be in the API below to notify the CPU when something
      ;; was ACTUALLY inserted into the mutable state?
      (setf (mutable-ops-executed-p sr) T))))

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
      (when (null (mutable-ops-executed-p sr))
        ;; If no mutable ops had executed, then clear the MC and we're done
        ;; constructing new mutation phases.
        (setf mc nil)
        (format t "Completed mutation phases!~%")
        (return-from execute-op/construct-mutation-phase nil)))


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

      ;; TODO: Should this actually go first or something like it?
      ;;
      ;; And push the op which clears the mutation context right now. so it
      ;; ends up in the right place. TODO: We need some sort of decision op to
      ;; decide if another mutation phase is warranted.  Probably something
      ;; gets set in SR that indicates there were ops that could need
      ;; it. Maybe, we have an op JUST BEFORE the mutation phase which clears a
      ;; flag, then ops set the flag, then another one that creates another
      ;; mutation phase of the flag was set, and doesn't if cleared. THat meant
      ;; there were no operations in the mutation phase so the generation can
      ;; stop.

      (op/construct-mutation-phase quack :sense)

      ;; set the mutation register to the newly created mutation context
      (setf mc new-mc
            (mutable-ops-executed-p sr) NIL))))


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
(defmethod enable ((self component))
  nil)

(defmethod disable ((self component))
  nil)

(defmethod destroy ((self component))
  nil)

;; Actor requests
(defun make-actor (context)
  (declare (ignore context))
  nil)

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
