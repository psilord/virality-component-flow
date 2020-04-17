;;;; attempt-1.lisp

(in-package #:attempt-1)

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

(defun quack-enqueue (queue thing)
  nil)

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
  ;; op-cursors are nodes (in some data structure) containing op/cursor
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


(defclass op ()
  ((%domain :accessor domain
            :initarg :domain)
   (%cursor-context :accessor cursor-context
                    :initarg :cursor-context
                    :initform nil)))
(defun op-p (op)
  (typep op 'op))

(defclass op/cursor (op)
  ((%location :accessor location
              :initarg :location)
   (%name :reader name
          :initarg :name)))
(defun op/cursor-p (op)
  (typep op 'op/cursor))


(u:define-printer (op/cursor strm)
  (format strm "~(~S~)" (name op/cursor)))

(defclass op/compute-physics (op)
  ((%data :accessor data
          :initarg :data)))
(defun op/compute-physics-p (op)
  (typep op 'op/compute-physics))

(defclass op/compute-and-emit-collisions (op)
  ((%data :accessor data
          :initarg :data)))
(defun op/compute-and-emit-collisions (op)
  (typep op 'op/compute-and-emit-collisions))

(defclass op/physics-update (op)
  ((%data :accessor data
          :initarg :data)))
(defun op/physics-update-p (op)
  (typep op 'op/physics-update))

(defclass op/update (op)
  ((%data :accessor data
          :initarg :data)))
(defun op/update-p (op)
  (typep op 'op/update))

(defclass op/render (op)
  ((%data :accessor data
          :initarg :data)))
(defun op/render-p (op)
  (typep op 'op/render))

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

(defclass op/recompilations (op)
  ((%queue :accessor queue
           :initarg :queue)))
(defun op/recompilations-p (op)
  (typep op 'op/recompilations))

(defclass op/make-component (op)
  ((%factory :accessor factory
             :initarg :factory
             :initform (constantly nil))))
(defun op/make-component-p (op)
  (typep op 'op/make-component))


(defun make-op (op-type &rest args)
  (apply #'make-instance op-type args))


;; Registers:
;; FC (frame Context: :end-of-frame, ...)
;; OP (executing op)
;; CC (executing operation's cursor context)
;;
;; N (Nursury)
;; G (Garden)
;; P (prefab)
;;
;; NEXT-TIME: Tell the others about thiese ideas. -psilord
;; S (status register, actually a clos instance, hold failed ops, number of
;; ops processed, profiling information for each op/bundle/etc number of ops
;; generated in the frame and what ops classes generated them, etc, etc)
;; C (Config register, "should I profile", "write frames to log", etc)
;;
;; # microcode for a op/make-prefab exceution, etc.
;; set CC (cc O)
;; N = new nursury
;; run make-prefab phases.
;; merge N into G.
(defclass quack ()
  ((%fc :accessor fc)
   (%op :accessor op)
   (%cc :accessor cc)
   (%ops :accessor ops
         :initform (dll:make-list))))

(u:define-printer (quack strm)
  (format strm "Ops: ~(~S~)" (ops quack)))


(defun make-quack ()
  (make-instance 'quack))

;;         :initform (dll:make-list (make-op 'op/cursor :name :end-of-frame))
(defun init-quack (quack)
  (assert (zerop (dll:length (ops quack))))

  (let* ((ops (ops quack))
         (cursor (make-op 'op/cursor :name :end-of-frame))
         (fc (make-cursor-context cursor)))

    (setf (location cursor) (dll:insert ops cursor)
          (fc quack) fc)

    quack))



;;; Operations

;; Component requests
;; putative
(defun make-component (context component-type
                       &optional (factory (constantly nil)))

  (let* ((core (core context))
         (component (make-instance component-type :context context))
         (op (make-op 'op/make-component
                      :component component
                      :factory factory)))
    (quack-enqueue (quack context) op)
    ;; NOTE: This is a reference to the component only. It is not set up. The
    ;; user can only treat this return value as a reference and must not access
    ;; any slots in it _this frame_.
    component))



;; Next time:
;; Explore enter/exit events.
;; Actually emulate component/actor system.
;; Resolve the cursor-context and frame-cursors _key_ API. Should the
;; cursor-context be more ornate?
;; Implement a bundle with sorting on the phases.
;; Define the semantics and concrete understanding of _Domains_.
;; ENsure ordering between operations is what we believe we need.

;; NOTE: make cursor pool so we can reuse them without GC as much.

(defun doit ()
  (let ((quack (make-quack)))

    (init-quack quack)

    ;; test operation making.
    (let (eof eouf)
      (setf eof (lookup-cursor (fc quack) :end-of-frame))
      (make-op/end-of-user-frame quack eof)
      (make-op/recompilations quack eof)
      (setf eouf (lookup-cursor (fc quack) :end-of-user-frame))
      (make-op/compute-physics quack eouf eouf)
      (make-op/compute-and-emit-collisions quack eouf eouf)
      (make-op/physics-update quack eouf eouf)
      (make-op/update quack eouf eouf)
      (make-op/render quack eouf eouf)
      )

    (quack-execute quack)

    quack))

(defun quack-execute-op (quack)
  (let ((op (op quack))
        (fc (fc quack))
        (cc (cc quack)))
    (format t "====~%")
    (when (op/cursor-p op)
      (remove-cursor fc op)
      (format t " Removed cursor: ~(~S~)~%" (name op))
      (return-from quack-execute-op))

    (format t " OP: ~S~% CC: ~S~% FC: ~S~%"
            op cc fc)
    (cond
      ((op/physics-update-p op)
       ;; Hack to simulate someone calling enable in this phase
       (make-op/enable quack (lookup-cursor cc :continuation)))

      ((op/update-p op)
       ;; Hack to simulate someone calling enable in this phase
       (make-op/disable quack (lookup-cursor cc :continuation)))

      ((op/enable-p op)
       ;; Hack to simulate someone calling enable in this phase
       (make-op/disable quack (lookup-cursor cc :continuation)))

      (t
       nil))))

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


(defun make-op/end-of-user-frame (quack insertion-cursor)
  (let* ((ops (ops quack))
         (insert-location (location insertion-cursor))
         (cursor (make-op 'op/cursor :name :end-of-user-frame)))

    (setf (location cursor)
          (dll:insert ops cursor
                      :where :before
                      :target insert-location))

    (add-cursor (fc quack) cursor)

    quack))


(defun make-op/compute-physics (quack insertion-cursor continuation-cursor)
  (let* ((ops (ops quack))
         (insert-location (location insertion-cursor))
         (cursor-context (make-cursor-context continuation-cursor))
         (op/compute-physics (make-op 'op/compute-physics
                                      :domain "all garden comps"
                                      :cursor-context cursor-context
                                      :data "Hello world compute/physics")))

    (dll:insert ops op/compute-physics :where :before :target insert-location)

    quack))

(defun make-op/compute-and-emit-collisions
    (quack insertion-cursor continuation-cursor)

  (let* ((ops (ops quack))
         (insert-location (location insertion-cursor))
         (cursor-context (make-cursor-context continuation-cursor))
         (op/compute-and-emit-collisions
           (make-op 'op/compute-and-emit-collisions
                    :domain "all garden comps"
                    :cursor-context cursor-context
                    :data "Hello world compute/phys-coll")))

    (dll:insert ops op/compute-and-emit-collisions
                :where :before :target insert-location)

    quack))

(defun make-op/physics-update
    (quack insertion-cursor continuation-cursor)

  (let* ((ops (ops quack))
         (insert-location (location insertion-cursor))
         (cursor-context (make-cursor-context continuation-cursor))
         (op/physics-update
           (make-op 'op/physics-update
                    :domain "all garden comps"
                    :cursor-context cursor-context
                    :data "Hello world compute/phys-update")))

    (dll:insert ops op/physics-update
                :where :before :target insert-location)

    quack))

(defun make-op/update
    (quack insertion-cursor continuation-cursor)

  (let* ((ops (ops quack))
         (insert-location (location insertion-cursor))
         (cursor-context (make-cursor-context continuation-cursor))
         (op/update
           (make-op 'op/update
                    :domain "all garden comps"
                    :cursor-context cursor-context
                    :data "Hello world compute/update")))

    (dll:insert ops op/update
                :where :before :target insert-location)

    quack))

(defun make-op/render
    (quack insertion-cursor continuation-cursor)

  (let* ((ops (ops quack))
         (insert-location (location insertion-cursor))
         (cursor-context (make-cursor-context continuation-cursor))
         (op/render
           (make-op 'op/render
                    :domain "all garden comps"
                    :cursor-context cursor-context
                    :data "Hello world compute/render")))

    (dll:insert ops op/render
                :where :before :target insert-location)

    quack))

(defun make-op/recompilations (quack insertion-cursor)
  (let* ((ops (ops quack))
         (insert-location (location insertion-cursor))
         (cursor (make-op 'op/cursor :name :recompilation))
         (cursor-context (make-cursor-context cursor))
         (op/recompilations (make-op 'op/recompilations
                                     :domain "RRR"
                                     :cursor-context cursor-context
                                     :queue "Hello world :recompilations")))

    (dll:insert ops op/recompilations :where :before :target insert-location)

    (setf (location cursor)
          (dll:insert ops cursor
                      :where :before
                      :target insert-location))
    quack))

(defun make-op/enable (quack insertion-cursor)
  (let* ((ops (ops quack))
         (insert-location (location insertion-cursor))
         (cursor (make-op 'op/cursor :name :enable))
         (cursor-context (make-cursor-context cursor))
         (op/enable (make-op 'op/enable
                             :domain "EEE"
                             :cursor-context cursor-context
                             :data "Hello world :enable")))

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
                              :cursor-context cursor-context
                              :data "Hello world :disable")))

    (dll:insert ops op/disable :where :before :target insert-location)

    (setf (location cursor)
          (dll:insert ops cursor
                      :where :before
                      :target insert-location))
    quack))







(defmethod enable ((self component))
  nil)

(defmethod disable ((self component))
  nil)

(defmethod destroy ((self component))
  nil)

;; Actor requests
(defun make-actor (context)
  nil)

(defun spawn-actor (actor)
  nil)

(defun reparent-actor (actor)
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
