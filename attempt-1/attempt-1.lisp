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
                :initform (u:dict))
   (%components-by-type :reader %components-by-type
                        :initform (u:dict))))

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

(defclass request ()
  ((%data :accessor data
          :initarg :data
          :initform (gensym "REQ-"))))

(defclass request/cursor (request) ())
(defclass request/t0 (request) ())
(defclass request/t1 (request) ())
(defclass request/t2 (request) ())

(defclass request/make-component (request)
  ((%component :accessor component
               :initarg :component
               :initform nil)
   (%initializer-factory :accessor initializer-factory
                         :initarg :initializer-factory
                         :initform (constantly nil))))

(defun make-request-factory (request-type &rest args)
  (apply #'make-instance request-type args))

;; Next time: Make some SIMPLE toy requests, build the quack defclass and
;; code which wraps the doubly linked list to build the datastructures, write
;; executor function that executes each request.

(defun execute-quack (quack)
  nil)


;; HEAD: (_: future event horizon )
;; [00]: _
;; HORIZON

;; Run a function to push stuff onto the quack.

;; HEAD: (_: future event horizon)
;; + [Y0] req/compute-physics (_A)
;; + [Y1] req/compute-and-emit-collisions (_A)
;; + [Y2] req/phase/physics-update (_A)
;; + [Y3] req/phase/update (_A)
;; + [Y4] req/phase/render (_A)
;; + [Y5] _A
;; + [Y6] req/recompilations (_B)
;; + [Y7] _B
;; [00] _
;; HORIZON

;; HEAD: (_: future, _B: end of frame, _A: end of user code)
;; - [Y0] req/compute-physics (_A) <nil requests>
;; - [Y1] req/compute-and-emit-collisions (_A) <nil requests>
;; - [Y2] req/phase/physics-update (_A) <generated X requests>
;; [Y3] req/phase/update (_A)
;; [Y4] req/phase/render (_A)
;; + [X0] req/make-prefab-instance (_C)
;; + [X1] _C
;; + [X2] req/enable <actor 0> (_D)
;; + [X3] _D
;; + [X4] req/disable <actor 0> (_E)
;; + [X5] _E
;; [Y5] _A
;; [Y6] req/recompilations (_B)
;; [Y7] _B
;; [00] _
;; HORIZON

;; HEAD: (_: future, _B: end of frame, _A: end of user code)
;; - [Y3] req/phase/update (_A) <generated W requests>
;; [Y4] req/phase/render (_A)
;; [X0] req/make-prefab-instance (_C)
;; [X1] _C
;; [X2] req/enable <actor 0> (_D)
;; [X3] _D
;; [X4] req/disable <actor 0> (_E)
;; [X5] _E
;; + [W0] req/make-component <comp 0> (_F)
;; + [W1] _F
;; + [W2] req/attach <actor 0 | comp 0> (_G)
;; + [W3] _G
;; [Y5] _A
;; [Y6] req/recompilations (_B)
;; [Y7] _B
;; [00] _
;; HORIZON

;; BR means "Beta reduction"
;; Vocabulary: "Frame context" is the ( ... ) list after HEAD.
;;             Any request can access.
;; Vocabulary: "Request context" is the ( ... ) for a _request_
;;             A set of cursors relavent to this specific request, provided by
;;             the maker of the request.
;; NOTE: A request can know the frame context and the current request context.
;; NOTE: This DOES preserve the _actual_ ordering of the requests as found in
;; the user's code.
;; =======================
;; HEAD: (_: future ...)
;; [X0] req/make-prefab-instance (_C_pre, C_v:bind, _C_default, _C_post, _C)
;; [X1] _C_pre
;; [X2] _C_v:bind
;; [X3] _C_default
;; [X4] _C_post
;; [X5] _C
;; [00] _
;; HORIZON
;; =======================
;;
;; =======================
;; HEAD: (_: future ...)
;; - [X0] req/make-prefab-instance (_C)
;; [X1] _C
;; [00] _
;; HORIZON
;; =======================

;; NOTE:
;; When the phase-order can be specified by the user, there are some special
;; V only symbols which represent actions of the engine that we do not want
;; to export to the user. So, here V:BIND means "The point at which the physical
;; assignment of the reference of the component into the actor happens." The
;; other keywords simply represent phases the user can hook into. These
;; V:BIND, etc keywords must NOT be overridable by the user. We can write
;; macros to help with the management of these keywords.
;;
;; (adjust-attach-phase-order '(:pre v:bind :default :post))

;; HEAD: (_: future, _B: end of frame, _A: end of user code)
;; - [Y4] req/phase/render (_A) <generated no requests>
;; - [X0] req/make-prefab-instance <prefab 1> <parent 1> (_C) <gen. V request>

;; + [V0] req/make-actor <actor 1> () <- This means cannot generate BR requests!
;; + [V1] req/make-component <comp 1> (_H)
;; + [V2] _H
;; + [V3] req/make-component <comp 2> (_I)
;; + [V4] _I

;; XXX
;; Leave this be until we determine if request contexts make sense and work.
;; XXX

;; + [V5] req/attach :pre <actor 1> <comp 1> (_J0)
;; + [V6] _J0
;; + [V7] req/attach :pre <actor 1> <comp 2> (_K0)
;; + [V8] _K0

;; + [V5] req/attach v:bind <actor 1> <comp 1> ()
;; + [V7] req/attach v:bind <actor 1> <comp 2> ()

;; + [V5] req/attach :default <actor 1> <comp 1> (_J1)
;; + [V6] _J1
;; + [V7] req/attach :default <actor 1> <comp 2> (_K1)
;; + [V8] _K1

;; + [V5] req/attach :post <actor 1> <comp 1> (_J2)
;; + [V6] _J2
;; + [V7] req/attach :post <actor 1> <comp 2> (_K2)
;; + [V8] _K2

;; + [V9] req/make-actor <actor 2> () <- This means cannot generate BR requests!
;; + [V10] req/make-component <comp 3> (_L)
;; + [V11] _L
;; + [V12] req/make-component <comp 4> (_M)
;; + [V13] _M
;; + [V14] req/attach <actor 2> <comp 3> (_N)
;; + [V15] _N
;; + [V16] req/attach <actor 2> <comp 4> (_O)
;; + [V17] _O

;; + [V18] req/reparent <actor 2> <actor 1>

;; + [V19] req/spawn-actor <actor 1> <parent 1> (_P)
;; + [V20] _P

;; [X1] _C
;; [X2] req/enable <actor 0> (_D)
;; [X3] _D
;; [X4] req/disable <actor 0> (_E)
;; [X5] _E
;; [W0] req/make-component <comp 0> (_F)
;; [W1] _F
;; [W2] req/attach <actor 0 | comp 0> (_G)
;; [W3] _G
;; [Y5] _A
;; [Y6] req/recompilations (_B)
;; [Y7] _B
;; [00] _
;; HORIZON



;; NOTE: Cursors when inserted are kept in order in a secondary data data
;; struture.

;;; Requests

;; Component requests
(defun make-component (context component-type
                       &optional (initializer-factory (constantly nil)))

  (let* ((core (core context))
         (component (make-instance component-type :context context))
         (req (make-request-factory 'request/make-component
                                    :component component
                                    :initializer-factory initializer-factory)))
    (quack-enqueue (quack context) req)
    ;; NOTE: This is a reference to the component only. It is not set up. The
    ;; user can only treat this return value as a reference and must not access
    ;; any slots in it _this frame_.
    component))

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

;; -----------------------------------------------------------------------
;; Random testing code.

(defun doit ()
  (let* ((dll (dll:list))
         ;; Keep track of these nodes for now.
         (cur-0 nil)
         (cur-1 nil))
    (flet ((emit-dll (edl msg)
             (format t "dlist: ~A~% HEAD~%~{  ~(~S~)~%~} HORIZON~%~%"
                     msg
                     (dll:list-values edl))))

      ;; TODO: I suggest allowing to specify a real dlist node for the
      ;; key, which would then sidestep the O(n) lookup. Then we can
      ;; keep references to the actual cursor dnodes in the dlist
      ;; in the "Request Context". This would vastly increase performance.


      (emit-dll dll "Empty")

      (setf cur-0 (dll:insert-before dll (dll:head dll) '(0 . :cursor)))
      (emit-dll dll "Insert before :head Cursor-0")

      (setf cur-1 (dll:insert-after dll (dll:tail dll) '(1 . :cursor)))
      (emit-dll dll "Insert after :tail Cursor-1")

      (dll:insert-before dll cur-0 :0-zero)
      (emit-dll dll "Queue before Cursor-0: :0-zero")

      (dll:insert-before dll cur-0 :0-one)
      (emit-dll dll "Queue before Cursor-0: :0-one")

      (dll:insert-before dll cur-1 :1-zero)
      (emit-dll dll "Queue before Cursor-1: :1-zero")

      (dll:insert-before dll cur-1 :1-one)
      (emit-dll dll "Queue before Cursor-1: :1-one")

      (dll:insert-before dll cur-1 :1-two)
      (emit-dll dll "Queue before Cursor-1: :1-two")

      (format t "Processing dll like a queue from head to horizon...~%")
      (format t "  HEAD~%")
      (loop :until (zerop (dll:length dll))
            :for node = (dll:head dll)
            :do (dll:delete (dll:head dll) dll)
                (format t "   Processed node: ~(~S~)~%" node))
      (format t "  HORIZON~%")


      )))
