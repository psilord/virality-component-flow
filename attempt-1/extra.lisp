




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
      (format t "  AT HORIZON~%")


      )))




;; HEAD: (_: future event horizon )
;; [00]: _
;; HORIZON

;; Run a function to push stuff onto the quack.

;; HEAD: (_: future event horizon)

;; + [P0] _prologue
;; + [Y0] op/set-mutation-phase 1
;; + [Y1] op/compute-physics (_A)   <----------------------------- probable fb
;; + [Y2] op/compute-and-emit-collisions (_A)
;; + [Y3] op/phase/physics-update (_A)
;; + [Y4] op/phase/update (_A)
;; + [Y5] op/phase/render (_A)
;; --------------------------------------- gamedev stable fb



;; Begin Mutation Phase 1

;; op/make-prefab-instance <foobar>
;; op/make-prefab-instance <bar>
;; _1_MakePrefab
;; op/reparent foobar qux
;; op/reparent bar qux
;; _1_Parenting
;; op/enable <actor 2>
;; op/disable <comp1>
;; op/deregister <comp1>
;; op/disable <comp2>
;; op/deregister <comp2>
;; _1_Attach/Detach/Enable/Disable

;; Status flags: destroy-requested-p reap-p
;; op/destroy <actor1> [op/destroy sets the reap-p flag in status register]
;; op/destroy <actor2>
;; op/reap-souls purgatory
;; _1_Destroy

;; End Mutation Phase 1


;; Begin Mutation Phase 2
;; _2_MakePrefab
;; _2_Parenting
;; _2_Attach/Detach/Enable/Disable
;; op/%destroy <actor3>
;; op/%destroy <actor1>
;; op/%destroy <actor2>
;; op/reap-souls purgatory [if destroy-requested-p then clear it, set reap-p]
;; _2_Destroy
;; End Mutation Phase 2

;; Begin Mutation Phase 3
;; _3_MakePrefab
;; _3_Parenting
;; _3_Attach/Detach/Enable/Disable
;; op/%%destroy <actor3-comp1>
;; op/%%destroy <actor3-comp2>
;; op/%%destroy <actor3>
;; op/%%destroy <actor1-comp1>
;; op/%%destroy <actor1>
;; op/%%destroy <actor2-comp1>
;; op/%%destroy <actor2>
;; op/reap-souls purgatory [if destroy-requested-p then clear it, set reap-p]
;; _3_Destroy
;; End Mutation Phase 3


;; Begin Mutation Phase 4
;; _4_MakePrefab
;; _4_Parenting
;; _4_Attach/Detach/Enable/Disable
;; op/reap-souls purgatory [if not destroy-requested-p then push ops & bundles & op/actually..., set reap-p]
;; _4_Destroy
;; End Mutation Phase 4


;; Begin Mutation Phase 5
;; _5_MakePrefab
;; _5_Parenting
;; [The below is generated by op/reap-souls]
;; op/deregister purgatory
;; op/compute-and-emit-deregistered-collision
;; op/disable purgatory
;; op/detach purgatory
;; _5_Attach/Detach/Enable/Disable
;; op/actually-reap-and-destroy purgatory [clear destroy-requested-p, clear reap-p]
;; _5_Destroy
;; End Mutation Phase 5

;; Begin Mutation Phase 6
;; op/make-prefab-instance <explosion>
;; _6_MakePrefab
;; _6_Parenting
;; _6_Attach/Detach/Enable/Disable
;; _6_Destroy
;; End Mutation Phase 6


;; and so on...

;; + [Y6] op/garbage-collect
;; + [Y7] req/recompilations (_B)
;; + [Y8] _B
;; ------------------------------------------------ actual fb
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
;; [X0] req/make-prefab-instance P0 (_C_pre, C_v:bind, _C_default, _C_post, _C)
;; [X1] _C_pre
;; [X2] _C_v:bind
;; [X3] _C_default
;; [X4] _C_post
;; [X5] _C
;; [Y0] req/make-prefab-instance P1 (_C_pre, C_v:bind, _C_default, _C_post, _C)
;; [Y1] _C_pre
;; [Y2] _C_v:bind
;; [Y3] _C_default
;; [Y4] _C_post
;; [5] _C
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
;; (adjust-phase-order 'attach '(:pre v:bind :default :post))

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


;; -----------------------------------------------
;; update
;; _prefab
;; op/make-prefab A ;; start of domain F
;; op/make-prefab B
;; op/make-prefab C ;; end of domain F
;; pre/default/post for multiple bundles in domain F <-
;; move nursery to garden
;; op/enable B
;; op/disable B
;; domain G
;; op/make-prefabs ....
;; domain G
;; _continuation

;; Notes about the op/prolog operation:
;; This particular operation will set up a nursery, cursors, etc, such that if
;; the user creates multiple actors and components they are all considered to be
;; in the SAME domain for the purposes of bundle and future operation execution.
;; After the user prologue function completes, then the operations execute on
;; the "prologue domain"--so across all actors and components in that domain
;; as a whole, as much as possible.


;; If v:enable is a macro that generates

;; (defmethod update ((self ...))
;;  (make-component 'foobar
;;                (lambda (foobar)
;;                  (setf (thingy foobar) self))))

;;(funcall (v:enable compl
;;         function-body))

;; Domains:
;;

;; NOTE: Cursors when inserted are kept in order in a secondary data data
;; struture.



;; -----------------------------------------------------------------------
;; Random testing code.

;; NOTICE: An operation has a scope about who it is going to affect (an actor
;; and it components and it descendants recursively (in typedag order) OR *all*
;; components in typedag order) _and_ operations are themselves ordered.




;; -----------------------------------------------------------------------




;; Next time:
;; Complete nursery and >>> component sorting <<<.
;; Implement bundle definition and storage and execution.
;; Implement attach/detach
;; Implement reparent
;; Implement enable/disable
;; Simulate make-prefab first and the nursery.
;; Don't forget about adding maximum mutation phase limit.
;;
;; Complete understanding of destroy operation.
;; Explore enter/exit events with the bundle system.

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


;; Tentative Bundle and behavior specifications
;;
;; (v:define-bundle :enable
;;    ((:register (:network :audio :collision))
;;     :pre
;;     :default
;;     (thingy ())
;;     :post))
;;
;;(v:define-bundle :disable
;;    (:pre
;;     :default
;;     :post
;;     (:deregister (:collision :audio :network))))
;;
;;(v:define-bundle foobar
;;    (:pre
;;     :default
;;     :post
;;     (qux (:default))))
;;
;; user writes on their component.
;;(v:define-behavior :enable (:register :collision) ((self comp-type))
;;  nil)
;;
;;(v:define-behavior :enable (thingy) ((self comp-type))
;;  nil)
;;
;;(v:define-behavior :enable :default ((self comp-type))
;;  nil)
;;
;;(v:define-behavior foobar (qux :default) ((self comp-type))
;;  nil)
;;
;; user defined bundle for their own game.
;;(define-bundle-order foo:thingy
;;    (:pre :default :post))
