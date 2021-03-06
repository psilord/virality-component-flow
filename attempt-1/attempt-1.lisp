;;;; attempt-1.lisp

(in-package #:attempt-1)




(defparameter *render-layers*
  (u:dict #'eq
          :background 10
          :player 11
          :foreground 12))

(defparameter *render-layers-inverse*
  (u:dict #'eql
          10 :background
          11 :player
          12 :foreground))


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

(let ((serial-num 0))
  ;; TODO: add a register/deregister to keep track of unused.
  (defun get-new-serial-number ()
    (prog1 serial-num
      (incf serial-num))))


;; TODO: Remove the APPEND methods and replace them with aggregating named
;; columns defined in each class. It COULD be a plist, or a hash table, or
;; something like that. We might use a PROGN method with an outside function
;; call/method to call the progn method and assemble all of the component
;; pieces of the sorting columns into a valid sorting key. The sorting columns
;; STILL need an ordering, even if they are stored in a hash table, etc.

(defgeneric sorting-key (sorter)
  (:method-combination append :most-specific-first))

(defgeneric (setf sorting-key) (new-val sorter))

(defgeneric sorting-comparator (sorter)
  (:method-combination append :most-specific-first))


;; TODO: Each inheritance layer of the sortable-mixin needs a key comparison
;; function to actually compare the keys and produce the -1 0 1 return code for
;; the sort. Then we use the right one for any particular type held in the
;; domain tree. This needs to be written. We may be able to get away with a
;; generic key sort function, but the items passed to it have to be the same
;; "impedance", meanings two keys from the SAME sorting inheritance layer. Sort
;; keys from different layers, say a sortable-mixin and a render order, are
;; incomparable.


(defclass sortable-mixin ()
  ((%list-of-avl-trees :accessor list-of-avl-trees)
   (%key :accessor key
         :initarg :key
         :initform (list (get-new-serial-number)))))


(defmethod sorting-key append ((sorter sortable-mixin))
  (key sorter))

(defmethod sorting-comparator append ((sorter sortable-mixin))
  `(,#'<))

;; ((diff (all transform) (all tranform-child-deep))
;;  -> (all render)
;;  -> (splice dagname)
;;  -> (all transform-child-deep) ) ;; trander

;; transform -> foo -> bar -> qux -> transform-child-deep -> A
;;                                                        -> B

;; Something the player can regularly use in their comonent simply to get a
;; sorted order for their component.
(defclass sortable-order (sortable-mixin)
  ((%sort-key :accessor sort-key
              :initarg :sort-key
              :initform 0)))

(defmethod sorting-key append ((sorter sortable-order))
  (key sorter))

(defmethod sorting-comparator append ((sorter sortable-order))
  `(,#'<))

;; no writer for sortable-mixin, can't adjust the serial number!

;; TODO: FIgure out a new key representation that acts like append but uses
;; eq or eql testing (like maybe each component of the append should go into a
;; bit range in a bigint?) [But this is expensive.]

;; TODO: Understand |3b|'s point of view for how to specify sorting order among
;; these sortable classes:
;;
;;<|3b|> even easier if you specify a bunch of them at once  [23:48]
;;<|3b|> (define-sort-order (sorting-mixin render-order render-order2
;;       render-order3) (:y/b1 :a x/b0 :b :s)) or whatever  [23:49]
;;<|3b|> or with ordering constraints
;;<|3b|> ordering constraints would let superclasses specify requirements so you
;;       couldn't accidentally swap :a and :b  [23:50]
;;<|3b|> and can't remove/move serial #  [23:51]
;;<|3b|> define-sort-order could introspect the listed classes somehow to see
;;       which columns they provide, then generate the union-sorting-columns
;;       methods, or whatever it ends up being internally  [23:52]
;;<|3b|> it could be an preallocated array in the instance, with a protocol to
;;       calculate the size of the array and the offsets of columns  [00:03]
;;<|3b|> then the sorting dsl would expand to code that knows the offsets
;;<|3b|> yeah, hash is probably better for prototyping  [00:04]
;;<|3b|> though thinking about it might just store the key permanently and have
;;       the overridable methods be "update-sorting-key" or whatever  [00:05]
;;<|3b|> oh yeah, that was another thing about ordering constraints instead of
;;       explicit dag, it could account for threading stuff  [00:06]


(defclass render-order (sortable-mixin)
  ((%render-key :accessor render-key
                :initarg :render-key
                :initform :background)))

(defmethod sorting-key append ((sorter render-order))
  (list "foobar" (u:href *render-layers* (render-key sorter))))

(defmethod (setf sorting-key) (new-val (sorter render-order))
  (setf (render-key sorter) new-val))

(defmethod sorting-comparator append ((sorter render-order))
  `(,#'string< ,#'<))




(defun higher-order-comparator (key-funcs)
  "Return a function that compares two key lists given the key functions for
each element in those lists."
  (lambda (seq0 seq1)
    (loop :for key :in key-funcs
          :for s0 :in seq0
          :for s1 :in seq1
          ;; considered s0 is strictly < than s1
          :when (funcall key s0 s1)
            :return t
          ;; considered s0 is strictly > than s1
          :when (funcall key s1 s0)
            :return nil
          ;; considered s0 is strictly = to s1
          ;; Defaults to nil...
          )))

;; testing junk
(defun doit-sort (key-funcs key0 key1 &key (key #'identity))
  (let ((box (list 0 0))
        (comparator (higher-order-comparator key-funcs)))
    (psetf (car box) key0
           (cadr box) key1)
    (sort box comparator :key key)))





(defclass component (kernel sortable-mixin)
  ((%type :reader component-type
          :initarg :type)
   (%actor :accessor actor
           :initarg :actor
           :initform nil)
   (%initializer :accessor initializer
                 :initarg :initializer
                 :initform nil)))

(defclass sorting-class-info ()
  ((%sorters :reader sorters
	     :initform (u:dict #'eq))
   (%defaults :reader defaults
	      :initform (u:dict #'eq))
   (%canon-sorting-specs :accessor canon-sorting-specs)
   (%linearization :accessor linearization)))

(defun make-sorting-class-info ()
  (make-instance 'sorting-class-info))

(defclass core ()
  ((%sorting-class-info :reader sorting-class-info
			:initform (make-sorting-class-info))
   (%scene-tree :reader scene-tree)
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

;; (all render)

(defclass render (component render-order)
  ;; filled in by the engine code.
  ())

(defclass render-order2 (render-order)
  ())

(defclass render2 (render render-order2)
  ())

(defclass render-order3 (render-order)
  ())

(defclass render3 (render render-order3)
  ())

;; compomnent:       :s
;; render:     :a :b :s
;; render2: :x :a :b :s
;; render3: :y :a :b :s

;; (union-sorting-columns <sortable-mixin> <sortable-mixin>)
;; -> (:a :b :s)

;; (union-sorting-columns <sortable-mixin> <render-order>)
;; -> (:x/B1 :a :b :s)

;; (union-sorting-columns <sortable-mixin> <render-order2>)
;; -> (:y/B0 :a :b :s)

;; (union-sorting-columns <render-order2> <render-order2>)
;; -> (:x :a :b :s)

;; (union-sorting-columns <render-order2> <render-order3>)
;; -> (:x/B1 :y/B0 :a :b :s)

;; (union-sorting-columns <render-order3> <render-order3>)
;; -> (:y :a :b :s)


;; A render   = (:y/B1 :x/B1 :a/24 :b/43 :s/436)
;; B render2b = (:y/B1 :x/22 :a/23 :b/43 :s/435)
;; C render2a = (:y/B1 :x/23 :a/23 :b/43 :s/434)
;; D render2c = (:y/B1 :x/23 :a/23 :b/43 :s/437)
;; E render3a = (:y/11 :x/B1 :a/10 :b/22 :s/223)



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

;; TODO: Implement me! (This may cause changes to the sorting API)
;; If something wants to sort we make an op that goes after all mutation
;; phases. This is so we don't adjust the sorting of the domains in the middle
;; of a set of mutation phases.
(defclass op/change-sort (op) ())
(defun op/make-change-sort-p (op)
  (typep op 'op/change-sort))


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

;; Requirements for act/comp-db
;; 1. Components must be segmented by type (according to typedag). HARD
;; 2. Store in sorting order. (draw order for example)
;; 3. We must enumerate the sorting keys that we're gonna support and how
;; those keys are represented.
;; 4. Figure out domain structure.

;; typeorder: <B C A F E D>
;; <ob/bundle: enable, domain Actor0, :root-to-leaves>

;; Traversal Discipline order of changes (properties of a bundle traversal)
;;
;; :layer/additive-root-start (0 1 2 10 3 4 5 6 7 8 9 11)
;; :layer/additive-leaf-start (9 11 7 8 3 4 5 6 1 2 10 0)
;; Additive order is thought of like (left-to-right, top-to-bottom) when
;; reading prefabs, and literal adding order of actors and components to
;; previously created actors, etc.
;; :additive-root-depth
;; :additive-root-breadth
;; :additive (purely executed in terms of when added to scene tree/actor)
;; :flat (components first in typedag order, then actors)
;; :additive-leaf-start
;;    (1. Take all leaves 2. sort by additive. 3 remove first. 4 repeat)
;; :additive-leaf-group-start
;;    (1. Take all leaves 2. sort by additive. 3 remove all leaves. 4 repeat)
;;
;; More explanation: Integers are actors added into a scene tree by the
;; cardinal order of the integer.
;;
;; Domain Descriptions in relation to Actor hierarchy.
;; (Note: lists after are additive-breadth.
;;         0
;;    1     2     10
;;  3  4   6   5
;;      7   8
;;    11     9
;; Additive breadth example below
;; (when a node is expanded, order by the additive order for that node
;; being expanded.)
;;
;; domain <0>: Actor0
;;  component0: A
;;  component1: A
;;  component2: B
;;  component3: C :first
;;  component4: D
;;  domain <1>: Actor1
;;    component5: A :default
;;    component8: B :default
;;    component7: C :third
;;    domain<2>: Actor2
;;      component6: B :default
;;      component9: C :second
;;      component10: F :default
;;  domain <3>: Actor3
;;    component11: A
;;    component12: B
;;    component13: E
;;    domain<4>: Actor4
;;      component14: B
;;      component15: C :second
;;      component16: F

;; :universe A B C D E F

;; 0 <- 1 <- 2

;;                    'A, 'B, 'C
;;              Z0   [77, 32, 23] <- priority queue? (heap as an array)
;;              Z1   ['B, 'C]
;;              Z2   ['C]


;;     0        1        2
;;     A        B        C
;;    f g      j k      l r

;; EXAMPLE 1
;; Limits are not only numbers but also links.
;; [X] is insertion order into a domain, assume [0] if unlabled.
;; When deleting something if the range is into the same subdomain, we can
;; delete more than one cookie as appropriate.
;;
;; domain0: Actor0
;;  @(L0) @(L8) @(L9) 10 @(@(L15)) 30 @(L40[0]) @(L40[2]) @(L48) 50 @(L60) 70 @(@L71))
;;  domain1: Actor1
;;   0 9 @(L15) 20 @(L25) 40[0] @(L40[1]) 48 60 @(L71) 80
;;   domain2: Actor2
;;    15 25 40[1] 71 75
;;  domain3:  Actor3
;;   8 40[2] 41

;; EXAMPLE 2
;;
;; Addition of components/domains only influences parents. The parent might
;; need to inspect its children, but (it appears) it doesn't need to change
;; them.
;;
;; domain 0: Actor 0
;;  100 @(L300) @(L699) @(@(L700)) @(L701) @(L900) @(L2000) 3000
;;  domain 1: Actor 1
;;   300 @(L700) 2000
;;   domain 2: Actor 2
;;    700 900
;;  domain 3: Actor 3
;;   699
;;  domain 4: Actor 4
;;   701 <- N links to left of new cookie can cause N splits(?)


;; domain -> entailment-tree

;; NOTE: NEED to figure out the last two ordering.
;; multicolumn sort order: typedag, sort-mixin, traversal-discipline


;; Entailment Description {x, y, z, ...} -> x entails y entails z entails ...
;; {actor0, actor3, actor4}
;; {actor0, actor1, actor2}


;; list of entry point domains into the domain of actor0 for each actor
;; (actor0 -> (actor0 (actor1 (actor2)) (actor3 (actor4)))
;;  actor1 -> (actor1 (actor2))
;;  actor2 -> (actor2)
;;  actor3 -> (actor3 (actor4))
;;  actor4 -> (actor4))

;; -------------------------------------------------------------

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multicolumn sort order: typedag, sort-mixin, traversal-discipline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The structure of sort-mixin is unknown currently, can the user add more
;; columns?

;; Example Domain: actor 1
;; Sort by column one: typedag sort (alphabetical type)
;;A : (5)
;; B : (8 6)
;; C : (7 9)
;; F : (10)

;; Sort by column two: sort-mixin (:default :second :third)
;; A : (:default: (5))
;; B : (:default: (8 6))
;; C : (:second: (9) :third (7))
;; F : (:default: (10))

;; Sort by column three: traversal-discipline (:layer/additive (as example))
;; td: is traversal-discipline type
;; A : (:default: (:td: 5))
;; B : (:default: (:td: (6 8))) ;; cause 6 was added before 8
;; C : (:second: (:td: (9)) :third (:td (7)))
;; F : (:default: (:td: (10)))

;; Final order is: 5 6 8 9 7 10

;; -------------------------------------------------------------

;; Example Domain: actor 2
;; A: nil
;; B: (6)
;; C: (9)
;; F: (10)

;; The second and third column sorts are noops, SO:

;; Final order is: 6 9 10.

;; -------------------------------------------------------------

;; least-entailed ( [5 8 7] [6 9 10]) most-entailed

;; CRITICAL WARNING: Ensure that the sorting final outcome of a inner domain
;; preserved in the final sort of a domain that entails it!!!!!

;; Hash table :
;; HT1 == COMPONENT-TYPE -> (TREE | SET)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implement this one, but only generate the domain trees on demand.  This
;; allows only the ones that need to exist by the user's code to actually
;; exist, and then if it becomes a problem we can understand the real use case.
;; After make-prefab if we happen to force domain tree construction, we can
;; know this and free it so the user doesn't have to pay it.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; U     CtU           : A B C D E G F H I J K L M N Z
;;  A0    A I N        : B C D E G F H J K L M Z
;;   A1    D L M       : B C E G F H J K Z
;;    B0   H J K B     : C E G F
;;     B1  C E G F     :
;;    B2   Z           :







;; 00 in subdomain       :: T 1 for in domain
;; 01 in current actor

;; 10 in some parent domain :: NIL 0 for not in domain
;; 11 not in domain

;;         01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;; U       Y' A  B  C  D  E  G  F  H  I  J  K  L  M  N  Z

;;         10 01 00 00 00 00 00 00 00 01 00 00 00 00 01 00
;;  A0    _Y  A' B  C  D  E  G  F  H  I' J  K  L  M  N' Z

;;         10 10 00 00 01 00 00 00 00 10 00 00 01 01 10 00
;;   A1   _Y _A  B  C  D' E  G  F  H _I  J  K  L' M'_N  Z

;;         10 10 01 00 10 00 00 00 01 10 01 01 10 10 10 11
;;    B0  _Y _A  B' C _D  E  G  F  H'_I  J' K'_L _M _N  Z

;;         10 10 10 01 10 01 01 01 10 10 10 10 10 10 10 11
;;     B1 _Y _A _B  C'_D  E' G' F'_H _I _J _K _L _M _N  Z

;;         10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 01
;;    B2  _Y _A _B _C _D _E _G _F _H _I _J _K _L _M _N  Z'

;; ---------------------------------------------------------------------

;;         1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
;; U       Y' A  B  C  D  E  G  F  H  I  J  K  L  M  N  Q  Z

;;         0  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
;;  A0    _Y  A' B  C  D  E  G  F  H  I' J  K  L  M  N' Q  Z

;;         0  0  1  1  1  1  1  1  1  0  1  1  1  1  0  1  1
;;   A1   _Y _A  B  C  D' E  G  F  H _I  J  K  L' M'_N  Q  Z

;;         0  0  1  1  0  1  1  1  1  0  1  1  0  0  0  0  0
;;    B0  _Y _A  B' C _D  E  G  F  H'_I  J' K'_L _M _N  Q  Z

;;         0  0  0  1  0  1  1  1  0  0  0  0  0  0  0  0  0
;;     B1 _Y _A _B  C'_D  E' G' F'_H _I _J _K _L _M _N  Q  Z

;;         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  1
;;    B2  _Y _A _B _C _D _E _G _F _H _I _J _K _L _M _N  Q' Z'


;; universe
;;  parallax
;;  7 objects
;;  level
;;   floors
;;    400 objects
;;   walls
;;    400 objects
;;   corners
;;    400 objects
;;   pickups
;;    health
;;     dozens objects
;;    medicine-boxes
;;     dozens objects
;;   decorations/fringe
;;    houseplants
;;    windows
;;   traps
;;    100
;;  lights
;;   ~200 objects
;;  player
;;   5 deep

;; U Ct0
;;  A Ct1[s] Mesh0 Render0
;;   B Ct2 Mesh1 Render1
;;   C Ct3 Mesh2 Render2
;;    D Ct4 Mesh3 Render3

;; A Ct1' Mesh0 Render0 Mesh1 Render1 Mesh2 Render2 Mesh3 Render3


;; U C0
;;  A C1
;;   B C2
;;    C C3
;;   D C4
;;  E C5 / [C7 C6 C5 C8]
;;   F C6
;;   G C7
;;   H C8

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
    ;; TODO: This needs to know which register to remove the cursor from!
    ;; Currently there is possibility that a cursor with the same name in
    ;; another cursor context can illegally be removed from fc.
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
