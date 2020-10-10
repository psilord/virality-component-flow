(in-package #:proj)

(v:define-sorting-classes proj ()
  ;; -> p i are implicit and on the right for each definition
  (sort/foo (v:sort/base) ((z :sorter < :default 0)
                           (pp :sorter < :default 0)))
  (sort/qux (sort/foo) ((d :sorter < :default 0)
                        (e :sorter blah :default :bottom)
                        z
                        (f :sorter > :default 0)
                        pp))
  (sort/feh (v:sort/base) ((r :sorter < :default 0)
                           (s :sorter > :default 0)
                           (k :sorter < :default 0)
                           (l :sorter < :default 0)))
  (sort/meh (sort/feh sort/foo) (r
                                 s
                                 k
                                 (h :sorter < :default 0)
                                 z
                                 l))
  (sort/bar (sort/foo) ((a :sorter < :default 0)
                        z
                        (b :sorter < :default 0)
                        (c :sorter > :default 0)))
  (sort/my-render-layer (v:sort/render-layer) ((zzz :sorter < :default 0))))


;; example comonent using sorting classes.
(v:define-component foobar (sort/feh)
  ())

;; Toplevel interface to alter core V functionality of sorting class info
;; This is probably a macro to insert this information into the meta table
;; for the sorting classes. This column MUST have been previously seen before
;; this macro can be used otherwise it is an error.
;; In the case of user code, all of V would have been laoded before the user
;; code gets loaded, so usually not a problem. In the case of contribs, the
;; contribs can only see the previously loaded contribs that came before it.
;; The user code should be loaded after all contribs to make final decisions.
(v:define-column-sorter v:render-layer blah ;; a function name
  :default :foo
  :converter converter) ;; a function name

;; Runtime API into V (or any previous sorting class definition)
;; to change sorting column comparator. This goes into the recompilation queue
;; so at the end of the frame, the objects are all ripped out of the sorting
;; tree the column function updated, then reinserted back into the tree.
;; This is a function.
(v:override-column-sorter 'v:render-layer 'blah ;; a function name
                          :default :foo
                          :converter 'converter) ;; a function name




(u:define-constant +blah+ (u:dict :foo 0
                                  :bar 1
                                  :thing 2
                                  :qux 3)
  :test #'equalp)

(defun blah (left-col-val right-col-val)
  (< (u:href +blah+ left-col-val)
     (u:href +blah+ right-col-val)))


(defun converter (x)
  (identity x))
