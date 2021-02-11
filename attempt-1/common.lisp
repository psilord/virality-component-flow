(in-package #:attempt-1)

(defun lexicographic/package-then-symbol-< (left right)
  (let* ((left-package-name (package-name (symbol-package left)))
         (right-package-name (package-name (symbol-package right)))
         (left-name (symbol-name left))
         (right-name (symbol-name right)))

    (let ((result
            (if (string= left-package-name right-package-name)
                (string< left-name right-name)
                (string< left-package-name right-package-name))))
      ;; Crunch it to boolean instead of geneeralized boolean.
      ;; Make prove tests more understandable.
      (if result t nil))))

(defun lexicographic/symbol-then-package-< (left right)
  (let ((left-package-name (package-name (symbol-package left)))
        (right-package-name (package-name (symbol-package right)))
        (left-name (symbol-name left))
        (right-name (symbol-name right)))

    (let ((result
            (if (string= left-name right-name)
                (string< left-package-name right-package-name)
                (string< left-name right-name))))
      ;; Crunch it to boolean instead of geneeralized boolean.
      ;; Make prove tests more understandable.
      (if result t nil))))


(defun set-equivalence-p (list1 list2 &key (key #'identity)
                                        (test #'eql))
  (not (set-exclusive-or list1 list2 :key key :test test)))
