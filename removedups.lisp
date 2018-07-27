(defun combine (item List)
 (if (member item List)
   List (cons item List)))

(defun eliminateDuplicates(L)
    (do
      ((M L) M)
      ((null L) M)
      (setq M (combine (car L) M))
      (setq L (cdr L))
))