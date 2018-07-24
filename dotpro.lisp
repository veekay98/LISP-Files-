(defun dot-product (l1 l2)
(
    if (null l1)
      0
      (+ (dot-product (cdr l1) (cdr l2)) (* (car l1) (car l2))) 




)
)
