(defun power (a b)
(cond ((<=  b 0) 1)
(t (* (power a (- b 1)) a))
))
