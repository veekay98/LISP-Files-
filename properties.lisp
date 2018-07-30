(defun findmax (ls)
   ( 


(cond
((null ls) max)
(( > (car ls) max) (setf max (car ls)))

(t (findmax (cdr ls)))

)


     



)
)


(defun grandparent (name)(

cond ((equal (get name 'Parents) nil) nil)
      (t (cons (get name 'Parents) (grandparent (car (get name 'Parents)) ) ))) ))

(defun grandparent (name)(

get (car (get name 'Parents)) 'Parents
))

(defun femanc (name)(
cond ((equal (get (car(get name 'Parents)) 'Parents) nil) (cadr (get name 'Parents)))
      (t  (femanc (car (get name 'Parents)) ) )))




