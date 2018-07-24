(defun flatten (ls)
   (if (null ls) 
     '()
      (if (atom (car ls))
        
        (

if (equal (car ls) NIL)
(flatten (cdr ls))
(cons (car ls) (flatten (cdr ls)))) 



        (append (flatten (car ls)) (flatten (cdr ls)))  
)))

