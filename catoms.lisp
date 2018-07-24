(defun count-atoms (ls)
   (if (null ls) 
     0
      (if (listp (car ls))
        (+ (count-atoms (cdr (car ls))) 1)  
        (+ (count-atoms (cdr ls)) 1)
)))

