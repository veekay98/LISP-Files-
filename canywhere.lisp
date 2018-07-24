(defun count-anywhere (v1 ls)
    (   if (null ls)  0
      (
         if (listp (car ls))


                 (+ (count-anywhere v1 (car ls)) (count-anywhere v1 (cdr ls))  )
                        

            
             (if (equal v1 (car ls))
                   (+ (count-anywhere v1 (cdr ls)) 1)  
                   (+ (count-anywhere v1 (cdr ls)) 0)  
                                                        )
          
)


                       )
                             )
