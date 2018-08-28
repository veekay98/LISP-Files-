

;;;                   (solve-path '(w w w w) '(e e e e))
;;   (bfs '(w w w w) '(e e e e))

(defun bfs (goal state) (make-path2 goal state nil)) 
(defun solve-path (goal state) (make-path goal state nil))

(defun mlist (state) (list state))

(defun make-path2 (goal state step-list)
   (cond ((null state) nil)
	 ((equal state goal) (append  step-list (mlist state)))
	 ((not (member state step-list :test #'equal))
	      (or (make-path2 goal (farmer-takes-self state) (append  step-list (mlist state)))
	          (make-path2 goal (farmer-takes-wolf state) (append  step-list (mlist state)))
	          (make-path2 goal (farmer-takes-goat state) (append  step-list (mlist state)))
	          (make-path2 goal (farmer-takes-cabbage state) (append  step-list (mlist state)))))))

(defun make-path (goal state step-list)
   (cond ((null state) nil)
	 ((equal state goal) (reverse (cons state step-list)))
	 ((not (member state step-list :test #'equal))
	      (or (make-path goal (farmer-takes-self state) (cons state step-list))
	          (make-path goal (farmer-takes-wolf state) (cons state step-list))
	          (make-path goal (farmer-takes-goat state) (cons state step-list))
	          (make-path goal (farmer-takes-cabbage state) (cons state step-list))))))



(defun farmer-takes-self (state)
   (safe (make-state (opposite (farmer-side state))
		(wolf-side state)
	 	(goat-side state)
	 	(cabbage-side state))))


(defun farmer-takes-wolf (state)
   (cond ((equal (farmer-side state) (wolf-side state))
                     (safe (make-state (opposite (farmer-side state))
	                                        (opposite (wolf-side state))
	                                        (goat-side state)
	                                        (cabbage-side state))))
   	    (t nil)))

(defun farmer-takes-goat (state)
   (cond ((equal (farmer-side state) (goat-side state))
                  (safe (make-state (opposite (farmer-side state))
	                                     (wolf-side state)
	                                     (opposite (goat-side state))
	                                     (cabbage-side state)))) 
  	    (t nil)))

(defun farmer-takes-cabbage (state)
   (cond ((equal (farmer-side state) (cabbage-side state))
                    (safe (make-state (opposite (farmer-side state))
	                                       (wolf-side state)
	                                       (goat-side state)
	                                       (opposite (cabbage-side state)))))   
	   (t nil)))




(defun make-state (f w g c) (list f w g c))

(defun farmer-side ( state )
   (nth 0 state))

(defun wolf-side ( state )
   (nth 1 state))

(defun goat-side ( state )
   (nth 2 state))

(defun cabbage-side ( state )
   (nth 3 state))


(defun opposite (side)
   (cond ((equal side 'e) 'w)
             ((equal side 'w) 'e)))


(defun safe (state)
   (cond ((and (equal (goat-side state) (wolf-side state))
	             (not (equal (farmer-side state) (wolf-side state))))  nil)
            ((and (equal (goat-side state) (cabbage-side state))
	             (not (equal (farmer-side state) (goat-side state)))) nil)
	   (t state)))

  
