: Best First Search solution to the 8 Puzzle Problem 

; Function to create a 2D array from a list 
(defun list-array (ls)
	(make-array '(3 3) 
		:initial-contents ls))

; Function to find position of the blank
(defun find-blank (state)
	(dotimes (i 3)
		(dotimes (j 3)
			(if (= (aref state i j) 0) 
				(progn (return-from find-blank (list i j)))))))

; Function to check if two arrays are equal
(defun equal-array (a1 a2)
	(dotimes (i 3)
		(dotimes (j 3)
			(if (/= (aref a1 i j) (aref a2 i j)) 
				(progn (return-from equal-array nil))))) t)

; Functions to move the blank up,down,left and right
  
(defun move-up (state)
	(if (/= 0 (car (find-blank state)))
			(progn (setq p1 (find-blank state))
				(setq i (- (car p1) 1))
				(setq j (cadr p1))
				(setq x (aref state i j))
				(setq p2 (list i j))
				(update state x p1 p2)) state))

(defun move-down (state)
	(if (/= 2 (car (find-blank state)))
		(progn (setq p1 (find-blank state))
			(setq i ( + (car p1) 1))
			(setq j (cadr p1))
			(setq x (aref state i j))
			(setq p2 (list i j))
			(update state x p1 p2)) state))

(defun move-left (state)
	(if (/= 0 (cadr (find-blank state)))
		(progn (setq p1 (find-blank state))
			(setq i (car p1))
			(setq j (- (cadr p1) 1))
			(setq x (aref state i j))
			(setq p2 (list i j))
			(update state x p1 p2)) state))

(defun move-right (state)
	(if (/= 2 (cadr (find-blank state)))
		(progn (setq p1 (find-blank state))
			(setq i (car p1))
			(setq j (+ 1 (cadr p1)))
			(setq x (aref state i j))
			(setq p2 (list i j))
			(update state x p1 p2)) state))

; Function to update values in the array
(defun update (state x point1 point2)
	(setq new (make-array '(3 3)))
	(dotimes (i 3)
		(dotimes (j 3)
			(cond ((equal (list i j) point1) (setf (aref new i j) x)) 
				((equal (list i j) point2) (setf (aref new i j) 0))
				(t (setf (aref new i j) (aref state i j)))))) new) 

; List of possible moves for the blank
(setq moves '(move-up move-down move-left move-right))

; Function to find the heuristic value of the state
(defun heuristic (state)
	(- 8 (no-of-tiles-misplaced state goal))	
)

; Function to find the number of tiles misplaced compared to goal state 
(defun no-of-tiles-misplaced (state goal)
	(setq c 0)
	(dotimes (i 3)
		(dotimes (j 3)
			(if (and (/= (aref state i j) 0) (= (aref state i j) (aref goal i j))) (setq c (+ 1 c)))
			(if (and (= i 2) (= j 2)) (progn (return-from no-of-tiles-misplaced c)))
)))
	


; Function to create a record	
(defun build-record (state parent depth weight)
	(list state parent depth weight))

; Functions to extract each part of a record 
(defun get-state (state-tuple)
	(nth 0 state-tuple))

(defun get-parent (state-tuple)
	(nth 1 state-tuple))

(defun get-weight (state-tuple)
	(nth 3 state-tuple))

(defun get-depth (state-tuple)
	(nth 2 state-tuple))

; Function to find the corresponding record for a state
(defun retrieve-by-state (state ls)
	(cond ((null ls) nil)
		((equal-array state (get-state (car ls))) (car ls))
		(t (retrieve-by-state state (cdr ls)))))


; Functions to create a list of records sorted in ascending order

(defun insert-by-weight (ls)
(if (null ls) '()
	(cons (min-val (car ls) (cdr ls)) (insert-by-weight (remove-ele ls (min-val (car ls) (cdr ls)))))))

(defun min-val (m ls)
(cond ((null ls) m)
	((< (get-weight (car ls)) (get-weight m)) (min-val (car ls) (cdr ls)))
	(t (min-val m (cdr ls)))))

(defun remove-ele (lst n)
	(cond ((null lst) '())
		((equal n (car lst)) (remove-ele (cdr lst) n))
		(t (cons (car lst) (remove-ele (cdr lst) n))))))


; Function to generate the descendants of a node
(defun generate-children (state depth moves)
	(cond ((null moves) nil)
		(t (let ((child (funcall (car moves) state))
			(rest (generate-children state depth (cdr moves))))
		   (cond ((null child) rest)
			 ((retrieve-by-state child rest) rest)
			 ((retrieve-by-state child open) rest)
			 ((retrieve-by-state child closed) rest)
			 (t (cons (build-record child state depth (+ depth (heuristic child))) rest)))))))

; Function to create the reverse of solution path
(defun soln (state)
	(cond ((null state) nil)
		(t (cons state (soln (get-parent (retrieve-by-state state closed)))))))


; Initialisation function 
(defun best-fs (i g)
	(setq start (list-array i))
	(setq goal (list-array g))
	(setq open (list (build-record start nil 0 (heuristic start))))
	(setq closed nil)
	(best-first)) 	

; Function to do best first search on the state space
(defun best-first ()
	(cond ((null open) nil)
		(t (let ((state (car open)))
			(setq closed (cons state closed))
			(cond ((equal-array (get-state state) goal) (reverse (soln goal)))
				((= (get-depth state) 10) (return-from best-first "not found"))
				(t (setq open
					(insert-by-weight
						(append (generate-children (get-state state) (+ 1 (get-depth state)) moves) (cdr open))))
					(best-first)))))))



;(best-fs '((2 8 3) (1 6 4) (0 7 5)) '((1 2 3) (8 0 4) (7 6 5)))
 
