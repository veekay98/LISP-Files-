; BFS and DFS solution to the water jug problem

(defun make-state (j1 j2)
	(list j1 j2))

(defun get-first (state)
	(nth 0 state))

(defun get-second (state)
	(nth 1 state))

; Function to fill first jug completely
(defun fill-first (state a b)
	(make-state a (get-second state)))

; Function to fill first jug from second jug
(defun fill-first-from-second (state a b)
	(cond ((<= (+ (get-second state) (get-first state)) a) (make-state (+ (get-second state) (get-first state)) 0))
		(t (make-state a (- (get-second state) (- a (get-first state)))))))

; Function to fill second jug completely
(defun fill-second (state a b)
	(make-state (get-first state) b))

; Function to fill second jug from first jug
(defun fill-second-from-first (state a b)
	(cond ((<= (+ (get-second state) (get-first state)) b) (make-state 0 (+ (get-second state) (get-first state))))
		(t (make-state (- (get-first state) (- b (get-second state))) b)))) 

; Functions to empty either jug or both
(defun empty-first (state a b)
	(make-state 0 (get-second state)))

(defun empty-second (state a b)
	(make-state (get-first state) 0))

(defun empty-both (state a b)
	(make-state 0 0))

; List of actions possible
 (setq moves '(empty-both empty-first empty-second fill-second fill-first fill-second-from-first fill-first-from-second))

(defun build-record (state parent) (list state parent))


(defun get-state (state)
	(nth 0 state))


(defun get-parent (state)
	(nth 1 state))

; Function to retrieve corresponding record of the state
(defun retrieve-by-state (state ls)
	(cond ((null ls) nil)
		((equal state (get-state (car ls))) (car ls))
		(t (retrieve-by-state state (cdr ls)))))

; Function to create a reverse of solution path
(defun soln (state)
	(cond ((null state) nil)
		(t (cons state (soln (get-parent (retrieve-by-state state closed)))))))

; Function to generate descendants of the node
(defun generate-children (state move a b)
	(cond ((null move) nil)
		(t (let ((child (funcall (car move) state a b))
			(rest (generate-children state (cdr move) a b)))
			(cond ((null child) rest)
				((retrieve-by-state child rest) rest)

				((retrieve-by-state child open) rest)
				((retrieve-by-state child closed) rest)
				(t (cons (build-record child state) rest)))))))

; BFS initialisation function
(defun bfs (a b g)

        (setq a1 a)
        (setq b1 b)
	(setq start '(0 0))
	(setq open (list (build-record start nil)))
	(setq closed nil)
	(setq goal g)
	(breadth-first))

; Function to implement BFS
(defun breadth-first ()
	(cond ((null open) nil)
		(t (let ((state (car open)))
		   (setq closed (cons state closed))
		   (cond ((equal (get-state state) goal) (reverse (soln goal)))
			 (t (setq open (append (cdr open) (generate-children (get-state state) moves a1 b1)))
		   (breadth-first)))))))


;(print (bfs 4 3 '(2 0)))


; DFS initialisation function
(defun dfs (a b g)
    (setq a2 a)
        (setq b2 b)
	
	(setq start '(0 0))
	(setq open (list (build-record start nil)))
	(setq closed nil)
	(setq goal g)
	(depth-first))

;Function to implement DFS
(defun depth-first ()
	(cond ((null open) nil)
		(t (let ((state (car open)))
		    (setq closed (cons state closed))
		    (cond ((equal (get-state state) goal) (reverse (soln goal)))
		    	  (t (setq open (append (generate-children (get-state state) moves a2 b2) (cdr open)))
		    (depth-first)))))))


;(print (dfs 4 3 '(2 0)))