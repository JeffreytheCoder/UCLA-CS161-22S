;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)

(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)

(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

(defun get-square-col (row-list c)
  (cond
		((or (> c (- (length row-list) 1)) (null row-list)) wall)
		((= c 0) (car row-list))
		(t (get-square-col (cdr row-list) (- c 1)))
	)
)

(defun get-square (s r c)
  (cond
		((or (> r (- (length s) 1)) (null s)) wall)
		((= r 0) (get-square-col (car s) c))
		(t (get-square (cdr s) (- r 1) c))
	)
);end defun

; (print (get-square p1 1 2))

(defun set-square-col (row-list c val)
	(cond
		((= c 0) (cons val (cdr row-list)))
		(t (cons (car row-list) (set-square-col (cdr row-list) (- c 1) val)))
	)
)

(defun set-square (s r c val)
	(cond
		((= r 0) (cons (set-square-col (car s) c val) (cdr s)))
		(t (cons (car s) (set-square (cdr s) (- r 1) c val)))
	)
)

; (print (set-square p1 1 2 0))

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	)
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (left) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       )
	 );end t
	)
  );end defun

; (print (getKeeperPosition p1 0))

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     )
	   );end t
	)
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test-col (s r c)
	(let (val (get-square s r c))
		(cond
			((isWall val) t)
			((or (isKeeper val) (isBox val)) nil)
			(t (goal-test-col s r (+ c 1)))
		)
	)
)

(defun goal-test-row (s r)
	(let (testRes (goal-test-col))
		(cond
			((null testRes) nil)
			(t (goal-test-row s (+ r 1)))
		)
	)
)

(defun goal-test (s)
  (goal-test-row s 0)
);end defun


; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test-row (row-list)
	(cond
		((null (car row-list)) t)
		; goal test not pass if keeper or box is not on goal
		((or (isKeeper (car row-list)) (isBox (car row-list))) NIL)
		(t (goal-test-row (cdr row-list)))
	)
)

(defun goal-test (s)
  (cond
		((null (car s)) t) 
		((null (goal-test-row (car s))) NIL) 
		(t (goal-test (cdr s)))
	)
)

; (print (goal-test p1))


; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun try-move (s d)
	(let* (
		(cur-pos (getKeeperPosition s 0))
		(c (car cur-pos))
		(r (cadr cur-pos))
		(cur-val (get-square s r c))
		(next-pos
			(cond 
				((equal d 'UP) (list (- r 1) c))
				((equal d 'DOWN) (list (+ r 1) c))
				((equal d 'LEFT) (list r (- c 1)))
				((equal d 'RIGHT) (list r (+ c 1)))
			)
		)
		(next-r (car next-pos))
		(next-c (cadr next-pos))
		(next-val (get-square s next-r next-c)))
		(cond
			((isWall next-val) NIL)
			((isBlank next-val) 
				(let ((next-state (set-square s next-r next-c keeper)))
					; check if keep is on star originally
					(cond
						((isKeeperStar cur-val) (set-square next-state r c star)) 
						(t (set-square next-state r c blank))
					)
				)
			)
			((isStar next-val) 
				(let ((next-state (set-square s next-r next-c keeperstar)))
					; check if keep is on star originally
					(cond
						((isKeeperStar cur-val) (set-square next-state r c star))
						(t (set-square next-state r c blank))
					)
				)
			)
			((or (isBox next-val) (isBoxStar next-val))
					(let* 
						((next-box-pos
							(cond 
								((equal d 'UP) (list (- next-r 1) next-c))
								((equal d 'DOWN) (list (+ next-r 1) next-c))
								((equal d 'LEFT) (list next-r (- next-c 1)))
								((equal d 'RIGHT) (list next-r (+ next-c 1)))
							)
						)
						(next-box-r (car next-box-pos))
						(next-box-c (cadr next-box-pos))
						(next-box-val (get-square s next-box-r next-box-c)))
					(cond 
						; can't push box to a wall or another box
						((or (isWall next-box-val) (isBox next-box-val) (isBoxStar next-box-val)) NIL)
						; move box then keeper
						((isBlank next-box-val) 
							(cond 
								((isBoxStar next-val) 
									(let* 
										((move-box (set-square s next-box-r next-box-c box))
										(move-keeper (set-square move-box next-r next-c keeperstar)))
										(cond 
											((isKeeperStar cur-val) (set-square move-keeper r c star))
											(t (set-square move-keeper r c blank))
										)
									)
								)
								((isBox next-val) 
									(let* 
										((move-box (set-square s next-box-r next-box-c box))
										(move-keeper (set-square move-box next-r next-c keeper)))
										(cond
											((isKeeperStar cur-val) (set-square move-keeper r c star))
											(t (set-square move-keeper r c blank))
										)
									)
								)
							)
						)
						((isStar next-box-val) 
							(cond 
								((isBoxStar next-val) 
									(let* 
										((move-box (set-square s next-box-r next-box-c boxstar))
										(move-keeper (set-square move-box next-r next-c keeperstar)))
										(cond 
											((isKeeperStar cur-val) (set-square move-keeper r c star))
											(t (set-square move-keeper r c blank))
										)
									)
								)
								((isBox next-val) 
									(let* 
										((move-box (set-square s next-box-r next-box-c boxstar))
										(move-keeper (set-square move-box next-r next-c keeper)))
										(cond
											((isKeeperStar cur-val) (set-square move-keeper r c star))
											(t (set-square move-keeper r c blank))
										)
									)
								)
							)
						)
					)
				)
			)
		)
	)
)

; (print p10)
; (print (try-move p10 'LEFT))
; (print (try-move p10 'RIGHT))
; (print (try-move p10 'UP))
; (print (try-move p10 'DOWN))

(defun next-states (s)
	(let* 
		((result (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT))))
    (cleanUpList result);end
  )
)

; (print (next-states p1))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
(defun count-box-col (L)
	(cond
		((null (car L)) 0)
		((isBox (car L)) (+ 1 (count-box-col (cdr L))))
		(t (count-box-col (cdr L)))
	)
)

(defun h1 (s)
	(cond
		((null (car s)) 0)
		(t (+ (count-box-col (car s)) (h1 (cdr s))))
	)
)

; EXERCISE: Modify this h2 function to compute an
; admissible heuristic val of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun get-boxes-pos-col (row-list r c)
	(cond
		((null row-list) nil)
		((isBox (car row-list)) (cons (list r c) (get-boxes-pos-col (cdr row-list) r (+ c 1))))
		(t (get-boxes-pos-col (cdr row-list) r (+ c 1)))
	)
)

(defun get-boxes-pos (s r)
	(cond
		((null s) nil)
		(t (append (get-boxes-pos-col (car s) r 0) (get-boxes-pos (cdr s) (+ r 1))))
	)
)

(defun get-goals-pos-col (row-list r c)
	(cond
		((null row-list) nil)
		((isStar (car row-list)) (cons (list r c) (get-goals-pos-col (cdr row-list) r (+ c 1))))
		(t (get-goals-pos-col (cdr row-list) r (+ c 1)))
	)
)

(defun get-goals-pos (s r)
	(cond
		((null s) nil)
		(t (append (get-goals-pos-col (car s) r 0) (get-goals-pos (cdr s) (+ r 1))))
	)
)

(defun get-distance (box goal)
	(let 
		((r1 (car box))
		(c1 (cadr box))
		(r2 (car goal))
		(c2 (cadr goal)))
		(+ (abs (- r1 r2)) (abs (- c1 c2)))
	)
)

; get distances between a box and all goals
(defun get-box-goals-distance (box-pos goals-pos)
	(let* 
		((goal-pos (car goals-pos))
		(distance (get-distance box-pos goal-pos)))
		(cond
			((= (length goals-pos) 1) distance)
			(t (+ distance (get-box-goals-distance box-pos (cdr goals-pos))))
		)
	)
)

; sum up distances between all boxes and all goals
(defun get-boxes-goals-distance (boxes-pos goals-pos)
	(cond
		((null boxes-pos) 0)
		(t (+ (get-box-goals-distance (car boxes-pos) goals-pos) (get-boxes-goals-distance (cdr boxes-pos) goals-pos)))
	)
)

(defun h2 (s)
	(let 
		((boxes-pos (get-boxes-pos s 0))
		(goals-pos (get-goals-pos s 0)))
		(get-boxes-goals-distance boxes-pos goals-pos)
	)
)

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  )
    )
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	)
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	)
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun

; (load-a-star)
; (time (a* p15 #'goal-test #'next-states #'h2))