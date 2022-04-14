(defun BFS (TREE)
    "Take a single argument that is the list representation of the tree, and returns a single,
    top-level list of the terminal nodes in the order they would be visited by a left-to-right breadth-first search."
    (cond 
        ; stop if node is empty
        ((NULL TREE) NIL)
        ; if node is leaf, add to front of result
        ((atom (car TREE)) (cons (car TREE) (bfs (cdr TREE))))
        ; else, recurse on rest nodes on this level
        ; remove list around the node and add to next level
        (t (BFS (append (cdr TREE) (car TREE))))))

(defun DFS (TREE)
    "Take a single argument that is the list representation of the tree, and returns a single, top-level list of
    the terminal nodes in the order they would be visited by a left-to-right depth-first search."
    (cond
        ; stop if node is empty
        ((NULL TREE) NIL)
        ; if node is leaf, add to front of result
        ((atom (car TREE)) (cons (car TREE) (dfs (cdr TREE))))
        ; else, remove list around the node and continue recurse on itself
        (t (DFS (append (car TREE) (cdr TREE))))))

(defun DFID-HELPER (TREE DEPTH)
    "Helper function of DFID."
    (cond 
        ; stop if node is empty or reaches depth
        ((or (NULL TREE) (< DEPTH 0)) NIL)
        ; if node is leaf, return itself
        ((atom TREE) (list TREE))
        ; else, recurse on rest nodes on this level and recurse this node on next level (from right to left)
        (t (append (DFID-HELPER (cdr TREE) DEPTH) (DFID-HELPER (car TREE) (- DEPTH 1))))))

(defun DFID (TREE DEPTH)
    "Take two arguments, the list representation of the tree, and an integer
    representing the maximum depth of the tree, and returns a single top-level list of the terminal nodes in the
    order that they would be visited by a right-to-left depth-first iterative-deepening search."
    (cond
        ; stop if node is empty or reaches depth
        ((or (NULL TREE) (< DEPTH 0)) NIL)
        ; recursively append DFS results with depth decremented
        (t (append (DFID TREE (- DEPTH 1)) (DFID-HELPER TREE DEPTH)))))

; (print (BFS '((A (B)) C (D))))
; (print (DFS '((A (B)) C (D))))
; (print (DFID '((A (B)) C (D)) 3))

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
    (equal s '(3 3 NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
    (let*
        ((THIS-M (first s))
        (THIS-C (second s))
        (THIS-SIDE (third s))
        (OTHER-M (- 3 THIS-M))
        (OTHER-C (- 3 THIS-C))
        (OTHER-SIDE (not THIS-SIDE)))
    (cond 
        ; check invalid states
        ((or 
            ; invalid to have C > M on either side
            (and (> (- THIS-C c) (- THIS-M m))
                (> (- THIS-M m) 0))
            (and (> (+ OTHER-C c) (+ OTHER-M m)) 
                (> (+ OTHER-M m) 0))
            ; invalid to move more M or C or more than 2 on the boat
            (> m THIS-M) (> c THIS-C) (> (+ m c) 2)) NIL)
        ; return next state as the other side
        (t (list (list (+ OTHER-M m) (+ OTHER-C c) OTHER-SIDE))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
    ; append all possible next states
    (append (next-state s 1 0)
        (next-state s 0 1)
        (next-state s 1 1)
        (next-state s 2 0)
        (next-state s 0 2)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
    (cond 
        ; stop if states are empty
        ((NULL states) NIL)
        ; if first element equals the state, return true
        ((equal s (car states)) t)
        ; else, recursively search the rest list
        (t (on-path s (cdr states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
    (cond 
        ; stop if states are empty (or means everything is searched)
        ((NULL states) NIL)
        ; if there is a path from current state to final state, return the path
        ((mc-dfs (car states) path) (mc-dfs (car states) path))
        ; else, recursively search the rest states
        (t (mult-dfs (cdr states) path))))

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
    (cond 
        ; stop if s is already in the path or there is no successor states of s
        ((or (on-path s path) (NULL (succ-fn s))) NIL)
        ; if reaches final state, returns the entire path in reversed order
        ((final-state s) (reverse (cons s path)))
        ; else, recursively dfs with successor states and path added with current state
        (t (mult-dfs (succ-fn s) (cons s path)))))

; Function execution examples

; Applying this operator would result in an invalid state, with more O's
; than X's on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL
; (print (next-state '(3 3 t) 1 0))

; Applying this operator would result in one O and zero X on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))
; (print (next-state '(3 3 t) 0 1))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
; (print (succ-fn '(3 3 t)))
; (print (succ-fn '(1 1 t)))
