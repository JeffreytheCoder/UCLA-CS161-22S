(defun TREE-CONTAINS (N TREE)
    (cond ((atom TREE) 
            ; if TREE is found as N, return true
            (cond ((OR (NULL TREE) (NOT (= N TREE))) NIL)
                ((= N TREE) t)))
        ; else, get current node, left and right subtree
        (t (let ((left (car TREE))
            (cur (cadr TREE))
            (right (caddr TREE)))
    ; binary search
    (cond ((< N cur) (TREE-CONTAINS N left))
        ((> N cur) (TREE-CONTAINS N right))
        ((= N cur) t))))))
(print (TREE-CONTAINS 3 '((1 2 3) 7 8)))
(print (TREE-CONTAINS 4 '((1 2 3) 7 8)))

(defun TREE-MAX (TREE)
    ; returns the last element in TREE
    (cond ((if (not (cdr TREE)) (car TREE)))
        (t (TREE-MAX (cdr TREE)))))
(print (TREE-MAX '((1 2 3) 7 8)))

(defun TREE-ORDER (TREE)
    (cond ((atom TREE) (list TREE))
        (t (let ((left (car TREE))
            (cur (cadr TREE))
            (right (caddr TREE)))
            ; post order: left - right - cur
            (append (TREE-ORDER left) (TREE-ORDER right) (list cur))))))
(print (TREE-ORDER 3))
(print (TREE-ORDER '((1 2 3) 7 8)))

(defun SUB-LIST (L START LEN)
    (cond ((or (NULL L) (= LEN 0)) NIL)
        ; if reached starting index, get next sublist of LEN length
        ((= START 0) (append (list (car L)) (SUB-LIST (cdr L) 0 (- LEN 1))))
        ; else, move starting index
        (t (SUB-LIST (cdr L) (- START 1) LEN))))
(print (SUB-LIST '(a b c d) 0 3))
(print (SUB-LIST '(a b c d) 3 1))
(print (SUB-LIST '(a b c d) 2 0))

(defun SPLIT-LIST (L)
    (cond ((NULL L) NIL)
        (t (let ((middle (ceiling (/ (length L) 2))))
        (list (SUB-LIST L 0 middle) (SUB-LIST L middle (- (length L) middle)))))))
(print (SPLIT-LIST '(a b c d)))
(print (SPLIT-LIST '(a b c d e)))
(print (SPLIT-LIST '(a b c d e f)))

(defun BTREE-HEIGHT (TREE)
    (cond ((NULL TREE) NIL)
        ((atom TREE) 0)
        (t (let ((left (car TREE))
            (right (cadr TREE)))
            ; get max height of left and right subtrees
            (cond ((> (BTREE-HEIGHT left) (BTREE-HEIGHT right)) (+ 1 (BTREE-HEIGHT left)))
                (t (+ 1 (BTREE-HEIGHT right))))))))
; (print (BTREE-HEIGHT 1))
; (print (BTREE-HEIGHT '(1 2)))
; (print (BTREE-HEIGHT '(1 (2 3))))
; (print (BTREE-HEIGHT '((1 2) (3 4))))
; (print (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))))
; (print (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))))

(defun LIST2BTREE (LEAVES)
    (cond ((= (length LEAVES) 1) (car LEAVES))
        ; split list into left and right subtrees
        (t (cons (LIST2BTREE (car (SPLIT-LIST LEAVES))) (cons (LIST2BTREE (cadr (SPLIT-LIST LEAVES))) '())))))
(print (LIST2BTREE '(1)))
(print (LIST2BTREE '(1 2)))
(print (LIST2BTREE '(1 2 3)))
(print (LIST2BTREE '(1 2 3 4)))
(print (LIST2BTREE '(1 2 3 4 5 6 7)))
(print (LIST2BTREE '(1 2 3 4 5 6 7 8)))

(defun BTREE2LIST (TREE)
    (cond ((atom TREE) (list TREE))
        ; combine left and right parts of tree into a list
        (t (let ((left (car TREE))
            (right (cadr TREE)))
        (append (BTREE2LIST left) (BTREE2LIST right))))))
(print (BTREE2LIST '(1)))
(print (BTREE2LIST '(1 2)))
(print (BTREE2LIST '((1 2) 3)))
(print (BTREE2LIST '((1 2) (3 4))))
(print (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))))
(print (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))))

(defun IS-SAME (E1 E2)
    (cond ((and (NULL E1) (NULL E2)) t)
        ((or (and (not (atom E1)) (atom E2)) 
            (and (atom E1) (not (atom E2)))
            (and (atom E1) (atom E2) (not (= E1 E2)))) NIL)
        ((and (atom E1) (atom E2) (= E1 E2)) t)
        (t (IS-SAME (car E1) (car E2)))))
(print (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)))
(print (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)))

(defun FLATTEN-APPEND(E1 E2)
    (cond ((NULL E2) E1)
        ; if whole E2 is an atom, add to E1 directly
        ((atom E2) (append E1 (list E2)))
        ; if first element of E2 is list, until it becomes atom
        ((listp (car E2)) (append (FLATTEN-APPEND NIL (car E2)) (FLATTEN-APPEND NIL (cdr E2))))
        ; if first element of E2 is atom, add to E1 and process rest
        (t (append E1 (cons (car E2) NIL) (FLATTEN-APPEND NIL (cdr E2))))))
(print (FLATTEN-APPEND '(0 1) NIL))
(print (FLATTEN-APPEND '(0 1) '2))
(print (FLATTEN-APPEND '(0 1) '(2 (3 4) 5 6)))
(print (FLATTEN-APPEND '(0 (1 (2 3)) 4) '(5 (6) 7)))
(print (FLATTEN-APPEND NIL '(1 2((3) 4) 5 (6 7) 8)))