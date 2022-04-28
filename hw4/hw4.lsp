;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists

(defun sat? (n delta)
  ; (print n)
  (dfs n delta 0 '())
)

(defun dfs (n delta level combinations)
  ; (format t "combination: ~A~%" combinations)
  (cond
    ; if the current combination doesn't satisfy CNF, prune this branch
    ((not (check-cnf delta combinations)) nil)
    ; if dfs reaches max depth, return combination if satisfies CNF, else return nil
    ((= n level)
      (cond
        ((check-cnf delta combinations) combinations)
        (t nil)
      )
    )
    ; search next level using combinations added both positive and negative of current level (variable value)
    (t 
      (or 
        (dfs n delta (+ level 1) (append combinations (list level)))
        (dfs n delta (+ level 1) (append combinations (list (- level))))
      )
    )
  )
)

(defun check-cnf (cnf combinations)
; (format t "cnf: ~A~%" (cdr cnf))
  (cond
    ; if no constraint clauses or combinations exit, satisfy directly
    ((or (null cnf) (and (not (null cnf)) (null combinations))) t)
    ; since clauses in cnf is ANDed, all clauses must be satisfied
    (t
      (and (check-cnf-clause (first cnf) combinations) (check-cnf (rest cnf) combinations))
    )
  )
)

(defun check-cnf-clause (clause combinations)
  ; (format t "clause: ~A~%" (cdr clause))
  (cond
    ; if all values in a clause is iterated and hasn't return true, no value is satisfied
    ((null clause) nil)
    ; since values in a clause is ORed, return t once a value is satisfied
    ((check-cnf-value (car clause) combinations) t)
    (t (check-cnf-clause (cdr clause) combinations))
  )
)

(defun check-cnf-value (value combinations)
  ; (format t "value: ~A~%" value)
  (cond
    ((null combinations) t)
    ; iterate all values in combinations and check if one same as the value in clause is satified
    ((= (abs value) (abs (car combinations))) (= value (car combinations)))
    (t (check-cnf-value value (cdr combinations)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; (parse-cnf "./cnfs/f1/sat_f1.cnf")

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

; (time (print (solve-cnf "./cnfs/f1/sat_f1.cnf")))