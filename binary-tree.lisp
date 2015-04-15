(defun null-tree ()
  "Create a new null tree node."
  nil)

(defun null-tree-p (tree)
  "Predicate for indentifying a null tree node."
  (null tree))

(defun node-tree (val left-node right-node)
  "Create a new tree."
  (vector val left-node right-node))

(defun node-tree-p (tree)
  "Predicate for identifying a non-null tree node."
  (not (null-tree-p tree)))

(defun singleton-tree (val)
  "Create a new tree where the default branches are null tree nodes."
  (node-tree val (null-tree) (null-tree)))

(defun val-tree (tree)
  "Get the val in a tree."
  (aref tree 0))

(defun left-branch (tree)
  "Get the left branch of a tree."
  (aref tree 1))

(defun right-branch (tree)
  "Get the right branch of a tree."
  (aref tree 2))

(defun insert-tree (tree val)
  "Return the old tree with the new value inserted."
  (cond ((null-tree-p tree)
	 (singleton-tree val))
	((< val (val-tree tree))
	 (node-tree (val-tree tree)
		    (insert-tree (left-branch tree) val)
		    (right-branch tree)))
	((> val (val-tree tree))
	 (node-tree (val-tree tree)
		    (left-branch tree)
		    (insert-tree (right-branch tree) val)))
	((equal val (val-tree tree))
	 tree)))

;; (reduce #'insert-tree '(4 1 2 3 5 6 7) :initial-value (null-tree))

(defun print-tree (tree)
  (labels ((print-iter (tree depth)
	     (dotimes (i depth)
	       (format t "| "))
	     (cond ((node-tree-p tree)
		    (format t "~a~%" (val-tree tree))
		    (print-iter (right-branch tree) (1+ depth))
		    (print-iter (left-branch tree) (1+ depth)))
		   ((null-tree-p tree)
		    (format t "null~%")))))
    (print-iter tree 0)))
		       

(defun elem (tree val) "Check if val is a member of the tree."
  (cond ((null-tree-p tree)
	 nil)
	((node-tree-p tree)
	 (or (equal val (val-tree tree))
	     (elem-tree (left-branch tree) val)
	     (elem-tree (right-branch tree) val)))))

(defun size-tree (tree)
  (cond ((null-tree-p tree)
	 0)
	((node-tree-p tree)
	 (+ 1
	    (sum-tree (left-branch tree))
	    (sum-tree (right-branch tree))))))

(defun sum-tree (tree)
  (cond ((null-tree-p tree)
	 0)
	((node-tree-p tree)
	 (+ (val-tree tree)
	    (sum-tree (left-branch tree))
	    (sum-tree (right-branch tree))))))

(defun size-tree-iter (tree)
  (let ((accum 0))
    (labels ((iter (tree)
	       (cond ((null-tree-p tree)
		      0)
		     ((node-tree-p tree)
		      (incf accum)
		      (iter (left-branch tree))
		      (iter (right-branch tree))))
	       accum))
      (iter tree))))

