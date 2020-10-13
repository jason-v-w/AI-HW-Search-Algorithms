;;; +-----------------------+
;;; | search_framework.lisp |
;;; +-----------------------+
;;;
;;; Jason Vander Woude
;;;
;;; Homework 4
;;; CSCE 875: Articifial Intelligence (University of Nebraska-Lincoln)
;;; Sunday October 11, 2020





;;; The functions in this file define general frameworks for using the graph
;;; search and tree search algorithms as described in AIMI.





;;; +-------------------+
;;; | Utility Functions |
;;; +-------------------+

;;; Used as the equality test function in tree search.
(defun equal-p-no (x y) nil)

;;; Used as the default priority function in tree and graph search. Helpful to
;;; think of all elements being equal in priority and this is like <= on the
;;; priority which would always return t.
(defun common-priority (x y) t)



;;; +--------------+
;;; | Tree Search |
;;; +--------------+

;;; Tree search can be viewed as a specific type of graph search where no two
;;; states are ever considered to be the same (even if the states truly are the
;;; same from the point of view of the problem). In other words, there is a real
;;; sense in which for graph search it is necessary to be able to identify
;;; whether two computer objects represent the same state in a problem, and we
;;; in some sense abuse that that is how the graph search algorithm is
;;; coded. Doing this has a cost on efficiency (but not complexity) because the
;;; graph-search routine will unnecessarily make many checks of equality that
;;; will always return false, but the advantage is that the code is very short.
(defun tree-search (problem &key (priority-function 'common-priority))
  "Perform a tree search of the problem"
  (graph-search-helper problem
		       'equal-p-no
		       priority-function))





;;; +--------------+
;;; | Graph Search |
;;; +--------------+

;;; problem should be a problem instance
;;;
;;; test should be a way to determine the equality of two states and defaults to
;;; 'equal to serve some usefulness (e.g. will work as expected for strings)
;;;
;;; priority-function should be a function of two nodes which returns true if
;;; the first node should be prioritized over the second when removing from the
;;; frontier (where a node is as desribed in general_problem_framework.lisp)
(defun graph-search (problem &key (priority-function 'common-priority))
  "Perform a graph search of the problem"
  (graph-search-helper problem
		       (general-problem-state-equal-p problem)
		       priority-function))





;;; +---------------------+
;;; | Graph Search Helper |
;;; +---------------------+

;;; At the highest level, a user should not be able to specify an equality test
;;; function as the behaviour would likely not be as expected. The only reason
;;; for needing the test argument is so that tree search can also use this code
;;; as decribed in the tree-search comments.
(defun graph-search-helper (problem test priority-function)
  "PRIVATE. Perform a graph search of the problem using test and priority-function"
  (let ((frontier (create-heap priority-function))                  ; empty priority queue
	(frontier-or-explored-set (make-hash-table :test test))     ; empty hash table
	(init-node (make-node :state (general-problem-init problem) ; node from initial state
			      :parent nil))                         ; root node of search tree
                                                                    ; path-cost handled by problem
	(get-allowed-actions (general-problem-actions problem))
	(get-transition-result (general-problem-transition-model problem))
	(path-cost (general-problem-path-cost problem))
	(heuristic (general-problem-heuristic problem))
	(goal-test (general-problem-goal-test problem))
	(current-node nil)		       ; repeatedly reasigned throughout
	(a-child-node nil)		       ; repeatedly reasigned throughout
	(num-nodes-expanded 0)                 ; used for analysis purposes
	(start-time (get-internal-real-time))) ; keep track of elapsed time

    (flet ((add-to-frontier-or-explored (node)
	     (setf (gethash (node-state node)
			    frontier-or-explored-set)
	       (node-state node)))
	   (in-frontier-or-explored-p (node)
	     (gethash (node-state node) frontier-or-explored-set))

	   (add-to-frontier (node)
	     (heap-insert frontier node)))

      ;; graph search initilization
      (setf (node-path-cost init-node) (funcall path-cost init-node)) ; set cost of init node
      (setf (node-heuristic init-node) (funcall heuristic init-node)) ; set heuristic of init node
      (add-to-frontier init-node)

      ;; graph search loop
      (do () (nil)                                 ; do forever
	(if (heap-empty-p frontier)	; if no options remain
	      (return nil))		; return nil (represents failure)
	(setq current-node (heap-remove frontier)) ; get current node (removed from frontier)

	;; if a goal is found
	(if (funcall goal-test (node-state current-node))
	    (return (make-general-solution :node current-node
					   :extra (list
						   (list 'n-expanded
							 num-nodes-expanded)
						   (list 'cpu-time
							 (- (get-internal-real-time)
							    start-time))))))

	(add-to-frontier-or-explored current-node)

	;; expand node
	; (print (node-state current-node)) ; for seeing the node expansion order
	(setq num-nodes-expanded (1+ num-nodes-expanded))
	(dolist (action (funcall get-allowed-actions (node-state current-node))) ; for each allowed action
	  (setq a-child-node (child-node problem ; get the child node for this action
					 current-node
					 action))
	  (if (not (in-frontier-or-explored-p a-child-node)) ; if not explored
	      (progn
		(add-to-frontier a-child-node)
		(add-to-frontier-or-explored a-child-node))))))))





;;; +-----------------+
;;; | Node Evaluation |
;;; +-----------------+

;;; Below is a collection of evaluation functions used by various search
;;; strategies to determine the quality of a node.

(defun best-first-evaluator (node)
  (node-path-cost node))                ; g(node)

(defun greedy-evaluator (node)
  (node-heuristic node))                ; nil or h(node)

(defun a*-evaluator (node)
  (if (node-heuristic node)		; if heuristic is specified
      (+ (node-heuristic node)          ;   h(node) +
	 (node-path-cost node))         ;   g(node)
    nil))				; else nil





;;; +-------------------------+
;;; | Node Expansion Priority |
;;; +-------------------------+

;;; The code below uses the node evaluation functions above as the basis of
;;; priority functions that can be passed to tree-search or graph-search to
;;; control how nodes from the frontier are chosen for expansion

(defun best-first-prioritizer (node-1 node-2)
  (<= (best-first-evaluator node-1)
      (best-first-evaluator node-2)))

(defun greedy-prioritizer (node-1 node-2)
  (<= (greedy-evaluator node-1)
      (greedy-evaluator node-2)))

(defun a*-prioritizer (node-1 node-2)
  (<= (a*-evaluator node-1)
      (a*-evaluator node-2)))

;;; to comply with homework assignment requirement
(defun get-prioritizer-descriptor (name)
  (cond ((equalp name "best-first") 'best-first-prioritizer)
	((equalp name "greedy")     'greedy-prioritizer)
	((equalp name "a*")         'a*-prioritizer)
	(t nil)))
