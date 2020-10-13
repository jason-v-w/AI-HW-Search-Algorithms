;;; Jason Vander Woude
;;;
;;; Homework 4
;;; CSCE 875: Articifial Intelligence (University of Nebraska-Lincoln)
;;; Sunday October 11, 2020





;;; The functions in this file define general frameworks for using the graph
;;; search and tree search algorithms as described in AIMI.





;;; +--------------+
;;; | Tree Search |
;;; +--------------+

;;; Tree search can be viewed as a specific type of graph search where no two
;;; states are ever considered to be the same (even if the states are the same
;;; from the point of view). In other words, there is a real sense in which for
;;; graph search it is necessary to be able to identify whether two computer
;;; objects represent the same state in a problem, and we abuse that that is how
;;; graph search works. Doing this has a cost on efficiency (not complexity)
;;; because the graph-search routine will make checks of equality that will
;;; always return false, but the advantage is that the code is very short.
(defun tree-search (problem &key
			    (priority-function '(lambda (any) 0)))
  "Perform a tree search of the problem"
  (graph-search problem
		:test '(lambda (state-1 state-2) nil) ; states are never the same
		:priority-function priority-function)); pass along the priority function





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
(defun graph-search (problem &key
			     (test 'equal)
			     (priority-function #'(lambda (x y) t))); vacuous ordering
  "Perform a graph search of the problem"
  (let ((frontier (create-heap priority-function))                  ; empty priority queue
	(frontier-or-explored-set (make-hash-table :test test))     ; empty hash table
	(init-node (make-node :state (general-problem-init problem) ; node from initial state
			      :parent nil))                         ; root node of search tree
                                                                    ; path-cost handled by problem
	(get-allowed-actions (general-problem-actions problem))
	(get-transition-result (general-problem-transition-model problem))
	(path-cost (general-problem-path-cost problem))
	(goal-test (general-problem-goal-test problem))
	(current-node nil)		; repeatedly reasigned throughout
	(a-child-node nil)		; repeatedly reasigned throughout
	(num-nodes-expanded 0))		; used for analysis purposes

    (flet ((add-to-frontier-or-explored (node)
	     (setf (gethash (node-state node)
			    frontier-or-explored-set)
	       (node-state node)))
	   (in-frontier-or-explored-p (node)
	     (gethash (node-state node) frontier-or-explored-set))

	   (add-to-frontier (node)
	     (heap-insert frontier node)))

      ;; graph search initilization
      (setf (node-path-cost init-node) (funcall path-cost init-node))

      (add-to-frontier init-node)

      ;; graph search loop
      (do () (nil)                                 ; do forever
	(if (heap-empty-p frontier)	; if no options remain
	      (return nil))		; return nil (represents failure)
	(setq current-node (heap-remove frontier)) ; get current node (removed from frontier)

	;; if a goal is found
	(if (funcall goal-test (node-state current-node))
	    (return (make-general-solution :path current-node
					   :extra (list
						   (list "num-nodes-expanded"
							 num-nodes-expanded)))))

	(add-to-frontier-or-explored current-node)

	;; expand node
	(print (node-state current-node))
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

(defun greedy-search-evaluator (node)
  (node-heuristic node))                ; nil or h(node)

(defun a*-search-evaluator (node)
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
