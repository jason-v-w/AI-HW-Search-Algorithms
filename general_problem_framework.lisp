;;; Jason Vander Woude
;;;
;;; Homework 4
;;; CSCE 875: Articifial Intelligence (University of Nebraska-Lincoln)
;;; Sunday October 11, 2020





;;; This file contains stuctures and functions for formulating and
;;; storing general problems as defined in AIMA





;;; +--------------------------------+
;;; | Structure for General Problems |
;;; +--------------------------------+

(defstruct general-problem
  init              ; initial state
  actions           ; function taking a state and returning a set of valid actions
  transition-model  ; function taking a state and valid action and returing the next state
  goal-test         ; function taking a state returning whether it is a goal
  path-cost         ; function taking a node and returning a cost (not necessarily step costs)
  state-equal-p)    ; function takeing two states and returning if they are the same





;;; +---------------------------------------+
;;; | Structure for General Solutions Paths |
;;; +---------------------------------------+

(defstruct general-solution
  path	  ; a goal node implicitly defining a path
  extra)  ; keep extra information about solving for a solution





;;; +---------------------+
;;; | Structure for Nodes |
;;; +---------------------+

(defstruct node
  state       ; state of the node
  parent      ; parent node
  action      ; the action that resulted in this node
  path-cost   ; not a function; just a number; useful for avoiding repeated calcuations
  heuristic)  ; not needed for all search types





;;; +---------------------+
;;; | Child Node Function |
;;; +---------------------+

;;; This function follows the framework of AIMA p. 79 to generate children nodes
;;; based on the actions function and transition model. It is slightly different
;;; in that it doesn't assume the cost of a node is based on step costs and
;;; instead uses whatever the problem's path-cost function is.
(defun child-node (problem parent action)
  (let* (				; information from problem
	 (transition-model (general-problem-transition-model problem))
	 (path-cost (general-problem-path-cost problem))

	 (child
	  (make-node :state (funcall transition-model
				     (node-state parent)
				     action)
		     :parent parent
		     :action action
		     :path-cost nil)))	; temporarily

    (setf (node-path-cost child) (funcall path-cost child)) ; set path-cost propety
    child))				; return
