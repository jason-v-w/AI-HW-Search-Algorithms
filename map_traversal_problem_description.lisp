;;; Jason Vander Woude
;;;
;;; Homework 4
;;; CSCE 875: Articifial Intelligence (University of Nebraska-Lincoln)
;;; Sunday October 11, 2020





;;; This file contains the function definitions that will be used to instantiate
;;; a Romanian Holiday problem description. These functions must abide by the
;;; template of the general framework in problem_framework.lisp even though some
;;; of them become trivialities.






;;; +----------------+
;;; | City Structure |
;;; +----------------+

(defstruct (city)
  (name      nil       :type string)
  (neighbors nil       :type list)
  (h         0         :type integer))





;;; +-------------------------+
;;; | Check Equality of State |
;;; +-------------------------+

;;; States are atoms, so use 'eq
(defun city-equal (state-1 state-2)
  "PRIVATE. Determine if two states are the same in the problem"
  (eq state-1 state-2))





;;; +----------------------------+
;;; | From Map to Hash Structure |
;;; +----------------------------+

;;; This function will convert a map into a hash table with names of cities (as
;;; atoms) as the keys and city structures as values. The neighbor property of a
;;; city is an association list with city structures for keys and distances for
;;; values.
(defun map-to-cities-hash-table (map)
  "PRIVATE. Convert a map into a hash table for easier access"
  (let ((cities-htable (make-hash-table :test 'city-equal))
	(city nil)
	(neighbor-name nil)
	(distance nil))
    (dolist (city-info map)
      (let ((city-name (first city-info))
	    (heuristic (second city-info)))

	(setf (gethash city-name cities-htable)
	  (make-city
	   :name city-name
	   :h    heuristic))))

    (dolist (city-info map)
      ;; add each neighbor to the city
      (dolist (pair (third city-info))
	(let ((neighbor-name (first pair))
	      (distance (second pair)))

	  (push (list (gethash neighbor-name
		      	       cities-htable)
		      distance)
		(city-neighbors (gethash (first city-info)
					 cities-htable))))))
    cities-htable))





;;; +--------------------------+
;;; | Closure Actions Function |
;;; +--------------------------+

;;; This function will, given a cities hash table as above, return a closure (function
;;; internally referencing the cities hash table). The returned closure is a function of a
;;; state that will return the valid actions from that state. In this case, a
;;; state is a city name (as an atom) and an action is also a city name (as an
;;; atom). In essence, the closure will return the neighbors of a given city.
(defun get-actions-generator (cities-htable)
  "PRIVATE. Returns a closure/function that given a state returns a list of legal actions"
  ;; return a lambda function
  #'(lambda (state)
      (mapcar #'(lambda (neighbor-assoc) (city-name (first neighbor-assoc)))
      	      (city-neighbors
      	       (gethash state cities-htable)))))





;;; +----------------------------+
;;; | Closure Goal Test Function |
;;; +----------------------------+

;;; Returns a closure which given a state determines if it is the goal state
(defun get-goal-test-function (end-city)
  #'(lambda (state) (city-equal state end-city)))





;;; +------------------+
;;; | Transition Model |
;;; +------------------+

;;; The transition model of AIMA takes a current state and a valid
;;; action and returns the state obtained from applying the action to
;;; the current state. In the Romanian Holiday example, this function
;;; is a trivial one just returning the name of the action as it is
;;; the same as the name of the next state by design
(defun transition-result (state action)
  "PRIVATE. Returns the new state obtained by applying action to state"
  action)





;;; +-----------------+
;;; | Helper Function |
;;; +-----------------+

;;; Given two city names, determine if the cities are neighboring. Return the
;;; distance between them or NIL. This function assumes that neigboring is an
;;; reflexive relation and the distance from 1 to 2 is the same as the distance
;;; from 2 to 1. For this reason, it will only check one of the neighbor lists.
(defun neighbors-p (name-1 name-2 cities-htable)
  (second (assoc name-1                          ; find name-1 as the key
		 (city-neighbors                 ; in the association list of neighbors
		  (gethash name-2 cities-htable))
		 :test 'city-equal               ; so strings match considering case
		 :key 'city-name)))              ; just looking at a city name





;;; +----------------------------------------+
;;; | Closure Path Cost Function (of a Node) |
;;; +----------------------------------------+

;;; Compute the path cost of a node. In general it does not need to be
;;; a sum of step costs, though in this case it will be.

;;; TODO Make sure there is a guarantee that a node has a path-cost value before
;;; computing the cost of a child
(defun get-path-cost-function (cities-htable)
  "PRIVATE. Returns a closure that returns the path cost associted with a node"
  #'(lambda (node)
      (let ((city-name (node-state node))
	    (prev-city-name (if (node-parent node) ; if parent is not nil
				(node-state (node-parent node))
			      nil)))
	(if (null prev-city-name)
	    0				; if no previous city, no cost
	  (or (node-path-cost node)	; use stored cost if non-nil to
					; prevents re-computing
	      (+ (node-path-cost (node-parent node)) ; cost of parent
		 (neighbors-p city-name prev-city-name cities-htable))))))) ; + dist b/w cities





;;; +------------------------------------------+
;;; | Create Instance of Map Traversal Problem |
;;; +------------------------------------------+

(defun new-map-traversal-problem (map start-city end-city)
  "Returns a problem instance of trying to go from the start-city to the
  end-city in the map"
  (let ((cities-htable (map-to-cities-hash-table map)))
    (make-general-problem
     :init start-city
     :actions (get-actions-generator cities-htable)
     :transition-model 'transition-result
     :goal-test (get-goal-test-function end-city)
     :path-cost (get-path-cost-function cities-htable)
     :state-equal-p 'city-equal)))
