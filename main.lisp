;;; Jason Vander Woude
;;;
;;; Homework 4
;;; CSCE 875: Articifial Intelligence (University of Nebraska-Lincoln)
;;; Sunday October 11, 2020





;;; This file contains the code specifying the Romanian Holiday instance of a
;;; map traversal problem and provides the top-level user function requested in
;;; the assignment for running the search algorithms. All analysis code to
;;; produce the desired tables is also in this file.





;;; +------------------------------------------------+
;;; | The map to use in the Romanian Holiday problem |
;;; +------------------------------------------------+

(defvar *ROMANIA-MAP*
    '((Arad 366
       ((Zerind 75) (Timisoara 118) (Sibiu 140)))
      (Bucharest 0
       ((Giurgiu 90) (Pitesti 101) (Fagaras 211) (Urziceni 85)))
      (Craiova 160
       ((Dobreta 120) (Rimnicu_Vilcea 146) (Pitesti 138)))
      (Dobreta 242
       ((Mehadia 75) (Craiova 120)))
      (Eforie 161
       ((Hirsova 86)))
      (Fagaras 176
       ((Sibiu 99) (Bucharest 211)))
      (Giurgiu 77
       ((Bucharest 90)))
      (Hirsova 151
       ((Eforie 86) (Urziceni 98)))
      (Iasi 226
       ((Neamt 87) (Vaslui 92)))
      (Lugoj 244
       ((Timisoara 111) (Mehadia 70)))
      (Mehadia 241
       ((Lugoj 70) (Dobreta 75)))
      (Neamt 234
       ((Iasi 87)))
      (Oradea 380
       ((Zerind 71) (Sibiu 151)))
      (Pitesti 100
       ((Craiova 138) (Rimnicu_Vilcea 97) (Bucharest 101)))
      (Rimnicu_Vilcea 193
       ((Sibiu 80) (Pitesti 97) (Craiova 146)))
      (Sibiu 253
       ((Fagaras 99) (Rimnicu_Vilcea 80) (Arad 140) (Oradea 151)))
      (Timisoara 329
       ((Arad 118) (Lugoj 111)))
      (Urziceni 80
       ((Bucharest 85) (Hirsova 98) (Vaslui 142)))
      (Vaslui  199
       ((Iasi 92) (Urziceni 142)))
      (Zerind 374
       ((Arad 75) (Oradea 71)))))





;;; +---------------------------------+
;;; | Top Level User Function: Search |
;;; +---------------------------------+

;;; This is the top-level user function to perform the search. It could not
;;; simply be called "search" because that name is in the COMMON-LISP
;;; package. The search method defaults to using tree-search which is more
;;; robust without problem specific guarantees.
(defun romania-search (name-of-city search-strategy-name &optional (use-graph-search-p nil))
  (let ((prioritizer (get-prioritizer-descriptor search-strategy-name))
	(problem (new-map-traversal-problem *ROMANIA-MAP* ; Use the Romania map abve
					    name-of-city  ; Use input as start city
					    'Bucharest))) ; Use Bucharest as end city

    (if prioritizer			; prioritizer for specified strategy name exists
	(if use-graph-search-p
	    (graph-search
	     problem
	     :priority-function prioritizer)
	  (tree-search
	   problem
	   :priority-function prioritizer))
      "Specified search strategy does not exist. Try A*, greedy, or best-first.")))





;;; +------------------------+
;;; | Format Helper Function |
;;; +------------------------+

(defun csv-output (search-strategy-name use-graph-search-p table-name)
  "Produce comma separated output for use in making a table"
  (let ((head-format-string "~17A, ~8A, ~62A, ~9A, ~8A~2%")
	(body-format-string "~17A, ~8D, ~62A, ~9D, ~8D~1%"))

    ;; table header
    (format t "~A~2%" table-name)
    (format t head-format-string
	    "City Name"
	    "Expanded"
	    "Path to Bucharest"
	    "Path Cost"
	    "CPU Time")

    ;; rest of the table; one row per start city
    (dolist (start-city (mapcar 'first *ROMANIA-MAP*))
      (let* ((solution   (romania-search start-city search-strategy-name use-graph-search-p))
	     (path       (get-state-sequence solution))
	     (cost       (node-path-cost (general-solution-node solution)))
	     (extra      (general-solution-extra solution))
	     (n-expanded (second (assoc 'n-expanded extra)))
	     (cpu-time   (second (assoc 'cpu-time extra))))
	(format t body-format-string
		start-city
		n-expanded
		path
		cost
		cpu-time)))
    (format t "~5%")))





;;; +---------------------+
;;; | Generate Table Data |
;;; +---------------------+

(csv-output "best-first" nil "BEST-FIRST TREE SEARCH")
(CSV-output "greedy"     nil "GREADY TREE SEARCH")
(csv-output "A*"         nil "A* TREE SEARCH")

(csv-output "best-first" t   "BEST-FIRST GRAPH SEARCH")
(CSV-output "greedy"     t   "GREADY GRAPH SEARCH")
(csv-output "A*"         t   "A* GRAPH SEARCH")
