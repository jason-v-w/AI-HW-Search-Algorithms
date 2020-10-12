;;; Jason Vander Woude
;;;
;;; Homework 4
;;; CSCE 875: Articifial Intelligence (University of Nebraska-Lincoln)
;;; Sunday October 11, 2020





;;; This file contains the code specifying and running search algorithms





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
	     :test (general-problem-state-equal-p problem)
	     :priority-function prioritizer))
      (tree-search
       problem
       :priority-function prioritizer))
    "Specified search strategy does not exist. Try A*, greedy, or best-first."))
