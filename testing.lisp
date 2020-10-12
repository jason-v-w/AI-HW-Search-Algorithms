(setq problem (new-map-traversal-problem *ROMANIA-MAP*
					 'Arad
					 'Bucharest))


(setq myeq (general-problem-state-equal-p problem))
(funcall myeq 'arad 'arad)
(funcall myeq 'arad 'bucharest)
(graph-search problem :test 'city-equal :eval-function 'best-first-evaluator)
(graph-search problem :test (general-problem-state-equal-p problem))

      (setq n1 (make-node :parent nil
		    :state 'arad
		    :path-cost 0
		    :heuristic nil))

      (setq n2 (make-node :parent nil
		    :state 'bucharest
		    :path-cost 100
		    :heuristic nil))
      (print n1)
(print n2)

