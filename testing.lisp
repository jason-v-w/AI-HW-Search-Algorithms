(load "/home/jason/Dropbox/classes/csce_876_artificial_intelligence/hw_4/lisp/general_problem_framework.lisp")
(load "/home/jason/Dropbox/classes/csce_876_artificial_intelligence/hw_4/lisp/map_traversal_problem_description.lisp")
(load "/home/jason/Dropbox/classes/csce_876_artificial_intelligence/hw_4/lisp/search_framework.lisp")
; (load "/home/jason/Dropbox/classes/csce_876_artificial_intelligence/hw_4/lisp/heap.lisp")
(load "/home/jason/Dropbox/classes/csce_876_artificial_intelligence/hw_4/lisp/my_heap.lisp")
(load "/home/jason/Dropbox/classes/csce_876_artificial_intelligence/hw_4/lisp/main.lisp")




(setq problem (new-map-traversal-problem *ROMANIA-MAP*
					 'Arad
					 'Bucharest))


(setq myeq (general-problem-state-equal-p problem))
(funcall myeq 'arad 'arad)
(funcall myeq 'arad 'bucharest)
(graph-search problem :test 'city-equal :priority-function 'best-first-prioritizer)
; (graph-search problem :test 'city-equal :eval-function 'best-first-evaluator)
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

