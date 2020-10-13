;;; +-----------+
;;; | heap.lisp |
;;; +-----------+
;;;
;;; Jason Vander Woude
;;;
;;; Homework 4
;;; CSCE 875: Articifial Intelligence (University of Nebraska-Lincoln)
;;; Sunday October 11, 2020





;;; This file contains a VERY INEFFICIENT quick and dirty heap. The
;;; purpose of the assignment is to get search strategies working
;;; which rely on a heap. This serves the purposes required for that
;;; level of abstraction.





(defstruct heap
  list      ; values in the heap
  less-fn)  ; ordering function


(defun heap-empty-p (h)
  (not (heap-list h)))


(defun create-heap (less-fn)
  (let ((result (make-heap)))
    (setf (heap-less-fn result) less-fn)
    result))


(defun heap-insert (h x)
  (push x (heap-list h))
  (setf (heap-list h) (sort (heap-list h) (heap-less-fn h))))


(defun heap-remove (h)
  (pop (heap-list h)))