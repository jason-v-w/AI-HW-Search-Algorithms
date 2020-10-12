; (setq mine (create-heap #'(lambda (x) (- x))))
; (setq mine (create-heap #'<=))
; (heap-insert mine 5)
; (heap-insert mine 4)
; (heap-insert mine 2)
; (heap-insert mine 10)
; (heap-insert mine 2)
; (heap-insert mine 3)
; (heap-insert mine 3)
; (heap-insert mine 10)

; (heap-remove mine)


; (apply 'min (heap-list mine))



; (defstruct heap
;   list
;   eval-fn)


; (defun heap-empty-p (h)
;   (not (heap-list h)))


; (defun create-heap (evaluation-fn)
;   (let ((result (make-heap)))
;     (setf (heap-eval-fn result) evaluation-fn)
;     result))


; (defun heap-insert (h x)
;   (push x (heap-list h)))


; (defun heap-remove (h)
;   (if (null (heap-list h))
;       nil
;     (progn
;       (let ((minimum (apply 'min (mapcar (heap-eval-fn h) (heap-list h)))))
; 	(setf (heap-list h)
; 	  (remove-if #'(lambda (x) (= minimum x))
; 		     (heap-list h)
; 		     :key (heap-eval-fn h)
					; 		     :count 1))))))


(defstruct heap
  list
  less-fn)


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