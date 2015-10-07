
;; int, int --> '(ints)
(defun list-from-to (from to)
  ;; Return a List of Integers which begins at from and ends with to
  (let ((counter to)
	(result nil))
    (while (<= from counter)
      (push counter result)
      (setf counter (1- counter)))
    result))
