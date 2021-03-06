;;; routines.el --- Small addition to org-mode
;;
;; Copyright (C) 2012-2015 Bjoern Graebe
;;
;; Author: Bjoern Graebe <bjoern.graebe@fernuni-hagen.de>
;; Maintainer: Bjoern Graebe
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: https://github.com/bjoerng/routines.el
;;
;; routines.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; routines.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; TODO:
;;; - Function which determines if there is a routline-tree for today
;;; - Function to insert new Today in an existing tree
;;; - Function which builds a Skeleton-Tree, DONE, needs testing
;;; - Function to prepare GTD-tree for a new year
;;; Commentary:
;;
;;; Code:

;;----------------------------------------------------------------------
;; Constants

(defconst starchar 42)
; The special characters for Regexp in Emacs
(defconst routines-rexexp-special-characters
  '(46 starchar 43 63 94 36 92 91))
(defconst gtd-container-root-string "GTD-Baskets")
(defconst gtd-business-day-string "Business-Days")
(defconst gtd-weekday-string "WeekDays")
(defconst gtd-daymonth-string "DayMonth")
(defconst gtd-weekyear-string "WeekOfTheYear")
(defconst gtd-nonrecurring-string "Nonrecurring")
(defconst gtd-weekday-format "%A")
(defconst gtd-monthday-format "md%d")
(defconst gtd-month-year-string "%B %Y")
(defconst gtd-weak-of-the-year-string "WotY%V %Y")
;;----------------------------------------------------------------------
;; Small functions
(defun routines-weekday () (format-time-string gtd-weekday-format))
(defun routines-monthday () (format-time-string gtd-monthday-format))
(defun routines-monthyear () (format-time-string gtd-month-year-string))
(defun routines-kwyear () (format-time-string gtd-weak-of-the-year-string))
(defun routines-month  () (format-time-string "%B %Y"))
(defun routines-date () (format-time-string "%Y-%m-%d"))
(defun year-now () (nth 5 (decode-time)))

(defun list-of-weekdays ()
  "Returns a list of weekdays in the right language."
  (let (result)
    (dotimes (i 7 result)
      (setq result
	    (cons (format-time-string "%A" (encode-time 0 0 0 (- 4 i)1 1970))
		  result)))))

(defun routines-insert-weekday-skeleton ()
  "Inserts the weekday skeleton in the right language."
  (insert (concat "** " gtd-weekday-string "\n"))
  (mapcar #'(lambda (strng)
	      (insert (concat "*** " strng "\n"))) (list-of-weekdays)))

(defun routines-insert-daymonth-skeleton ()
  "Insert the skeleton for the days of a month."
  (insert (concat "** " gtd-daymonth-string "\n"))
    (let (value)
      (dotimes (i 31 value)
	(insert (concat "*** "
			(format-time-string gtd-monthday-format
					    (encode-time 0 0 0 (1+ i) 1 1970))
			"\n")))))

(defun encode-time-with-week-and-year (week year)
  "Takes a week a year an returns an the encoded time".
  (encode-time 0 0 0 (+ 4 (* week 7)) 1 year))

(defun insert-kw-with-year (week)
  "Insert a week with year, with the prexis '*** '. If the week
is less thean the current week, it inserts the week with a string
representing the next year."
  (insert (concat "*** "
		  (format-time-string
		   gtd-weak-of-the-year-string
		   (encode-time-with-week-and-year
		    week
		    (+ (year-now)
		       (if (time-less-p
			    (encode-time-with-week-and-year week (year-now))
			    (current-time))
			   1 0))))
		  "\n")))

(defun routines-insert-kwyear-skeleton ()
  "Inserts a skeleton for the task to do in the different weeks of a year" 
  (insert (concat "** " gtd-weekyear-string "\n"))
    (let (value)
      (dotimes (i 52 value)
	(insert-kw-with-year i)
	)))

(defun routines-insert-month-skeleton ()
  "Inserts a skeleton for the different month of a year."
  (let (value)
    (dotimes (i 12 value)
      (insert (concat "** " (format-time-string
			     gtd-month-year-string
			     (if (time-less-p
				  (current-time)
				  (encode-time 0 0 0 1 (1+ i) (year-now)))
				 (encode-time 0 0 0 1 (1+ i) (year-now))
			       (encode-time 0 0 0 1 (1+ i) (1+ (year-now)))))
		      "\n")))))
						
(defun routines-create-skeleton ()
  "Create a skeleton for routines"
  (interactive)
  (progn
    (insert (concat "* " gtd-container-root-string "\n"))
    (insert (concat "** " gtd-business-day-string "\n"))
    (routines-insert-weekday-skeleton)
    (routines-insert-daymonth-skeleton)
    (routines-insert-kwyear-skeleton)
    (routines-insert-month-skeleton)
    ))
    
;; '(String Char) -> Number 
(defun routines-count-char-at-beginning (strng chr)
  "Takes a string strng and a charakter char and returns the
  count of chr at the beginnng of strng"
  (let ((result 0)
	(tmpstrng strng))
    (while (= (string-to-char tmpstrng) chr)
	  (setf tmpstrng (substring tmpstrng  1 (length tmpstrng)))
	  (setf result (1+ result)))
    result))

;; String -> String
(defun routines-remove-n-times-char-from-line (strng n char)
  "Removes n times char from the beginning from strng. If char
  does not occur n or more times at the beginning it evaluaties
  to strng."
  (let* ((c (concat  
    (if (member char routines-rexexp-special-characters)
	"\\" "") (format "%c" char)))
	 (regexp-string
	  (concat "^" c "\\{" (format "%i" n) "\\}"))) ;;endLet
    (replace-regexp-in-string regexp-string "" strng)))

;; -> String
(defun build-todays-search-list ()
  "Creates today search list, depending on the day it is executed"
  (cl-labels ((cons-business-days (sl) 
				  (if (business-day-p) 
				      (cons '("GTD-Baskets" "Business-Days") sl)
				    sl)))
      (mapcar #'(lambda (item) (cons "GTD-Baskets" item))
	      (cons-business-days
	      `(("Business-Days")
		("weekdays" ,(routines-weekday))
		("DayMonth" ,(routines-monthday))
		("WeekOfTheYear" ,(routines-kwyear))
		(,(routines-month)))))))

(defun increment-number-at-point ()
  "If there is a number at point, the number will be incremented.
If the is no number at point, the error \"No number at point\"
will be thrown."
      (interactive)
      (skip-chars-backward "0123456789")
      (or (looking-at "[0123456789]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; -> String
(defun routines-weekday () (format-time-string "%A"))
(defun routines-monthday () (format-time-string "md%d"))
(defun routines-monthyear () (format-time-string "%B %Y"))
(defun routines-kwyear () (format-time-string "WotY%V %Y"))
(defun routines-month  () (format-time-string "%B %Y"))

(defun business-day-p () (<= (string-to-number (format-time-string "%u")) 5))
;; This is not a good solution, because it only looks if today is
;; Saturday or Sonday

;;-> String
(defun routines-outline-subtree-to-string (&optional delete-subtree-p)
  "Returns the outline subtree rooted at point as a string. If delete-subtree-p is not nil, the subtree will be removed befor it is returned."
  (let ((beg (point))
	(end (point))
	(return-string nil))
	(save-excursion
	  (beginning-of-line)
	  (setq beg (point))
	  (outline-end-of-subtree)
	  (setq end (point))
	  (setq return-string (buffer-substring beg end))
	  (if delete-subtree-p
	      (delete-region beg end))
	  return-string)))

;; (Strings) --> Boolean
(defun routines-find-outline-subtree-by-stringlist (sl)
  "Moves the cursor to the outline subtree given by the string list sl"
  (let ((search-list 
	 (cl-mapcar #'concat
		  ;; Creates a list of " *^n" strings, while n is in
		  ;; {1 ...(length sl)}
		    (mapcar
		     #'(lambda (n) (concat (make-string n starchar) " "))
		     (list-from-to 1 (length sl)))
		    sl))
	(everything-found nil))
    (mapc #'(lambda (strng) 
	       (progn
		 (push (search-forward strng nil 't) everything-found)
		 (show-subtree)))
	    search-list)
    (cl-every '(lambda (x) x) everything-found)))

;; '(String Boolean) -> String
(defun routines-get-outline-subtree-by-stringlist
    (sl &optional increment delete-subtree-p)
  "Takes a stringlist sl and returns the subtree of the suntree
  in sl. If increment is not nil, the number at the end of the
  header will be incremented."
  (save-excursion
    (let ((search-root nil)
	  (result nil))
      (routines-find-outline-subtree-by-stringlist (list (car sl)))
      (setf search-root (point))
      (if (routines-find-outline-subtree-by-stringlist (cdr sl))
	  (progn 
	    (setf result (routines-outline-subtree-to-string delete-subtree-p))
	    (if increment
		(increment-number-at-point))
	    (goto-char search-root)))
      (message-tree-not-found sl)
      (goto-char search-root)
      (hide-subtree) result)))

(defun message-tree-not-found (string-list)
  "Dsiplays a message thatt the string representation in
sringlist does not lead to a subtree."
  (message (concat
	    (cl-reduce #'(lambda (item1 item2)
			   (concat item1 "->" item2)) 
		       string-list) " not found.")))
    
;; -> String
(defun routines-build-todays-stringlist ()
  "Builds the string list for today to get the right outline-trees."
  (list
   (cons (list gtd-container-root-string gtd-business-day-string) nil)
   (cons (list gtd-container-root-string gtd-weekday-string
	       (routines-weekday))
	 nil)
   (cons (list gtd-container-root-string gtd-daymonth-string
	       (routines-monthday))
	 nil)
   (cons (list gtd-container-root-string gtd-weekyear-string
	       (routines-kwyear))
	 't)
   (cons (list gtd-container-root-string (routines-month)) t)))

(defun build-today-todo-from-today-stringlist (today-stringlist)
  "Take a list of stringlists and return the subtree of the
knot-sequence in the stringlist."
  (mapcar #'(lambda (item)
	      (routines-get-outline-subtree-by-stringlist 
	       (car item)
	       (cdr item)))
	  today-stringlist))

(defun insert-todo-with-proper-starcount (todo)
  "Insert a single todo ith right starcount."
  (progn
    (setf todo-string
	  (routines-remove-n-times-char-from-line todo
	   (- (routines-count-char-at-beginning
	       todo starchar) 2) starchar))
    (insert todo-string)
    (insert "\n")) )

(defun insert-today-todos-with-proper-starcount (todo-list)
  "Inserts todos wit hthe right amount of tacound to fit into the
today-tree."
  (mapcar
     #'(lambda (strng) 
	 (if strng
	     (insert-todo-with-proper-starcount strng)))
     todo-list))
  
(defun routines-insert-today-as-new-bg ()
  "Creates a string of today TODO items as an outline tree and
  inserts them at point and hides the subtree."
  (interactive)
  (let* ((point-position (point))
	 (today-stringlist (routines-build-todays-stringlist))
	 (today-todo-list (build-today-todo-from-today-stringlist
			   today-stringlist))
	  ) ;endLet
    (insert "* Today\n")
    (insert-today-todos-with-proper-starcount today-todo-list)
    (goto-char point-position)
    (hide-subtree)))
