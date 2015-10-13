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
;;; - Function which builds the List of Stringlist, to build the String for 
;;;   today (DONE, needs testing)
;;; - Function which determines if there is a routline-tree for today
;;; - Function to insert new Today in an existing tree
;;; - Function which builds a Skeleton-Tree
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


(defun routines-create-skeleton ()
  "Create a scelet for routines"
  (interactive)
  (progn
    (insert (concat "* " gtd-container-root-string))
    (insert (concat "** " gtd-business-day-string))
    (insert (concat "** " gtd-weekday-string))
    (let ((i 0))
      (while (< i 7)
	(insert
	 (concat "*** " (format-time-string "%A" (encode-time 0 0 0 (+ 5 i) 1 1970)) "\n"))
	(setq i (1+ i))))
    (insert (concat "** "  gtd-daymonth-string))
    (let ((i 1))
      (while (<= i 31)
	(in
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
    (mapc #'(lambda (string) 
	       (progn
		 (push (search-forward string nil 't) everything-found)
		 (show-subtree)))
	    search-list)
    (cl-every '(lambda (x) x) everything-found)))

;; '(String Boolean) -> String
(defun routines-get-outline-subtree-by-stringlist
    (sl &optional increment delete-subtree-p)
  "It increment is not nil, the number at the end of the header
  will be incremented."
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
	    (goto-char search-root))
	    (message-tree-not-found sl)
      (goto-char search-root)
      (hide-subtree) result))))

(defun message-tree-not-found (string-list)
  (message (concat
	    (cl-reduce #'(lambda (item1 item2)
			   (concat item1 "->" item2)) 
		       string-list) " not found.")))


(message-tree-not-found '("1" "2" "3" "4" "5"))
    
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

(defun build-today-to-from-today-stringlist (today-stringlist)
  (mapcar #'(lambda (item)
	      (routines-get-outline-subtree-by-stringlist 
	       (car item)
	       (cdr item)))
	  today-stringlist))

(defun insert-todo-with-proper-starcount (todo)
  (progn
    (setf strng
	  (routines-remove-n-times-char-from-line strng
	   (- (routines-count-char-at-beginning
	       strng starchar) 2) starchar))
    (insert strng)
    (insert "\n")))

(defun insert-today-todos-with-proper-starcount (todo-list)
  (mapcar
     #'(lambda (strng) 
	 (if strng
	     (insert-todo-with-proper-starcount string))
     todo-list)))
  

(defun routines-insert-today-as-new-bg ()
  "Creates a string of today TODO items as an outline tree and
  inserts them at point and hides the subtree."
  (interactive)
  (let* ((point-position (point))
	 (today-stringlist (routines-build-todays-stringlist))
	 (today-todo-list (build-today-to-from-today-stringlist
			   today-stringlist))
	  ) ;endLet
    (insert "* Today\n")
    (insert-today-todos-with-proper-starcount today-todo-list)
    (goto-char point-position)
    (hide-subtree)))
