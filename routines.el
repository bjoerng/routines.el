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
;;----------------------------------------------------------------------

;; This is a mess!
(defun routines-insert-skeleton ()
  (interactive)
  (insert "* GTD-Baskets\n")
  (insert "** Business-Days\n")
  (insert "** WeekDays\n")
  ;; The following solutions is a workaround, until i find out how to
  ;; insert the days in a week in a smarter way.
  (insert (concat "*** "
		  (format-time-string "%A" (date-to-time "Tue, 05-Jan-70 00:00:1 EST" )) "\n"))
  (insert (concat "*** "
		  (format-time-string "%A" (date-to-time "Tue, 06-Jan-70 00:00:1 EST")) "\n"))
  (insert (concat "*** "
		  (format-time-string "%A" (date-to-time "Tue, 07-Jan-70 00:00:1 EST"))) "\n")
  (insert (concat "*** "
		  (format-time-string "%A" (date-to-time "Tue, 08-Jan-70 00:00:1 EST")) "\n"))
  (insert (concat "*** "
		  (format-time-string "%A" (date-to-time "Tue, 09-Jan-70 00:00:1 EST")) "\n"))
  (insert (concat "*** "
		  (format-time-string "%A" (date-to-time "Tue, 10-Jan-70 00:00:1 EST")) "\n"))
  (insert (concat "*** "
		  (format-time-string "%A" (date-to-time "Tue, 11-Jan-70 00:00:1 EST")) "\n"))
  ;; Now the Days in Month are inserted 
  (insert "** DayMonth\n")
  (let ((dm 1))
    (while (<= dm 31)
      (insert (concat "*** md" (number-to-string dm) "\n"))
      (setf dm (1+ dm))))
  ;; Now the weeks in a years are inserted, by determining the actual
  ;; year, and the actual week. As long as tmpweek ist lower or equal
  ;; than thisweek, the next year will be inserted. When the tmpweek
  ;; ist greater than thisweek, the actual year will be inserted.
  (insert "** WeekOfTheYear\n")
  (let ((thisyear (string-to-number (format-time-string "%Y"))))
    (let ((thisweek (string-to-number (format-time-string "%V")))
	  (tmpweek 1))
      (while (<= tmpweek 52)
	(if (< tmpweek thisweek) 
	    (insert (format  "*** WotY%02d %d\n" tmpweek thisyear))
	  (insert (format  "*** WotY%02d %d\n" tmpweek (1+ thisyear))))
      (setf tmpweek (1+ tmpweek))))
    ;; Now The Month are inserted analog to the weeks.
    (mapc #'
     (lambda (item) (insert (concat "** "(car item) " " (number-to-string (cdr item)) "\n")))
	 (cl-mapcar #'cons
		    (let ((month 12)
			  (result nil))
		      (while (>= month 1)
			(push (format-time-string "%B" 
						  (date-to-time
						   (format "1970-%d-05 00:01 EST" month))) 
			      result)
			(setf month (1- month)))
		      result)
		    (let ((this-month (string-to-number (format-time-string "%m"))))
		      (append (make-list this-month thisyear)
			      (make-list (- 12 9) (1+ thisyear))))))))

;; '(String Char) -> Number 
(defun routines-count-char-at-beginning (strng chr)
  "Takes a string strng and a charakter char and returns
the count of chr at the beginnng of strng"
  (let ((result 0)
	(tmpstrng strng))
    (while (= (string-to-char tmpstrng) chr)
	  (setf tmpstrng (substring tmpstrng  1 (length tmpstrng)))
	  (setf result (1+ result)))
    result))

;; String -> String
;; Bad solution, I am working on a new one. See below.
(defun routines-remove-n-stars-from-line (strng n)
  ;; Removes n 
  (let ((starstring "")
	(i 0))
    (while (< i n)
      (setf starstring (concat "\\*" starstring))
      (setf i (1+ i)))
    (setf starstring (concat "^" starstring))
    (replace-regexp-in-string starstring "" strng)))

;; This is meant to replace routines-remove-n-stars-from-line.
(defun routines-remove-n-times-char-from-line (strng n char)
  (let* ((c (concat  
    (if (member char routines-rexexp-special-characters)
	"\\" "") (format "%c" char)))
	 (regexp-string
	  (concat "^" c "\\{" (format "%i" n) "\\}")))
    (replace-regexp-in-string regexp-string "" strng)))

;; -> String
(defun build-todays-search-list ()
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
(defun routines-outline-subtree-to-string ()
  (let ((beg (point))
	(end (point)))
	(save-excursion
	  (beginning-of-line)
	  (setq beg (point))
	  (outline-end-of-subtree)
	  (setq end (point))
	  (buffer-substring beg end))))

;; (Strings) --> Boolean
(defun routines-find-outline-subtree-by-stringlist (sl)
  "Moves the cursor the Outline-Subtree given by the string list sl"
  (let ((search-list 
	 (cl-mapcar #'concat
		  ;; Creates a list of " *^n" strings, while n is in
		  ;; {1 ...(length ls)}
		  (mapcar #'(lambda (n) (concat (make-string n starchar) " "))
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
(defun routines-get-outline-subtree-by-stringlist (sl increment)
  ;; It increment is not nil, the number at the end of the header will be incremented
  (save-excursion
    (let ((search-root nil)
	  (result nil))
      (routines-find-outline-subtree-by-stringlist (list (car sl)))
      (setf search-root (point))
      (if (routines-find-outline-subtree-by-stringlist (cdr sl))
	  (progn 
	    (setf result (routines-outline-subtree-to-string))
	    (if increment
		(increment-number-at-point))
	    (goto-char search-root))
	    (message (concat (cl-reduce #'(lambda (item1 item2) 
					    (concat item1 "->" item2)) 
				     sl) 
			     " not found.")))
      (goto-char search-root)
      (hide-subtree)
	    result)))

;; -> String
(defun routines-build-todays-stringlist ()
  (list
;   (cons (list gtd-container-root-string gtd-business-day-string) nil)
   (cons (list gtd-container-root-string gtd-weekyear-string (routines-weekday)) nil)
   (cons (list gtd-container-root-string gtd-daymonth-string (routines-monthday)) nil)
   (cons (list gtd-container-root-string gtd-weekyear-string (routines-kwyear)) 't)
   (cons (list gtd-container-root-string (routines-month)) t)))

;; 
(defun routines-insert-today-as-new-bg () 
  (interactive)
  (let* ((point-position (point))
	 (today-stringlist (routines-build-todays-stringlist)))
    (insert "* Today\n")
    (mapcar #'(lambda (strng) 
		(if strng
		    (progn
		      (setf strng (routines-remove-n-times-char-from-line 
				    strng
				    (- (routines-count-char-at-beginning strng starchar)
				       2)
				    starchar))
		      (insert strng)
		      (insert "\n"))))
	    (mapcar #'(lambda (item) (routines-get-outline-subtree-by-stringlist 
				      (car item)
				      (cdr item)))
		    today-stringlist))
    (goto-char point-position)
  (hide-subtree)))
