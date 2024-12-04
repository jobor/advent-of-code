;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(setq input-file (elt command-line-args-left 0))
(message "input-file: %s" input-file)

(defun walk-east ()
  (let ((initial-line (line-number-at-pos))
        (initial-column (current-column)))
    (ignore-errors
      (right-char))
    (and (not (eq initial-column (current-column)))
         (eq initial-line (line-number-at-pos)))))

(defun walk-west ()
  (let ((initial-line (line-number-at-pos))
        (initial-column (current-column)))
    (ignore-errors
      (left-char))
    (and (not (eq initial-column (current-column)))
         (eq initial-line (line-number-at-pos)))))

(defun walk-north ()
  (let ((initial-line (line-number-at-pos))
        (initial-column (current-column)))
    (ignore-errors
      (forward-line -1)
      (forward-char initial-column))
    (and (not (eq initial-line (line-number-at-pos)))
         (eq initial-column (current-column)))))

(defun walk-south ()
  (let ((initial-line (line-number-at-pos))
        (initial-column (current-column)))
    (ignore-errors
      (forward-line 1)
      (forward-char initial-column))
    (and (not (eq initial-line (line-number-at-pos)))
         (eq initial-column (current-column)))))

(defun walk-into-direction (direction)
  (cond
   ((eq direction 'E) (walk-east))
   ((eq direction 'W) (walk-west))
   ((eq direction 'S) (walk-south))
   ((eq direction 'N) (walk-north))
   ((eq direction 'NW) (and (walk-west) (walk-north)))
   ((eq direction 'NE) (and (walk-east) (walk-north)))
   ((eq direction 'SW) (and (walk-west) (walk-south)))
   ((eq direction 'SE) (and (walk-east) (walk-south)))
   (t (error "Unknown direction"))))

(defun string-at-point-in-direction (s direction)
  (save-excursion
    (and (eq (char-after (point)) (aref s 0))
         (or (eq (length s) 1)
             (and
              (walk-into-direction direction)
              (string-at-point-in-direction (substring s 1) direction))))))

(defun count-xmas-strings-at-point ()
  (let ((result 0))
    (dolist (direction '(N E S W NE SE SW NW))
      (when (string-at-point-in-direction "XMAS" direction)
        (cl-incf result)))
    result))

(let ((xmas-count 0)
      (case-fold-search nil))
  (with-temp-buffer
    (insert-file-contents input-file)
    (goto-char (point-min))
    (while (search-forward "X" nil :noerror)
      (backward-char)
      (cl-incf xmas-count (count-xmas-strings-at-point))
      (forward-char))
  (message "number of XMAS occurrences: %s" xmas-count)))

