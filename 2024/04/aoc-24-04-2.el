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

(defun char-in-direction (direction)
  (save-excursion
    (when (walk-into-direction direction)
      (char-after (point)))))

(defun is-X-MAS-at-point ()
  (let ((nw (char-in-direction 'NW))
        (ne (char-in-direction 'NE))
        (sw (char-in-direction 'SW))
        (se (char-in-direction 'SE)))
    (and
     (or
      (and
       (eq nw ?M)
       (eq se ?S))
      (and
       (eq nw ?S)
       (eq se ?M)))
     (or
      (and
       (eq ne ?M)
       (eq sw ?S))
      (and
       (eq ne ?S)
       (eq sw ?M))))))

(let ((xmas-count 0)
      (case-fold-search nil))
  (with-temp-buffer
    (insert-file-contents input-file)
    (goto-char (point-min))
    (while (search-forward "A" nil :noerror)
      (backward-char)
      (when (is-X-MAS-at-point)
        (cl-incf xmas-count))
      (forward-char))
  (message "number of X-MAS occurrences: %s" xmas-count)))
