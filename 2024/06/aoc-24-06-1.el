;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(setq input-file (elt command-line-args-left 0))
;; (unless input-file
  ;; (error "No input file given"))
(message "input-file: %s" input-file)

(defun find-guard-pos ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "[\\^v<>]")
    (left-char)
    (point)))

(defun direction-to-string (direction)
  (cond
   ((eq direction ?^) "north")
   ((eq direction ?v) "south")
   ((eq direction ?>) "east")
   ((eq direction ?<) "west")))

(defun replace-char-at-point (new-char)
  (save-excursion
    (delete-char 1)
    (insert new-char)))

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
   ((eq direction ?>) (walk-east))
   ((eq direction ?<) (walk-west))
   ((eq direction ?v) (walk-south))
   ((eq direction ?^) (walk-north))
   (t (error "Unknown direction"))))

(defun rotate-direction (direction)
  (cond
   ((eq direction ?>) ?v)
   ((eq direction ?<) ?^)
   ((eq direction ?v) ?<)
   ((eq direction ?^) ?>)))

(defun next-position-in-direction (pos direction)
  (save-excursion
    (goto-char pos)
    (when (walk-into-direction direction)
      (point))))

(defun obstacle-in-direction-p (pos direction)
  (if-let ((pos (next-position-in-direction pos direction)))
      (eq (char-after pos) ?#)))

(defun simulate-step (guard-pos)
  (interactive (list (find-guard-pos)))
  (goto-char guard-pos)
  (let ((guard-direction (char-after guard-pos))
        (new-guard-pos)
        (new-guard-direction))
    (message "guard is at %s facing %s" guard-pos (direction-to-string guard-direction))

    ;; clear the guard's old position
    (replace-char-at-point ?X)

    ;; calculate the guard's new position
    (setq new-guard-direction guard-direction)
    (while (obstacle-in-direction-p guard-pos new-guard-direction)
        (setq new-guard-direction (rotate-direction new-guard-direction)))
    (setq new-guard-pos (next-position-in-direction guard-pos new-guard-direction))

    ;; set the guard to the new position
    (if new-guard-pos
        (progn
          (message "guard walks to %s" new-guard-pos)
          (goto-char new-guard-pos)
          (replace-char-at-point new-guard-direction))
      (message "guard could not walk"))

    new-guard-pos))

(when input-file
  (with-temp-buffer
    (insert-file-contents input-file)
    (goto-char (point-min))
    (let* ((guard-pos (find-guard-pos))
           (guard-trail (list guard-pos)))
      (while guard-pos
        (push guard-pos guard-trail)
        (setq guard-pos (simulate-step guard-pos)))
      (message "The guard walked %s steps." (length guard-trail))
      (message "The guard was on %s distinct positions." (length (delete-dups guard-trail))))))
