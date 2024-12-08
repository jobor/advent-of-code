;; -*- lexical-binding: t; -*-

;; To solve this, we try to place an obstruction on every position of the trail (that we compute
;; first).  There's quite some optimization potential here.  Using a regular Emacs buffer as data
;; structure is abysmally slow...

(require 'cl-lib)

(setq input-file (elt command-line-args-left 0))
(message "input-file: %s" input-file)

(defun find-guard-pos ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "[\\^v<>]" nil :noerror)
      (left-char)
      (point))))

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

(cl-defun simulate-step (guard-pos)
  (interactive (list (find-guard-pos)))
  (goto-char guard-pos)
  (let ((guard-direction (char-after guard-pos))
        (new-guard-pos)
        (new-guard-direction))
    ;; (message "guard is at %s facing %s" guard-pos (direction-to-string guard-direction))

    ;; calculate the guard's new position
    (setq new-guard-direction guard-direction)
    (while (obstacle-in-direction-p guard-pos new-guard-direction)
        (setq new-guard-direction (rotate-direction new-guard-direction)))
    (setq new-guard-pos (next-position-in-direction guard-pos new-guard-direction))

    (unless (and new-guard-pos
                 (eq (char-after new-guard-pos) ?.))
      ;; (message "guard could not walk")
      (cl-return-from simulate-step))

    ;; clear the guard's old position
    (replace-char-at-point ?.)

    ;; set the guard to the new position
    ;; (message "guard walks to %s" new-guard-pos)
    (goto-char new-guard-pos)
    (replace-char-at-point new-guard-direction)

    ;; return the new position and direction
    (cl-values new-guard-pos new-guard-direction)))

(defun string-from-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun has-duplicate-first-pair (numbers)
  (let* ((len (length numbers))
         (first-pair (when (>= len 2)
                      (list (nth 0 numbers)
                            (nth 1 numbers)))))
    (and first-pair
         (let ((found nil)
               (i 2))
           (while (and (not found)
                      (< (1+ i) len))
             (when (equal (list (nth i numbers)
                              (nth (1+ i) numbers))
                        first-pair)
               (setq found t))
             (setq i (1+ i)))
           found))))

(cl-defun run-simulation (initial-state &key obstruction)
  (with-temp-buffer
    (insert initial-state)
    (when (numberp obstruction)
      (goto-char obstruction)
      (replace-char-at-point ?#))
    (goto-char (point-min))
    (let* ((guard-pos (find-guard-pos))
           (guard-direction (char-after guard-pos))
           (guard-trail)
           (seen (make-hash-table :test #'equal))
           (simulation-result 'normal))
      (while (and guard-pos (eq simulation-result 'normal))
        (push guard-pos guard-trail)
        (puthash (list guard-pos guard-direction) t seen)
        ;; (message "trail is %s" guard-trail)
        (cl-multiple-value-bind (new-pos new-direction) (simulate-step guard-pos)
          (setq guard-pos new-pos)
          (setq guard-direction new-direction))
        ;; (message "simulate-step returned %s %s" guard-pos guard-direction)

        ;; Check for a loop in the trail.
        (when (gethash (list guard-pos guard-direction) seen)
          ;; (message "LOOP detected")
          (setq simulation-result 'loop-detected)))

      ;; Return how the simulation ended and the guard trail.
      (cl-values simulation-result guard-trail))))

(defvar obstruction-count 0)

(when input-file
  (let* ((initial-state (string-from-file input-file)))
    (cl-multiple-value-bind (simulation-result trail) (run-simulation initial-state)
      (message "Running first simulation to get the guard trail without obstructions...")
      (message "Simulation result: %s" simulation-result)
      (message "The guard walked %s steps." (length trail))

      (let* ((distinct-positions (delete-dups trail))
             (nr-of-distinct-positions (length distinct-positions))
             (nr-of-obstructions-to-try (- nr-of-distinct-positions 1))
             (nr-of-obstructions-tried 0))
        (message "The guard was on %s distinct positions." nr-of-distinct-positions)
        (dolist (pos (butlast distinct-positions))
          (cl-incf nr-of-obstructions-tried)
          (message "Trying obstruction at position %s (%s/%s)" pos nr-of-obstructions-tried nr-of-obstructions-to-try)
          (when (eq 'loop-detected (car (run-simulation initial-state :obstruction pos)))
            (message "LOOP!")
            (cl-incf obstruction-count))))

      (message "Possible obstruction positions: %s" obstruction-count))))
