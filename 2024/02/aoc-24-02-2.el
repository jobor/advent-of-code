;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(setq input-file (elt command-line-args-left 0))

(defun read-input-from-file (file)
  "Return a list of 'reports', read from FILE."
  (let ((content-list nil)
        (left-list nil)
        (right-list nil)
        (is-left t))
    (with-temp-buffer
      (insert-file-contents input-file)

      (goto-char (point-min))
      (insert "(\n")
      (while (not (eobp))
        (move-beginning-of-line nil)
        (insert "(")
        (move-end-of-line nil)
        (insert ")")
        (next-line))

      (insert ")")
      (goto-char (point-min))
      (insert "(")

      (setq content-list (read (current-buffer)))
      content-list)))

(defun report-dampened-safe-p (report)
  (if (report-safe-p report)
      t
    (let ((candidates))
      (dotimes (i (length report))
        (push (append (seq-subseq report 0 i) (seq-subseq report (+ i 1))) candidates))
      (seq-some #'report-safe-p candidates))))

(defun report-safe-p (report)
  (message "checking report %s" report)
  (when (report-monotonously-rising-p report)
    (message "* rising"))
  (when (report-monotonously-falling-p report)
    (message "* falling"))
  (when (report-diffs-in-range report)
    (message "* diffs in range"))
  (and (report-monotonous-p report)
       (report-diffs-in-range report)))

(defun report-monotonous-p (report)
  (or (report-monotonously-rising-p report)
      (report-monotonously-falling-p report)))

(defun report-monotonously-rising-p (report)
  (if (> (length report) 1)
      (and (< (car report) (cadr report))
           (report-monotonously-rising-p (cdr report)))
    t))

(defun report-monotonously-falling-p (report)
  (if (> (length report) 1)
      (and (> (car report) (cadr report))
           (report-monotonously-falling-p (cdr report)))
    t))

(defun report-diffs-in-range (report)
  (let ((max-abs-diff 0)
        (a (car report)))
    (dolist (b (cdr report))
      (let ((abs-diff (abs (- a b))))
        (when (< max-abs-diff abs-diff)
          (setq max-abs-diff abs-diff)))
      (setq a b))
    (<= max-abs-diff 3)))

(defun solve (reports)
  (message "reports: %s" reports)
  (length (seq-filter #'report-dampened-safe-p reports)))

(message "number of valid reports: %d"
         (solve (read-input-from-file input-file)))
