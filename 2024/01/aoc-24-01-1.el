;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(setq input-file (elt command-line-args-left 0))
(message "input-file: %s" input-file)

(defun read-input-from-file (file)
  "Return a list of two lists, read from the input file."
  (let ((content-list nil)
        (left-list nil)
        (right-list nil)
        (is-left t))
    (with-temp-buffer
      (insert "(")
      (insert-file-contents input-file)
      (goto-char (point-max))
      (insert ")")

      (goto-char (point-min))
      (setq content-list (read (current-buffer)))
      ;; (message "content-list %s" content-list)

      (dolist (elem content-list)
        (push elem (if is-left left-list right-list))
        (setq is-left (not is-left)))

      (list left-list right-list))))

(cl-destructuring-bind (left right) (read-input-from-file input-file)
  (setq left (sort left #'<))
  (setq right (sort right #'<))
  (let ((sum 0))
    (while left
      (let* ((a (pop left))
             (b (pop right))
             (diff (abs (- a b))))
        ;; (message "pair: %s %s, diff: %s" a b diff)
        (cl-incf sum diff)))
    (message "sum of differences: %s" sum)))

