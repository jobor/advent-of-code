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
  (let ((score 0)
        (h (make-hash-table)))

    ;; Count occurrences in the right list.
    (dolist (e right)
      (puthash e (+ 1 (or (gethash e h) 0)) h)
      )

    ;; Accumulate the scores for the values in the left list.
    (while left
      (let* ((a (pop left))
             (count-in-right (or (gethash a h) 0)))
        ;; (message "a: %s, count-in-right: %s" a count-in-right)
        (cl-incf score (* a count-in-right))
        ))
    (message "accumulated score: %s" score)))

