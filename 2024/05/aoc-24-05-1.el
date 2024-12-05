;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(setq input-file (elt command-line-args-left 0))
(unless input-file
  (error "No input file given"))
(message "input-file: %s" input-file)

(defun parse-input (file)
  "Read the input file and return a list.
The first element is the list of rules.
The second element is the list of updates."
  (let ((rules)
        (updates))
    (with-temp-buffer
      (insert-file-contents input-file)

      ;; read rules
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:digit:]]+\\)|\\([[:digit:]]+\\)[[:space:]]*$" nil :noerror)
        (message "rule %s|%s" (match-string 1) (match-string 2))
        (let ((rule (list (string-to-number (match-string 1))
                          (string-to-number (match-string 2)))))
          (push rule rules)))

      ;; read updates
      (goto-char (point-min))
      (while (re-search-forward "^[[:digit:]]*,.*" nil :noerror)
        (let ((update (mapcar #'string-to-number
                              (string-split (match-string 0) ","))))
          (message "update %s" update)
          (push update updates))))

    ;; return the result
    (list rules updates)))

(cl-defun update-adheres-to-rule (rule update)
  (let ((left)
        (valid t))
    (cl-loop for value in update
             for index from 0
             while valid
             do
             (when (and (eq value (car rule))
                        (memq (cadr rule) left))
               ;; (message "update %s violates rule %s" update rule)
               (setq valid nil))
             (push value left))
    valid))

(defun update-adheres-to-rules (rules update)
  (seq-every-p (lambda (rule)
              (update-adheres-to-rule rule update))
            rules))

(defun valid-updates (rules updates)
  (seq-filter (lambda (update)
                (update-adheres-to-rules rules update))
              updates))

(defun middle-value-of-update (update)
  (elt update (/ (length update) 2)))

(defun middle-values-of-valid-updates (rules updates)
  (mapcar #'middle-value-of-update (valid-updates rules updates)))

(defun solve (rules updates)
  (seq-reduce #'+ (middle-values-of-valid-updates rules updates) 0))

(cl-destructuring-bind (rules updates) (parse-input input-file)
  (message "solution: %s" (solve rules updates)))
