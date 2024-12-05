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
  ;; (message "update-adheres-to-rule %s %s" rule update)
  (let ((left)
        (valid t))
    (cl-loop for value in update
             for index from 0
             while valid
             do
             (when (and (eq value (car rule))
                        (memq (cadr rule) left))
               ;; (message "update %s violates rule %s" update rule)
               ;; (message "left: %s" left)
               (setq valid nil))
             (push value left))
    valid))

(defun update-adheres-to-rules (rules update)
  (seq-every-p (lambda (rule)
              (update-adheres-to-rule rule update))
            rules))

(defun find-first-violated-rule-in-update (rules update)
  (seq-find (lambda (rule) (not (update-adheres-to-rule rule update))) rules))

(defun fix-update (rules update)
  "Fix the update according to the rules.
1. Find a rule that's violated.
2. Swap the values in update that correspond to rule.
3. Repeat until no violated rule is found."
  (message "fixing update %s" update)
  (let ((fixed-update (copy-tree update)))
    (while-let ((rule (find-first-violated-rule-in-update rules fixed-update))
                (i (cl-position (car rule) fixed-update))
                (k (cl-position (cadr rule) fixed-update)))
      (message "the update violates rule: %s" rule)
      (setf (nth i fixed-update) (cadr rule))
      (setf (nth k fixed-update) (car rule))
      (message "fix for rule %s: %s" rule fixed-update))
    fixed-update))

(defun invalid-updates (rules updates)
  (seq-filter (lambda (update)
                (not (update-adheres-to-rules rules update)))
              updates))

(defun fixed-invalid-updates (rules updates)
  (mapcar (lambda (update)
            (fix-update rules update))
          (invalid-updates rules updates)))

(defun middle-value-of-update (update)
  (elt update (/ (length update) 2)))

(defun middle-values-of-fixed-invalid-updates (rules updates)
  (mapcar #'middle-value-of-update (fixed-invalid-updates rules updates)))

(defun solve (rules updates)
  (seq-reduce #'+ (middle-values-of-fixed-invalid-updates rules updates) 0))

(cl-destructuring-bind (rules updates) (parse-input input-file)
  (message "solution: %s" (solve rules updates)))
