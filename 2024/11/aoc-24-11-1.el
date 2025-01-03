;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;; If we're running in batch-mode, read command-line arguments.
(when noninteractive
  (setq input-file (elt command-line-args-left 0))
  (unless input-file
    (error "No input file given"))
  (message "input-file: %s" input-file))

(defun read-input (file)
  (with-temp-buffer
    (insert ?()
    (insert-file-contents file)
    (goto-char (point-max))
    (insert ?))
    (goto-char (point-min))
    (read (current-buffer))))

(defun rule1 (stone)
  (if (eql 0 stone)
      (list 1)))

(defun rule2 (stone)
  (let ((nr-of-digits (1+ (floor (log stone 10)))))
    (when (cl-evenp nr-of-digits)
      (let* ((half-nr-of-digits (/ nr-of-digits 2))
             (b (expt 10 half-nr-of-digits))
             (left (/ stone b))
             (right (- stone (* left b))))
        (list left right)))))

(defun rule3 (stone)
  (list (* 2024 stone)))

(defun apply-rules (initial-stones)
  (let ((rules '(rule1 rule2 rule3))
        (stones))
    (dolist (stone initial-stones)
      (cl-loop for rule in rules
               do
               (when-let ((rule-result (funcall rule stone)))
                 (setq stones (append stones rule-result))
                 (cl-return))))
    ;; (message "%s" stones)
    stones))

(defun solve (stones)
  (message "initial setup: %s" stones)
  (dotimes (_ 25)
    (setq stones (apply-rules stones)))
  (length stones))

(when noninteractive
  (message "benchmark %s"
           (benchmark-run 1
             (message "solution: %s" (solve (read-input input-file))))))
