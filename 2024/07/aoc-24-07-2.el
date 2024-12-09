;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;; If we're running in batch-mode, read command-line arguments.
(when noninteractive
  (setq input-file (elt command-line-args-left 0))
  (unless input-file
    (error "No input file given"))
  (message "input-file: %s" input-file))

(defun read-input (file)
  (let ((result))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (line (string-split (buffer-string) "\n"))
        (let ((split-by-colon (string-split line ":")))
          (when (length> split-by-colon 1)
            (let ((equation-result (string-to-number (car split-by-colon)))
                  (operands (mapcar #'string-to-number
                                    (string-split (string-trim (cadr split-by-colon)) " "))))
              (push (list equation-result operands) result)))))
      result)))

(defun number-concat (x y)
  (string-to-number (concat (number-to-string x) (number-to-string y))))

(cl-defun equation-can-be-satisfied-p (equation)
  "Return non-nil if the equation can be satisfied."
  (let ((equation-result (car equation))
        (operands (cadr equation)))
    ;; (message "%s <- %s" equation-result operands)
    (if (eq (length operands) 1)
        (eq equation-result (car operands))
      (dolist (operator '(+ * number-concat))
        ;; (message "trying operator %s" operator)
        (when (equation-can-be-satisfied-p
               (list equation-result
                     (append (list (funcall operator (car operands) (cadr operands)))
                             (cddr operands))))
          (cl-return-from equation-can-be-satisfied-p t))))))

(defun solve (equations)
  "Return the number of equations that can be satisfied with the operators + and *."
  (seq-reduce #'+ (mapcar #'car (seq-filter #'equation-can-be-satisfied-p equations)) 0))

(when noninteractive
  (message "total calibration result: %s" (solve (read-input input-file)))
  )
