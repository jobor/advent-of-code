;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(setq input-file (elt command-line-args-left 0))
(message "input-file: %s" input-file)

(with-temp-buffer
  (insert-file-contents input-file)
  (goto-char (point-min))
  (let ((sum 0)
        (enabled t))
    (while (re-search-forward "\\(do()\\)\\|\\(don't()\\)\\|\\(?:mul(\\([[:digit:]]+\\),\\([[:digit:]]+\\))\\)" nil :noerror)
      (cond
       ((match-string 1)
        (message "ON")
        (setq enabled t))
       ((match-string 2)
        (message "OFF")
        (setq enabled nil))
       (enabled
        (let ((a (string-to-number (match-string 3)))
              (b (string-to-number (match-string 4))))
          (message "%s * %s = %s" a b (* a b))
          (cl-incf sum (* a b))))))
      (message "sum: %s" sum)))
