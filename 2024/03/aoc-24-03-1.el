;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(setq input-file (elt command-line-args-left 0))
(message "input-file: %s" input-file)

(with-temp-buffer
  (insert-file-contents input-file)
  (goto-char (point-min))
  (let ((sum 0))
    (while (re-search-forward "mul(\\([[:digit:]]+\\),\\([[:digit:]]+\\))" nil :noerror)
      (let ((a (string-to-number (match-string 1)))
            (b (string-to-number (match-string 2))))
        (message "%s * %s = %s" a b (* a b))
        (cl-incf sum (* a b))))
    (message "sum: %s" sum)))
