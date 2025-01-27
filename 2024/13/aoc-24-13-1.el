;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;; If we're running in batch-mode, read command-line arguments.
(when noninteractive
  (setq input-file (elt command-line-args-left 0))
  (unless input-file
    (error "No input file given"))
  (message "input-file: %s" input-file))

(defun read-input (file)
  "Reads the input and returns a list of 2-dimensional linear equation systems.

For the input
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

We return the system of linear equations:
(I)  94A + 22B = 8400
(II) 34A + 67B = 5400

And represent it as:
'(94 22 8400 34 67 5400)
"
  (let ((result))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^Button A: X\\+\\([[:digit:]]+\\), Y\\+\\([[:digit:]]+\\)" nil :noerror)
        (let ((ax (string-to-number (match-string 1)))
              (ay (string-to-number (match-string 2))))
          (message "Button A %s %s" ax ay)
          (when (re-search-forward "^Button B: X\\+\\([[:digit:]]+\\), Y\\+\\([[:digit:]]+\\)" nil :noerror)
            (let ((bx (string-to-number (match-string 1)))
                  (by (string-to-number (match-string 2))))
              (message "Button B %s %s" bx by)
              (when (re-search-forward "^Prize: X=\\([[:digit:]]+\\), Y=\\([[:digit:]]+\\)" nil :noerror)
                (let ((px (string-to-number (match-string 1)))
                      (py (string-to-number (match-string 2))))
                  (message "Prize %s %s" px py)
                  (push (list ax bx px ay by py) result))))))))
    result))

(defun solve-2x2-system (m)
  "Solve the linear 2-dimensional equation system M.

We use Cramer's rule. M is given as extended coefficient matrix
in list form."
  (cl-destructuring-bind (ax bx px ay by py) m
    (message "solving %s %s %s %s %s %s" ax bx px ay by py)
    (let ((det (- (* ax by) (* ay bx))))
      (if (eql det 0)
          (message "no solution or infinitely many solutions")
        (let ((det1 (- (* px by) (* bx py)))
              (det2 (- (* ax py) (* px ay))))
          (let ((k (/ det1 det))
                (l (/ det2 det)))
            (if (and (eql px (+ (* k ax) (* l bx)))
                     (eql py (+ (* k ay) (* l by))))
                (list k l))))))))

(defun useful-solution-p (solution)
  (and (not (null solution))
       (seq-every-p (lambda (k) (>= k 0)) solution)
       (seq-every-p (lambda (k) (<= k 100)) solution)))

(defun solve (equations)
  (message "input is %s" equations)
  (let ((solutions (mapcar #'solve-2x2-system equations)))
    (message "solutions: %s" solutions)
    (let ((useful-solutions (seq-filter #'useful-solution-p solutions)))
      (message "useful solutions: %s" useful-solutions)
      (let ((tokens (mapcar (lambda (solution) (+ (* 3 (car solution)) (cadr solution))) useful-solutions)))
        (message "tokens per prize: %s" tokens)
        (seq-reduce #'+ tokens 0)))))

(when noninteractive
  (message "number of tokens: %s" (solve (read-input input-file))))
