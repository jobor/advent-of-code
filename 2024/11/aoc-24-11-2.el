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

(defun apply-rules (stone)
  "Apply the three rules above on one STONE.
The result is a list of new stones."
  (or (rule1 stone)
      (rule2 stone)
      (rule3 stone)))

(defun solve-one-stone (stone n &optional h)
  "Return the number of stones for one initial STONE after blinking
N times.

Memoize the result in the hash table H."
  (unless h
    (setq h (make-hash-table :test 'equal)))
  (with-memoization (gethash (cons stone n) h)
    (let ((new-stones (apply-rules stone))
          (new-n (1- n)))
      (if (eql new-n 0)
          (length new-stones)
        (if (eql 2 (length new-stones))
            (+ (solve-one-stone (car new-stones) new-n h)
               (solve-one-stone (cadr new-stones) new-n h))
          (solve-one-stone (car new-stones) new-n h))))))

(defun solve (stones n)
  "Return the number of stones after N blinks."
  (message "initial stones: %s" stones)
  (let ((result 0))
    (dolist (stone stones)
      (message "solving for stone %s" stone)
      (cl-incf result (solve-one-stone stone n)))
    result))

(when noninteractive
  (let ((blinks 75))
    (message "benchmark %s"
             (benchmark-run 1
               (message "number of stones after %s blinks is %s"
                        blinks
                        (solve (read-input input-file) blinks))))))
