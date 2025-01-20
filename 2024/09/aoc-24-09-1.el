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
    (insert-file-contents file)
    (string-trim (buffer-string))))

(defun dump-fs (fs)
  (mapcar (lambda (x)
            (if (eq x -1)
                (princ ".")
              (princ x))) fs)
  (princ "\n"))

(defun extract-fs (input)
  (let ((fs (make-vector 0 nil))
        (file-id 0))
    (dotimes (i (length input))
      (let ((x (string-to-number (char-to-string (aref input i)))))
        (if (cl-evenp i)
            (progn
              (setq fs (vconcat fs (make-vector x file-id)))
              (cl-incf file-id))
          (setq fs (vconcat fs (make-vector x -1))))
        )
      )
    ;; (dump-fs fs)
    fs))

(defun free-space-idx (fs start-idx stop-idx)
  "Search free space in left direction, starting from START-IDX.
Return STOP-IDX if no space could be found."
  (let ((i start-idx))
    (while (and (not (eq (aref fs i) -1))
                (<= i stop-idx))
      (cl-incf i))
    i))

(defun data-idx (fs start-idx stop-idx)
  "Search data from in right direction, starting from START-IDX.
Return STOP-IDX if data could be found."
  (let ((i start-idx))
    (while (and (eq (aref fs i) -1)
                (>= i stop-idx))
      (cl-decf i))
    i))

(defun defragment-fs (fs)
  (let ((left 0)
        (right (- (length fs) 1))
        (continue t))
    (while continue
      (setq left (free-space-idx fs left right))
      (if (eq left right)
          (setq continue nil)
        (setq right (data-idx fs right left))
        (if (eq left right)
            (setq continue nil)
          (aset fs left (aref fs right))
          (aset fs right -1)))
      ;; (dump-fs fs)
      )
    fs))

(defun compute-fs-checksum (fs)
  (let ((sum 0)
        (i 0))
    (while (< i (length fs))
      (when (not (eq (aref fs i) -1))
        (cl-incf sum (* i (aref fs i))))
      (cl-incf i))
    sum))

(defun solve (input)
  (message "input: %s" input)
  (compute-fs-checksum (defragment-fs (extract-fs input))))

(when noninteractive
  (message "fs checksum: %s" (solve (read-input input-file))))
