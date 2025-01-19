;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;; If we're running in batch-mode, read command-line arguments.
(when noninteractive
  (setq input-file (elt command-line-args-left 0))
  (unless input-file
    (error "No input file given"))
  (message "input-file: %s" input-file))

(cl-defstruct (grid
               (:constructor grid--create))
  (width 0 :type integer :read-only t)
  (height 0 :type integer :read-only t)
  (data nil :type vector))

(defun grid-create (width height &optional initial-value)
  "Create a WIDTH x HEIGHT grid with INITIAL-VALUE (default space)."
  (let ((value (or initial-value ?\s)))
    (grid--create
     :width width
     :height height
     :data (make-vector (* width height) value))))

(defun grid-index (grid row col)
  "Convert ROW and COL to vector index for GRID."
  (let ((width (grid-width grid)))
    (+ (* row width) col)))

(defun grid-get (grid row col)
  "Get value at ROW,COL in GRID."
  (aref (grid-data grid)
        (grid-index grid row col)))

(defun grid-set (grid row col value)
  "Set VALUE at ROW,COL in GRID. VALUE is a character."
  (aset (grid-data grid)
        (grid-index grid row col)
        value))

(defun grid-dump (grid)
  "Print GRID contents in a human-readable format."
  (let* ((width (grid-width grid))
         (height (grid-height grid))
         (output ""))
    (dotimes (row height)
      (setq output (concat output
                          (if (> row 0) "\n" "")
                          (mapconcat
                           (lambda (col)
                             (char-to-string
                              (grid-get grid row col)))
                           (number-sequence 0 (1- width))))))
    (message "%s" output)))

(defun read-input (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((lines (string-split (string-trim (buffer-string)) "\n"))
           (height (seq-length lines))
           (width (seq-length (car lines)))
           (grid (grid-create width height)))
      (let ((row 0))
        (dolist (line lines)
          (dotimes (col width)
            (grid-set grid row col (aref line col)))
          (cl-incf row)))
      grid)))

(cl-defstruct (plot-region)
  (char ?\s :type char)
  (positions nil :type list)
  (perimeter 0 :type integer))

(defun plot-region-area (region)
  (length (plot-region-positions region)))

(defun region-calculate-price (region)
  (* (plot-region-area region) (plot-region-perimeter region)))

(defun count-outer-edges (grid row col c)
  "Return the number of elements around (ROW,COL) in GRID that
are not equal to C."
  (let ((result 4)
        (max-row (1- (grid-height grid)))
        (max-col (1- (grid-width grid))))
    (when (and (> row 0)
               (eql c (grid-get grid (1- row) col)))
      (cl-decf result))
    (when (and (> col 0)
               (eql c (grid-get grid row (1- col))))
      (cl-decf result))
    (when (and (< row max-row)
               (eql c (grid-get grid (1+ row) col)))
      (cl-decf result))
    (when (and (< col max-col)
               (eql c (grid-get grid row (1+ col))))
      (cl-decf result))
    result))

(defun plot-region-calculate-perimeter (region grid)
  (dolist (pos (plot-region-positions region))
    (let ((row (car pos))
          (col (cdr pos)))
      (cl-incf (plot-region-perimeter region)
               (count-outer-edges grid row col (plot-region-char region))))))

(defun find-plot-regions (grid)
  (let ((height (grid-height grid))
        (width (grid-width grid))
        (seen-positions (make-hash-table :test #'equal))
        (regions))
    (dotimes (row height)
      (dotimes (col width)
        (unless (gethash (cons row col) seen-positions)
          (let ((new-region (make-plot-region)))
            (setf (plot-region-char new-region) (grid-get grid row col))
            (find-plot-region-at grid row col seen-positions
                                 new-region)
            (plot-region-calculate-perimeter new-region grid)
            (push new-region regions)
            (message "region for %s plants: %s" (char-to-string (grid-get grid row col)) new-region)))))
    regions))

(defun find-plot-region-at (grid row col seen-positions region)
  (unless (or (gethash (cons row col) seen-positions)
              (not (eql (plot-region-char region) (grid-get grid row col))))
    (puthash (cons row col) t seen-positions)
    (push (cons row col) (plot-region-positions region))
    (let ((max-row (1- (grid-height grid)))
          (max-col (1- (grid-width grid))))
      (when (> row 0)
        (find-plot-region-at grid (1- row) col seen-positions region))
      (when (> col 0)
        (find-plot-region-at grid row (1- col) seen-positions region))
      (when (< row max-row)
        (find-plot-region-at grid (1+ row) col seen-positions region))
      (when (< col max-col)
        (find-plot-region-at grid row (1+ col) seen-positions region)))))

(defun solve (grid)
  (grid-dump grid)
  (let* ((regions (find-plot-regions grid))
         (prices (mapcar #'region-calculate-price regions)))
    (seq-reduce #'+ prices 0)))

(when noninteractive
  (message "benchmark %s"
           (benchmark-run 1
             (message "solution: %s" (solve (read-input input-file))))))
