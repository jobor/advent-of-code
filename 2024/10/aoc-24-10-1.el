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
  "Create a WIDTH x HEIGHT grid with INITIAL-VALUE (defaults to 0)."
  (let ((value (or initial-value 0)))
    (when (or (not (numberp value))
              (< value 0)
              (> value 9))
      (error "Initial value must be between 0 and 9"))
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
  "Set VALUE at ROW,COL in GRID. VALUE must be 0-9."
  (unless (and (numberp value)
               (>= value 0)
               (<= value 9))
    (error "Value must be between 0 and 9"))
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
                             (number-to-string
                              (grid-get grid row col)))
                           (number-sequence 0 (1- width))))))
    (message "%s" output)))

(cl-defstruct trailhead start ends)

(defun trailhead-score (trailhead)
  (length (trailhead-ends trailhead)))

(defun find-trailheads-at-pos (grid start-pos-hash row column old-value &optional trail)
  "Find trailheads at the given position.
The trailheads are stored in START-POS-HASH.
TRAIL is a list of positions and denotes the already walked
trail."
  ;; (message "find-trailheads-at-pos %s %s %s" row column old-value)
  (when (and (>= row 0)
             (>= column 0)
             (< row (grid-height grid))
             (< column (grid-width grid))
             (eql (grid-get grid row column) (1+ old-value))
             (not (member (list row column) trail)))
    ;; (message "conditions OK at %s %s" row column)
    (let ((pos (list row column))
          (current-value (grid-get grid row column)))
      ;; (message "current-value %s" current-value)

      (if (eql 9 current-value)
          ;; Reached a summit, return a list that contains the current trail.
          (let* ((start-pos (car (last trail)))
                 (trailhead (gethash start-pos start-pos-hash)))
            ;; (message "found trail from %s to %s" start-pos pos)
            (if trailhead
                (when (not (member pos (trailhead-ends trailhead)))
                  (push pos (trailhead-ends trailhead)))
              (setq trailhead (make-trailhead :start start-pos
                                              :ends (list pos)))
              (puthash start-pos trailhead start-pos-hash)))

        ;; Not yet at a summit.
        ;; Try to walk north.
        ;; (message "walk north")
        (find-trailheads-at-pos grid start-pos-hash (- row 1) column current-value
                                              (cons pos trail))
        ;; walk south
        ;; (message "walk south")
        (find-trailheads-at-pos grid start-pos-hash (+ row 1) column current-value
                                (cons pos trail))

        ;; walk west
        ;; (message "walk west")
        (find-trailheads-at-pos grid start-pos-hash row (- column 1) current-value
                                (cons pos trail))

        ;; walk east
        ;; (message "walk east")
        (find-trailheads-at-pos grid start-pos-hash row (+ column 1) current-value
                                (cons pos trail))))))

(defun trailheads (grid)
  (let ((trailheads)
        (start-pos-hash (make-hash-table :test #'equal)))
    (dotimes (row (grid-height grid))
      ;; (message "row %s" row)
      (dotimes (column (grid-width grid))
        (when (eql 0 (grid-get grid row column))
          (message "starting search at %s %s" row column)
          (find-trailheads-at-pos grid start-pos-hash row column -1))))
    ;; return trailheads
    (maphash (lambda (k v)
               (message "k %s v %s" k v)
               (push v trailheads))
             start-pos-hash)
    trailheads))

(defun read-input (file)
  (with-temp-buffer
    (insert-file-contents file)

    (let ((width 0)
          (height 0)
          (lines)
          (grid))

      (goto-char (point-min))
      (while (re-search-forward "\\b[[:digit:]]+\\b" nil :noerror)
        (push (match-string 0) lines)
        (setq width (max width (length (car lines)))))

      (setq height (length lines))
      (message "width: %s, height: %s" width height)
      (setq grid (grid-create width height))

      (setq row-nr 0)
      (dolist (line (reverse lines))
        (dotimes (i (length line))
          (grid-set grid row-nr i (- (aref line i) ?0)))
        (cl-incf row-nr))

      ;; Return the grid.
      grid)))

(defun solve (grid)
  (seq-reduce #'+ (mapcar #'trailhead-score (trailheads grid)) 0))

(when noninteractive
  (message "solution: %s" (solve (read-input input-file))))
