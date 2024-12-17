;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;; If we're running in batch-mode, read command-line arguments.
(when noninteractive
  (setq input-file (elt command-line-args-left 0))
  (unless input-file
    (error "No input file given"))
  (message "input-file: %s" input-file))

(defun read-input (filename)
  "Read antenna layout from FILENAME and return dimensions and antenna positions.
Returns (WIDTH HEIGHT ANTENNAS) where ANTENNAS is a hash-table
that maps char -> list of positions. Each position is a vector of length two."
  (with-temp-buffer
    (insert-file-contents filename)
    (let* ((lines (split-string (buffer-string) "\n" t))
           (height (length lines))
           (width (length (car lines)))
           (antennas (make-hash-table)))

      (let ((y (1- height)))
        (dolist (line lines)
          (let ((x 0))
            (dolist (char (append line nil))
              (unless (char-equal char ?.)
                (let ((pos (vector x y))
                      (existing (gethash char antennas)))
                  (puthash char
                          (if existing
                              (cons pos existing)
                            (list pos))
                          antennas)))
              (cl-incf x))
            (cl-decf y))))
      (list width height antennas))))

(defun calculate-antinodes (p q)
  "Returns the two antinodes of the two antennas at the points P and Q.

The antinode near p is calculated by adding the vector q->p to p.
The antinode near q is calculated by adding the vector p->q to q."
  (let ((ap (vector
             (- (* 2 (aref p 0)) (aref q 0))
             (- (* 2 (aref p 1)) (aref q 1))))
        (aq (vector
             (- (* 2 (aref q 0)) (aref p 0))
             (- (* 2 (aref q 1)) (aref p 1)))))
    (list ap aq)))

(defun pos-in-bounds-p (width height pos)
  (let ((x (aref pos 0))
        (y (aref pos 1)))
    (and (>= x 0)
         (>= y 0)
         (< x width)
         (< y height))))

(defun unique-antinode-positions (width height antennas)
  (let ((antinode-positions)
        (i 0)
        (k 0))
    (maphash (lambda (key positions)
               (message "calculating antinodes for antenna label %s" (char-to-string key))

               (let ((i 0)
                     (nr-of-positions (length positions)))
                 (while (< i nr-of-positions)
                   (let ((k (1+ i)))
                     (while (< k nr-of-positions)
                       (let* ((p (nth i positions))
                              (q (nth k positions)))
                         (message "calculating antinodes for antenna pair %s / %s" p q)
                         (setq antinode-positions (append antinode-positions (calculate-antinodes p q))))
                       (cl-incf k))
                     )
                   (cl-incf i))
                 )
               )
             antennas)
    (delete-dups (seq-filter (lambda (pos)
                               (pos-in-bounds-p width height pos))
                             antinode-positions))))

(when noninteractive
  (cl-multiple-value-bind (width height antennas) (read-input input-file)
    (message "area dimensions: %sx%s" width height)
    (maphash (lambda (key value)
               (message "antennas with label %s: %s" (char-to-string key) value))
             antennas)
    (message "number of unique antinode positions %s" (length (unique-antinode-positions width height antennas)))))
