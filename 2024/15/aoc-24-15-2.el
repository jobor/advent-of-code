;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;; If we're running in batch-mode, read command-line arguments.
(when noninteractive
  (setq input-file (elt command-line-args-left 0))
  (unless input-file
    (error "No input file given"))
  (message "input-file: %s" input-file))


(cl-defstruct (room
               (:copier nil))
  size-x
  size-y
  grid)

(cl-defstruct initial-configuration
  "Represents the initial configuration read from the input file."
  room
  instructions)

(defun room-create (size-x size-y)
  "Create a new room object of the given size.
The room is filled with dots (to denote empty space)."
  (let ((grid (make-vector size-y nil)))
    (dotimes (y size-y)
      (aset grid y (make-vector size-x ?.)))
    (make-room :size-x size-x :size-y size-y :grid grid)))

(cl-defstruct position
  "Represents a position on the grid."
  x
  y)

(defun position-create (x y)
  "Construct a new position object."
  (make-position :x x :y y))

(defun grid-get (grid pos)
  "Get the value in GRID at the given POS."
  (aref (aref grid (position-y pos)) (position-x pos)))

(defun grid-set (grid pos newelt)
  "Set the value at POS in GRID to NEWELT."
  (aset (aref grid (position-y pos)) (position-x pos) newelt))

(defun dump-room (room)
  "Dump the room to the output."
  (let ((s ""))
    (seq-do
     (lambda (line)
       (seq-do
        (lambda (item)
          (setq s (concat s (char-to-string item))))
        line)
       (setq s (concat s "\n")))
     (room-grid room))
    (message "%s" s)))

(cl-defun read-input (file)
  "Read FILE and return an initial-configuration object.
Enlarge the grid as described in part II."
  (let ((room nil)
        (instructions nil))
    (with-temp-buffer
      (insert-file-contents file)

      ;; Do the replacements as described. Very inefficient, but whatever.
      (goto-char (point-min))
      (while (search-forward "#" nil t)
        (replace-match "##" nil t))
      (goto-char (point-min))
      (while (search-forward "O" nil t)
        (replace-match "[]" nil t))
      (goto-char (point-min))
      (while (search-forward "." nil t)
        (replace-match ".." nil t))
      (goto-char (point-min))
      (while (search-forward "@" nil t)
        (replace-match "@." nil t))

      (goto-char (point-min))
      (unless (re-search-forward "^#+$" nil :noerror 2)
        (error "Could not determine room boundaries."))
      (setq room (room-create (- (match-end 0) (match-beginning 0))
                              (line-number-at-pos (point))))

      ;; Fill the grid.
      (goto-char (point-min))
      (re-search-forward "^#+$" nil :noerror)
      (forward-line 0)
      (dotimes (y (room-size-y room))
        (dotimes (x (room-size-x room))
          (grid-set (room-grid room) (position-create x y) (char-after))
          (forward-char))
        (forward-line 1))

      ;; Fill the instruction list.
      (re-search-forward "[<>^v]+" nil :noerror)
      (forward-line 0)
      (setq instructions
            (replace-regexp-in-string "\n" ""
                                      (buffer-substring-no-properties (point) (point-max))))
      (make-initial-configuration :room room :instructions instructions))))

(cl-defun find-robot (room)
  "Return the coordinates of the robot in the ROOM."
  (dotimes (y (room-size-y room))
    (dotimes (x (room-size-x room))
      (when (eq (grid-get (room-grid room) (position-create x y)) ?@)
        (cl-return-from find-robot (position-create x y))))))

(defun room-move-pos (room pos direction)
  "Return the moved the position into DIRECTION in ROOM.
If the moved position is out of bounds, return nil.  We shouldn't hit
this case in our simulation since the whole room is walled off."
  (let ((x (position-x pos))
        (y (position-y pos)))
    (cond
     ((eq direction ?<)
      (cl-decf x))
     ((eq direction ?>)
      (cl-incf x))
     ((eq direction ?^)
      (cl-decf y))
     ((eq direction ?v)
      (cl-incf y)))
    (when (and (>= x 0)
               (>= y 0)
               (< x (room-size-x room))
               (< y (room-size-y room)))
      (position-create x y))))

(defun find-empty-space-behind-boxes (room start-pos direction)
  "Search for a dot in DIRECTION, starting from START-POS.
Return a position if found, nil otherwise.  If some other element than
?O or ?. is found, return nil."
  (when start-pos
    (let ((elem (grid-get (room-grid room) start-pos)))
      (cond
       ((eq elem ?.)
        start-pos)
       ((eq elem ?O)
        (find-empty-space-behind-boxes room (room-move-pos room start-pos direction) direction))))))

(defun box-part-p (char)
  "Return non-nil if CHAR is part of a box."
  (or (eq char ?\[) (eq char ?\])))

(defun empty-space-p (char)
  "Return non-nil if CHAR is empty."
  (or (eq char ?.)))

(defun empty-or-box-part-p (char)
  "Return non-nil if CHAR is empty or part of a box."
  (or (empty-space-p char) (box-part-p char)))

(cl-defun find-movable-boxes (room pos direction)
  "Return a list of positions.
Starting with the box at POS in ROOM, return all boxes that can be
pushed into DIRECTION."
  (let* ((grid (room-grid room))
         (elem-at-pos (grid-get grid pos))
         (pos-left (copy-position pos))
         (pos-right (copy-position pos)))

    ;; Calculate left and right positions of the box.
    (if (eq elem-at-pos ?\[)
        (cl-incf (position-x pos-right))
      (cl-decf (position-x pos-left)))

    (let ((new-pos-left (room-move-pos room pos-left direction))
          (new-pos-right (room-move-pos room pos-right direction)))
      (cond
       ;; moving to the left
       ((eq direction ?<)
        (let ((elem-left-to-box (grid-get grid new-pos-left)))
          (cond
           ((eq elem-left-to-box ?.)
            ;; There's space left to the box.  Result is this box.
            (list pos))
           ((eq elem-left-to-box ?\])
            ;; There's a box left to this box.
            ;; If that box is movable, return it plus this box.
            (when-let ((movable-boxes (find-movable-boxes room new-pos-left direction)))
              (cons pos movable-boxes))))))
       ;; moving to the right
       ((eq direction ?>)
        (let ((elem-right-to-box (grid-get grid new-pos-right)))
          (cond
           ((eq elem-right-to-box ?.)
            ;; There's space right to the box.  Result is this box.
            (list pos))
           ((eq elem-right-to-box ?\[)
            ;; There's a box right to this box.
            ;; If that box is movable, return it plus this box.
            (when-let ((movable-boxes (find-movable-boxes room new-pos-right direction)))
              (cons pos movable-boxes))))))
       ;; moving up/down
       ((or (eq direction ?^) (eq direction ?v))
        (let* ((elem-new-pos-left (grid-get grid new-pos-left))
               (elem-new-pos-right (grid-get grid new-pos-right))
               (elems-new (list elem-new-pos-left elem-new-pos-right)))
          ;; The elements at the new positions must be empty or movable boxes.
          (when (seq-every-p #'empty-or-box-part-p elems-new)
            (if (seq-every-p #'empty-space-p elems-new)
                ;; There's space above/below the box.  Result is this box.
                (list pos)
              ;; Some elements above/below are boxes.
              (let ((boxes-at-destination))

                (if (and (eq elem-new-pos-left ?\[)
                          (eq elem-new-pos-right ?\]))
                    ;; We have [] at destination.  That's only one box.
                    (setq boxes-at-destination (list new-pos-left))
                  ;; We have one of these cases at destination: ][ .[ ].
                  ;; Filter out the non-box positions.
                  (setq boxes-at-destination (seq-filter (lambda (pos)
                                                           (box-part-p (grid-get grid pos)))
                                                         (list new-pos-left new-pos-right))))

                (let ((movable-boxes-lists
                       (seq-map (lambda (adjacent-box-pos)
                                  (find-movable-boxes room adjacent-box-pos direction))
                                boxes-at-destination)))
                  ;; All elements in movable-boxes-lists are non-nil?
                  ;; Then the result is current position plus the found movable boxes.
                  (when (seq-every-p #'identity movable-boxes-lists)
                    ;; Use a hash table to deduplicate while combining results
                    (let ((seen (make-hash-table :test 'equal))
                          (result))
                      ;; Add current position (normalized)
                      (let* ((norm-pos (normalize-box-position grid pos))
                             (key (cons (position-x norm-pos) (position-y norm-pos))))
                        (puthash key norm-pos seen))
                      ;; Add all boxes from sublists
                      (dolist (lst movable-boxes-lists)
                        (dolist (box-pos lst)
                          (let* ((norm-pos (normalize-box-position grid box-pos))
                                 (key (cons (position-x norm-pos) (position-y norm-pos))))
                            (puthash key norm-pos seen))))
                      ;; Convert hash table to list
                      (maphash (lambda (_k v) (push v result)) seen)
                      result))))))))))))

(defun normalize-box-position (grid pos)
  "Normalize POS to point to the left side of a box.
If POS points to the right side ']' of a box, return the left side position.
Otherwise return POS unchanged."
  (let ((elem (grid-get grid pos)))
    (if (eq elem ?\])
        (position-create (1- (position-x pos)) (position-y pos))
      pos)))

(defun push-boxes (room boxes direction)
  "Push all BOXES into DIRECTION.
No check is done whether the push is valid.  That's up to the caller."
  (let* ((grid (room-grid room))
         (new-left-positions)
         (new-right-positions)
         ;; Normalize all positions to point to the left side of boxes
         (normalized-boxes (mapcar (lambda (pos) (normalize-box-position grid pos)) boxes))
         ;; Deduplicate the normalized positions
         (unique-boxes (seq-uniq normalized-boxes
                                 (lambda (a b)
                                   (and (= (position-x a) (position-x b))
                                        (= (position-y a) (position-y b)))))))
    (dolist (pos unique-boxes)
      (let ((elem-at-pos (grid-get grid pos))
            (left-pos (copy-position pos))
            (right-pos (copy-position pos)))

        ;; Calculate left and right positions of the box.
        (if (eq elem-at-pos ?\[)
            (cl-incf (position-x right-pos))
          (cl-decf (position-x left-pos)))

        ;; Clear the old positions.
        (grid-set grid left-pos ?.)
        (grid-set grid right-pos ?.)

        ;; Store new positions.
        (push (room-move-pos room left-pos direction) new-left-positions)
        (push (room-move-pos room right-pos direction) new-right-positions)))

    ;; "Draw" boxes at new positions.
    (dolist (pos new-left-positions)
      (grid-set grid pos ?\[))
    (dolist (pos new-right-positions)
      (grid-set grid pos ?\]))))

(defun simulate-step (room direction)
  "Execute one simulation step.
The robot in ROOM is moved into DIRECTION.
ROOM is directly modified."
  (let* ((robot-pos (find-robot room))
         (moved-pos (room-move-pos room robot-pos direction))
         (grid (room-grid room))
         (elem-next-to-robot (grid-get grid moved-pos)))
    (cond
     ((eq elem-next-to-robot ?#)
      ;; Robot runs into the wall and stops.
      )
     ((eq elem-next-to-robot ?.)
      ;; Swap dot and robot.
      (grid-set grid robot-pos ?.)
      (grid-set grid moved-pos ?@))
     ((box-part-p elem-next-to-robot)
      (when-let ((movable-boxes (find-movable-boxes room moved-pos direction)))
        (push-boxes room movable-boxes direction)
        (grid-set grid robot-pos ?.)
        (grid-set grid moved-pos ?@)))))
  room)

(defun box-gps-coordinate (box)
  "Return the GPS coordinate of a BOX at X,Y.
The box is given as a position object."
  (let ((x (position-x box))
        (y (position-y box)))
    (+ x (* 100 y))))

(defun room-boxes (room)
  "Return the list of all boxes in a room.
Returns a list of position objects."
  (let ((result))
    (dotimes (y (room-size-y room))
      (dotimes (x (room-size-x room))
        (when (eq (grid-get (room-grid room) (position-create x y)) ?[)
          (push (position-create x y) result))))
    result))

(defun box-gps-coordinates (room)
  "Return a list of the GPS coordinates of all boxes in the room."
  (mapcar #'box-gps-coordinate (room-boxes room)))

(defun solve (initial-configuration)
  "Simulate the movements of the robot in the room.
This is done according to the instructions in INITIAL-CONFIGURATION.
Afterwards, sum up all GPS coordinates of all boxes."
  (let ((room (initial-configuration-room initial-configuration)))
    (seq-do (lambda (direction)
              (simulate-step room direction))
            (initial-configuration-instructions initial-configuration))
    (seq-reduce #'+ (box-gps-coordinates room) 0)))

(when noninteractive
  (message "solution: %s" (solve (read-input input-file))))
