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
  "Read FILE and return an initial-configuration object."
  (let ((room nil)
        (instructions nil))
    (with-temp-buffer
      (insert-file-contents file)
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
     ((eq elem-next-to-robot ?O)
      ;;
      (let ((dot-pos (find-empty-space-behind-boxes room moved-pos direction)))
        (when dot-pos
          (grid-set grid robot-pos ?.)
          (grid-set grid moved-pos ?@)
          (grid-set grid dot-pos ?O))))))
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
        (when (eq (grid-get (room-grid room) (position-create x y)) ?O)
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
    (dump-room room)
    (seq-do (lambda (direction)
              (simulate-step room direction))
            (initial-configuration-instructions initial-configuration))
    (seq-reduce #'+ (box-gps-coordinates room) 0)))

(when noninteractive
  (message "solution: %s" (solve (read-input input-file))))
