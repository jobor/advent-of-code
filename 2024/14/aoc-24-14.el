;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;; If we're running in batch-mode, read command-line arguments.
(when noninteractive
  (setq input-file (elt command-line-args-left 0))
  (unless input-file
    (error "No input file given"))
  (message "input-file: %s" input-file))

(cl-defstruct dimension
  "Dimension of a room."
  x y)

(cl-defstruct position
  "Position of a robot.
Top-left is (0,0).
Values are never negative."
  x
  y)

(cl-defstruct velocity
  "Velocity vector of a robot.
Values can be negative."
  x
  y)

(cl-defstruct robot
  position
  velocity)

(cl-defstruct room
  dimension
  robots)

(defun read-input (file-path dimension)
  "Read FILE-PATH and return a room object."
  (let ((result (make-room :dimension dimension)))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      (while (re-search-forward "^p=\\([[:digit:]]+\\),\\([[:digit:]]+\\) v=\\([-[:digit:]]+\\),\\([-[:digit:]]+\\)" nil :noerror)
        (let ((robot (make-robot :position (make-position :x (string-to-number (match-string 1))
                                                          :y (string-to-number (match-string 2)))
                                 :velocity (make-velocity :x (string-to-number (match-string 3))
                                                          :y (string-to-number (match-string 4))))))
          (push robot (room-robots result)))))
    result))

(defun solve (room)
  "Return the safety factor of the room."
  (message "initial configuration:")
  (dump-room room)
  (dotimes (i 100)    ;; increase this to 10000 for part II
    (simulate-step room)
    (message "%s s" (number-to-string (1+ i)))
    (dump-room room (lambda ()
                      (goto-char (point-min))
                      (search-forward "XXXXXXXXXX" nil :noerror)))
    )
  (calculate-safety-factor room))

(defvar dump-room-show-counts nil
  "Whether `dump-room' should display the number of robots.
This is only useful for numbers < 10.")

(defun dump-room (room &rest predicate)
  "Dump the room/robot setup for debugging purposes.
If PREDICATE is given, only dump the room if PREDICATE returns true."
  (with-temp-buffer
    (goto-char (point-min))
    (dotimes (line (dimension-y (room-dimension room)))
      (dotimes (column (dimension-x (room-dimension room)))
        (insert "."))
      (insert "\n"))
    (dolist (robot (room-robots room))
      (goto-char (point-min))
      (forward-line (position-y (robot-position robot)))
      (beginning-of-line)
      (forward-char (position-x (robot-position robot)))
      (let ((c (char-after)))
        (delete-char 1)
        (if dump-room-show-counts
            (if (cl-digit-char-p c)
                (insert (number-to-string (1+ (string-to-number (char-to-string c)))))
              (insert "1"))
          (insert "X"))))
    (when (or (null predicate)
              (apply predicate))
      (message "%s" (buffer-string)))))

(defun simulate-step (room)
  "Simulate one second of robot movements in the ROOM.
Loop through all robots.
Update the robot's position."
  (dolist (robot (room-robots room))
    (setf (position-x (robot-position robot))
          (mod (+ (position-x (robot-position robot)) (velocity-x (robot-velocity robot)))
               (dimension-x (room-dimension room))))
    (setf (position-y (robot-position robot))
          (mod (+ (position-y (robot-position robot)) (velocity-y (robot-velocity robot)))
               (dimension-y (room-dimension room))))))

(defun calculate-safety-factor (room)
  "Return the safety factor of the given ROOM.
Loop through all the robots in the room.
Check if a robot is in one of the quadrants.
If yes, increment the quadrant's robot counter."
  (let ((upper-left-robots 0)
        (upper-right-robots 0)
        (lower-left-robots 0)
        (lower-right-robots 0)
        (mid-x (/ (dimension-x (room-dimension room)) 2))
        (mid-y (/ (dimension-y (room-dimension room)) 2)))
    (dolist (robot (room-robots room))
      (let ((x (position-x (robot-position robot)))
            (y (position-y (robot-position robot))))
        (cond
         ((and (< x mid-x) (< y mid-y))
          (cl-incf upper-left-robots))
         ((and (< x mid-x) (> y mid-y))
          (cl-incf lower-left-robots))
         ((and (> x mid-x) (< y mid-y))
          (cl-incf upper-right-robots))
         ((and (> x mid-x) (> y mid-y))
          (cl-incf lower-right-robots)))))
    (message "number of robots in upper-left quadrant: %s" upper-left-robots)
    (message "number of robots in upper-right quadrant: %s" upper-right-robots)
    (message "number of robots in lower-left quadrant: %s" lower-left-robots)
    (message "number of robots in lower-right quadrant: %s" lower-right-robots)
    (* upper-left-robots upper-right-robots lower-left-robots lower-right-robots)))

(when noninteractive
  (message "solution: %s" (solve (read-input input-file
                                             (if (string-match-p "input-0\\.txt$" input-file)
                                                 (make-dimension :x 11 :y 7)
                                               (make-dimension :x 101 :y 103))))))
