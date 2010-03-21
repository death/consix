;;;; +----------------------------------------------------------------+
;;;; | CONSIX                                             DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:consix)


;;;; Grid

(defconstant cell-unclaimed #x00)
(defconstant cell-claimed   #x01)
(defconstant cell-claiming  #x02)
(defconstant cell-edge      #x03)

(defconstant grid-rows 90)
(defconstant grid-cols 90)

(defconstant cell-width 2)
(defconstant cell-height 2)

(defconstant grid-x-offset (floor (* grid-cols cell-width) -2))
(defconstant grid-y-offset (floor (* grid-rows cell-height) -2))

(deftype cell () '(unsigned-byte 8))

(defclass grid ()
  ((total-unclaimed :accessor total-unclaimed)
   (current-unclaimed :accessor current-unclaimed)
   (cells :accessor cells)))

(defmethod initialize-instance :after ((grid grid) &rest initargs)
  (declare (ignore initargs))
  (setf (cells grid) (make-cells))
  (setf (total-unclaimed grid) (count cell-unclaimed (cells grid)))
  (setf (current-unclaimed grid) (total-unclaimed grid)))

(defun make-cells ()
  (let ((cells (make-array (* grid-rows grid-cols)
                           :element-type 'cell
                           :initial-element cell-unclaimed)))
    (macrolet ((ref (row col) `(aref cells (+ ,col (* ,row grid-cols)))))
      (dotimes (row grid-rows)
        (setf (ref row 0) cell-edge)
        (setf (ref row (1- grid-cols)) cell-edge))
      (dotimes (col grid-cols)
        (setf (ref 0 col) cell-edge)
        (setf (ref (1- grid-rows) col) cell-edge)))
    cells))

(defun cell-ref (row col grid)
  (aref (cells grid) (+ (* row grid-cols) col)))

(defun (setf cell-ref) (new-value row col grid)
  (setf (aref (cells grid) (+ (* row grid-cols) col)) new-value))

(defun cell-center-position (row col)
  (vec (+ grid-x-offset (* col cell-width) (floor cell-width 2))
       (+ grid-y-offset (* row cell-height) (floor cell-height 2))))

(defun cell-row/col (pos)
  (values (floor (- (y pos) grid-y-offset) cell-height)
          (floor (- (x pos) grid-x-offset) cell-width)))

(defun valid-cell-p (row col)
  (and (>= row 0) (< row grid-rows)
       (>= col 0) (< col grid-cols)))

(defmethod render ((grid grid))
  (loop for i from 0
        for cell across (cells grid)
        do (multiple-value-bind (row col)
               (floor i grid-rows)
             (multiple-value-bind (r g b a)
                 (case cell
                   (#.cell-unclaimed (values 0.0 0.0 0.0 0.0))
                   (#.cell-claimed   (values 0.0 1.0 0.0 0.5))
                   (#.cell-claiming  (values 1.0 0.0 0.0 0.5))
                   (#.cell-edge      (values 0.0 1.0 0.0 1.0)))
               (gl:color r g b a)
               (gl:with-primitive :quads
                 (let ((sx (+ grid-x-offset (* col cell-width) 0.1))
                       (sy (+ grid-y-offset (* row cell-height) 0.1))
                       (ex (+ grid-x-offset (* (1+ col) cell-width) -0.1))
                       (ey (+ grid-y-offset (* (1+ row) cell-height) -0.1)))
                   (gl:vertex sx sy)
                   (gl:vertex ex sy)
                   (gl:vertex ex ey)
                   (gl:vertex sx ey)))))))

(defun claim-cells (grid)
  (let ((unclaimed-neighbors '()))
    (dotimes (row grid-rows)
      (dotimes (col grid-cols)
        (symbol-macrolet ((cell (cell-ref row col grid)))
          (cond ((= cell cell-claiming)
                 (setf cell cell-edge)
                 (when (null unclaimed-neighbors)
                   (setf unclaimed-neighbors
                         (collect-neighbors-if (lambda (neighbor)
                                                 (= neighbor cell-unclaimed))
                                               row col grid))))))))
    (when unclaimed-neighbors
      (flood-fill grid (first (first unclaimed-neighbors)) (second (first unclaimed-neighbors))
                  cell-unclaimed cell-claimed))
    (setf (current-unclaimed grid) (count cell-unclaimed (cells grid)))))

(defun collect-neighbors-if (function row col grid)
  (loop with result = '()
        for neighbor-row from (1- row) to (1+ row)
        do (loop for neighbor-col from (1- col) to (1+ col)
                 when (and (valid-cell-p row col)
                           (or (/= neighbor-row row)
                               (/= neighbor-col col))
                           (funcall function (cell-ref neighbor-row neighbor-col grid)))
                 do (push (list neighbor-row neighbor-col) result))
        finally (return result)))

(defun flood-fill (grid starting-row starting-col source target)
  (let ((filled 0)
        (locations (list (list starting-row starting-col))))
    (flet ((interesting-p (row col)
             (and (valid-cell-p row col)
                  (= source (cell-ref row col grid)))))
      (do () ((null locations))
        (destructuring-bind (row col) (pop locations)
          (let ((value (cell-ref row col grid)))
            (when (= value source)
              (let ((left (do ((left col (1- left)))
                              ((not (interesting-p row left)) left)))
                    (right (do ((right col (1+ right)))
                               ((not (interesting-p row right)) right))))
                (loop for fill-col from (1+ left) below right do
                      (setf (cell-ref row fill-col grid) target)
                      (incf filled)
                      (when (interesting-p (1- row) fill-col)
                        (push (list (1- row) fill-col) locations))
                      (when (interesting-p (1+ row) fill-col)
                        (push (list (1+ row) fill-col) locations)))))))))
    filled))

(defun random-unclaimed-cell (grid)
  (assert (plusp (current-unclaimed grid)))
  (loop
   (let ((n (random (current-unclaimed grid)))
         (count 0))
     (dotimes (row grid-rows)
       (dotimes (col grid-cols)
         (when (= (cell-ref row col grid) cell-unclaimed)
           (when (= n count)
             (return-from random-unclaimed-cell
               (values row col)))
           (incf count)))))
   ;; We can get here when the player is in the middle of claiming;
   ;; try again.
   ))


;;;; Halo

(defclass halo ()
  ((value :initform 0.0 :accessor halo-value)
   (step :initform 0.05 :accessor halo-step)))

(defun halo-update (halo)
  (let ((new-value (+ (halo-value halo) (halo-step halo))))
    (cond ((or (< new-value 0.0) (> new-value 1.0))
           (setf (halo-step halo) (- (halo-step halo))))
          (t (setf (halo-value halo) new-value)))))


;;;; Player

(defconstant player-movement-steps 3)

(defclass player ()
  ((pos :accessor pos)
   (row :initarg :row :accessor row)
   (col :initarg :col :accessor col)
   (grid :initarg :grid :accessor grid)
   (halo :initform (make-instance 'halo) :accessor halo)
   (movement-actions :initform '() :accessor movement-actions)
   (claiming :initform nil :accessor claiming-p)))

(defmethod initialize-instance :after ((player player) &rest initargs)
  (declare (ignore initargs))
  (let ((row (row player))
        (col (col player)))
    (setf (pos player) (cell-center-position row col))))

(defmethod update ((player player))
  (when (null (movement-actions player))
    (maybe-queue-movement-actions player))
  (when-let (action (pop (movement-actions player)))
    (funcall action))
  (halo-update (halo player)))

(defmethod render ((player player))
  (gl:with-pushed-matrix
    (with-vec (x y (pos player))
      (gl:translate x y 0.0)
      (dotimes (i 5)
        (gl:color 0.0 0.0 1.0 (* i 0.2))
        (draw-circle (- 5 i) 30 t))
      (gl:color 0.0 0.0 1.0 (halo-value (halo player)))
      (draw-circle 5))))

(defun maybe-queue-movement-actions (player)
  (flet ((goto (row col)
           (let* ((target (cell-center-position row col))
                  (step (vec/ (vec- target (pos player))
                              (float player-movement-steps))))
             (setf (movement-actions player)
                   (loop for i from 1 to player-movement-steps
                         if (< i player-movement-steps)
                         collect (lambda () (vec+= (pos player) step))
                         else
                         collect (lambda ()
                                   (setf (pos player) target)
                                   (setf (row player) row)
                                   (setf (col player) col)
                                   (player-changed-cell player)))))))
    (when-let (move (find-if (lambda (move)
                               (destructuring-bind (row col) move
                                 (movement-possible-p row col player)))
                             (requested-moves (row player) (col player))))
      (apply #'goto move))))

(defun requested-moves (row col)
  (remove nil (mapcar (lambda (key)
                        (case key
                          (:key-left (list row (1- col)))
                          (:key-right (list row (1+ col)))
                          (:key-up (list (1+ row) col))
                          (:key-down (list (1- row) col))))
                      *keys*)))

(defun movement-possible-p (row col player)
  (and (valid-cell-p row col)
       (or (= cell-edge (cell-ref row col (grid player)))
           (and (member #\Space *keys*)
                (= cell-unclaimed (cell-ref row col (grid player)))))))

(defun player-changed-cell (player)
  (symbol-macrolet ((cell (cell-ref (row player) (col player) (grid player))))
    (case cell
      (#.cell-unclaimed
       (setf cell cell-claiming)
       (setf (claiming-p player) t))
      (#.cell-edge
       (when (claiming-p player)
         (claim-cells (grid player))
         (setf (claiming-p player) nil)))
      (t (warn "Player changed to a cell it shouldn't have changed to (~D)." cell)))))


;;;; Enemy

(defclass enemy ()
  ((pos :initarg :pos :accessor pos)
   (grid :initarg :grid :accessor grid)
   (targets :initform nil :accessor targets)
   (target-angle :initform nil :accessor target-angle)
   (structure :initarg :structure :accessor enemy-structure)))

(defmethod update ((enemy enemy))
  (let ((target (first (targets enemy)))
        (pos (pos enemy)))
    (cond ((or (null target) (vec=~ pos target 1.0))
           (loop until (or (null (targets enemy))
                           (not (vec=~ pos (first (targets enemy)) 1.0)))
                 do (pop (targets enemy)))
           (when (null (targets enemy))
             (setf (targets enemy) (pick-target-curve enemy)))
           (setf target (first (targets enemy)))
           (setf (target-angle enemy) (vec-angle (vec- pos target))))
          (t
           (vec+= pos (vel-vec 0.5 (vec- target pos)))
           (let ((last-angle (target-angle enemy)))
             (map-into (enemy-structure enemy)
                       (lambda (x)
                         (let ((inc (max -10.0 (min 10.0 (/ (- last-angle x) 10.0)))))
                           (setf last-angle (- x last-angle))
                           (+ inc x)))
                       (enemy-structure enemy)))))))

(defun pick-target-curve (enemy)
  (let ((pos (pos enemy))
        (others (loop repeat 3 collect
                      (multiple-value-call #'cell-center-position
                        (random-unclaimed-cell (grid enemy))))))
    (let ((xs (list* (x pos) (mapcar #'x others)))
          (ys (list* (y pos) (mapcar #'y others)))
          (curve '()))
      (gob::call-with-curve-multipliers
       (lambda (&rest ms)
         (push (vec (reduce #'+ (mapcar #'* xs ms))
                    (reduce #'+ (mapcar #'* ys ms)))
               curve)))
      (nreverse curve))))
    
(defmethod render ((enemy enemy))
  (gl:with-pushed-matrix
    (with-vec (x y (pos enemy))
      (gl:translate x y 0.0))
    (gl:color 1.0 1.0 0.0)
    (render-structure (enemy-structure enemy))))

(defun render-structure (object)
  (typecase object
    (atom)
    (cons
     (gl:rotate (car object) 0.0 0.0 1.0)
     (render-box-pair)
     (if (cdr object)
         (gl:with-pushed-matrix
           (gl:translate 6.0 0.0 0.0)
           (render-arrow)
           (gl:with-pushed-matrix
             (gl:translate 6.0 0.0 0.0)
             (render-structure (cdr object))))
         (gl:with-pushed-matrix
           (gl:translate 4.0 0.0 0.0)
           (render-box-filling))))))

(defun render-box-pair ()
  (gl:with-primitive :line-loop
    (gl:vertex 0 -2)
    (gl:vertex 8 -2)
    (gl:vertex 8 +2)
    (gl:vertex 0 +2))
  (gl:with-primitive :lines
    (gl:vertex 4 -2)
    (gl:vertex 4 +2)))

(defun render-arrow ()
  (gl:with-primitive :lines
    (gl:vertex 0 0)
    (gl:vertex 4 0))
  (gl:with-primitive :line-loop
    (gl:vertex 4 -1)
    (gl:vertex 6 0)
    (gl:vertex 4 +1)))

(defun render-box-filling ()
  (gl:with-primitive :lines
    (gl:vertex 4 +2)
    (gl:vertex 0 -2)))


;;;; Game

(defclass consix-window (game-window)
  ()
  (:default-initargs
   :title "Consix"))

(define-level (consix :test-order '(player t))
  (grid :named grid)
  (player :row (1- grid-rows) :col (floor grid-cols 2) :grid grid)
  (enemy :pos (vec 0 0) :structure (list 0.0 0.0 0.0) :grid grid))

(defun game ()
  (glut:display-window
   (make-instance 'consix-window :world 'consix)))
