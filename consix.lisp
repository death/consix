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
         (claim-cells player)
         (setf (claiming-p player) nil)))
      (t (warn "Player changed to a cell it shouldn't have changed to (~D)." cell)))))

(defun claim-cells (player)
  (map-into (cells (grid player))
            (lambda (cell)
              (if (= cell cell-claiming)
                  cell-edge
                  cell))
            (cells (grid player))))


;;;; Game

(defclass consix-window (game-window)
  ()
  (:default-initargs
   :title "Consix"))

(define-level (consix :test-order '(player t))
  (grid :named grid)
  (player :row (1- grid-rows) :col (floor grid-cols 2) :grid grid))

(defun game ()
  (glut:display-window
   (make-instance 'consix-window :world 'consix)))
