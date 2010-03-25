;;;; +----------------------------------------------------------------+
;;;; | CONSIX                                             DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:consix)


;;;; Grid

(defconstant cell-unclaimed #x00)
(defconstant cell-claimed   #x01)
(defconstant cell-claiming  #x02)
(defconstant cell-edge      #x03)
(defconstant cell-fill      #x04)

(defconstant grid-rows 90)
(defconstant grid-cols 90)

(defconstant cell-width 2)
(defconstant cell-height 2)

(defconstant grid-x-offset (floor (* grid-cols cell-width) -2))
(defconstant grid-y-offset (floor (* grid-rows cell-height) -2))

(defconstant cell-weight-radius 2)

(defvar *weight-computation* :compute)

(defvar *render-cell-weights* nil)

(deftype cell () '(unsigned-byte 8))
(deftype weight () 'single-float)
(deftype small-integer () '(integer -1000 1000))

(defclass grid ()
  ((total-unclaimed :accessor total-unclaimed)
   (current-unclaimed :accessor current-unclaimed)
   (cells :accessor cells)
   (weights :accessor weights)))

(defmethod initialize-instance :after ((grid grid) &rest initargs)
  (declare (ignore initargs))
  (setf (cells grid) (make-cells))
  (setf (weights grid) (make-weights grid))
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

(defun make-weights (grid)
  (compute-cell-weights
   (make-array (* grid-rows grid-cols)
               :element-type 'weight)
   grid))

(defun compute-cell-weights (weights grid)
  (macrolet ((ref (row col) `(aref weights (+ ,col (* ,row grid-cols)))))
    (dotimes (row grid-rows)
      (dotimes (col grid-cols)
        (setf (ref row col)
              (compute-cell-weight row col grid)))))
  weights)

(defun call-with-weight-computation (function value grid)
  (let ((*weight-computation* value))
    (funcall function)
    (when (eq :delay value)
      (compute-cell-weights (weights grid) grid))))
  
(defmacro with-weight-computation ((value grid) &body forms)
  `(call-with-weight-computation (lambda () ,@forms) ,value ,grid))

(defsubst cell-ref/index (index grid)
  (aref (the (simple-array cell) (cells grid)) index))

(defsubst cell-ref (row col grid)
  (cell-ref/index (+ (* row grid-cols) col) grid))

(defun (setf cell-ref) (new-value row col grid)
  (let ((index (+ (* row grid-cols) col)))
    (unless (= new-value (aref (cells grid) index))
      (setf (aref (cells grid) index) new-value)
      (when (eq :compute *weight-computation*)
        (setf (cell-weight row col grid) (compute-cell-weight row col grid)))))
  new-value)

(defmacro do-neighbors ((nrow-var ncol-var row col &key (radius 1) (include-self nil) index) &body forms)
  (with-gensyms (min-row max-row min-col max-col row-index)
    (once-only (row col radius)
      `(let ((,min-row (max 0 (- ,row ,radius)))
             (,max-row (min (1- grid-rows) (+ ,row ,radius)))
             (,min-col (max 0 (- ,col ,radius)))
             (,max-col (min (1- grid-cols) (+ ,col ,radius))))
         (declare (type small-integer ,min-row ,max-row ,min-col ,max-col ,radius ,row ,col))
         (do ((,nrow-var ,min-row (1+ ,nrow-var))
              ,@(when index `((,row-index (+ ,min-col (* ,min-row grid-cols))
                                          (+ ,row-index grid-cols)))))
             ((> ,nrow-var ,max-row))
           (declare (type small-integer ,nrow-var))
           ,@(when index `((declare (type array-index ,row-index))))
           (declare (ignorable ,nrow-var))
           (do ((,ncol-var ,min-col (1+ ,ncol-var))
                ,@(when index `((,index ,row-index (1+ ,index)))))
               ((> ,ncol-var ,max-col))
             (declare (type small-integer ,ncol-var))
             ,@(when index `((declare (type array-index ,index))))
             (declare (ignorable ,ncol-var))
             ,(if include-self
                  `(progn ,@forms)
                  `(when (and (/= ,nrow-var ,row) (/= ,ncol-var ,col))
                     ,@forms))))))))

(defmacro do-neighbor-cells ((nrow-var ncol-var ncell-var row col grid &key (radius 1) (include-self nil)) &body forms)
  (let ((index (gensym)))
    `(do-neighbors (,nrow-var ,ncol-var ,row ,col :radius ,radius :include-self ,include-self :index ,index)
       (let ((,ncell-var (cell-ref/index ,index ,grid)))
         (declare (type cell ,ncell-var))
         ,@forms))))

(defsubst cell-weight (row col grid)
  (aref (weights grid) (+ col (* row grid-cols))))

(defun (setf cell-weight) (new-value row col grid)
  (let ((index (+ (* row grid-cols) col)))
    (unless (= new-value (aref (weights grid) index))
      (setf (aref (weights grid) index) new-value)
      (do-neighbors (nrow ncol row col :radius cell-weight-radius)
        (setf (cell-weight nrow ncol grid)
              (compute-cell-weight nrow ncol grid)))))
  new-value)

(defsubst cell-local-weight (cell)
  (ecase cell
    ((#.cell-edge #.cell-claimed) 0.0)
    ((#.cell-unclaimed #.cell-fill) 0.3)
    ((#.cell-claiming) 1.0)))

(defun compute-distance-weights (radius)
  (let* ((rows (1+ (* 2 radius)))
         (cols (1+ (* 2 radius)))
         (row radius)
         (col radius)
         (weights (make-array (list rows cols) :element-type 'single-float)))
    (dotimes (nrow rows)
      (dotimes (ncol cols)
        (setf (aref weights nrow ncol)
              (/ 1.0 (+ (square (- ncol col))
                        (square (- nrow row))
                        0.1)))))
    weights))

(defsubst cell-neighbors-weight (row col grid)
  (declare (optimize (speed 3)))
  (declare (type small-integer row col))
  (let ((sum 0.0)
        (total-weights 0.0))
    (declare (type single-float sum total-weights))
    (do-neighbor-cells (nrow ncol ncell row col grid :radius cell-weight-radius)
      (let ((w (aref (the (simple-array single-float (* *))
                       (load-time-value (compute-distance-weights cell-weight-radius)))
                     (+ (- nrow row) cell-weight-radius)
                     (+ (- ncol col) cell-weight-radius))))
        (declare (type single-float w))
        (incf sum (* (the single-float (cell-local-weight ncell)) w))
        (incf total-weights w)))
    (/ sum total-weights)))

(defsubst compute-cell-weight (row col grid)
  (let ((local-weight (cell-local-weight (cell-ref row col grid))))
    (if (= 0.0 local-weight)
        0.0
        (* local-weight (cell-neighbors-weight row col grid)))))

(defsubst cell-center-position (row col)
  (vec (+ grid-x-offset (* col cell-width) (floor cell-width 2))
       (+ grid-y-offset (* row cell-height) (floor cell-height 2))))

(defsubst cell-row/col (pos)
  (values (floor (- (y pos) grid-y-offset) cell-height)
          (floor (- (x pos) grid-x-offset) cell-width)))

(defsubst valid-cell-p (row col)
  (and (>= row 0) (< row grid-rows)
       (>= col 0) (< col grid-cols)))

(defmethod update ((grid grid))
  (cond ((member #\w *keys*)
         (setf *render-cell-weights* t))
        ((member #\W *keys*)
         (setf *render-cell-weights* nil))))

(defmethod render ((grid grid))
  (loop for i from 0
        for cell across (cells grid)
        do (multiple-value-bind (row col)
               (floor i grid-rows)
             (multiple-value-bind (r g b a)
                 (if *render-cell-weights*
                     (let ((w (cell-weight row col grid)))
                       (values w w w 1.0))
                     (ecase cell
                       (#.cell-unclaimed (values 0.0 0.0 0.0 0.0))
                       (#.cell-claimed   (values 0.0 1.0 0.0 0.5))
                       (#.cell-claiming  (values 1.0 0.0 0.0 0.5))
                       (#.cell-edge      (values 0.0 1.0 0.0 1.0))))
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
          (when (= cell cell-claiming)
            (setf cell cell-edge)
            (do-neighbor-cells (nrow ncol ncell row col grid)
              (when (= ncell cell-unclaimed)
                (push (list nrow ncol) unclaimed-neighbors)))))))
    (when unclaimed-neighbors
      (claim-parts unclaimed-neighbors grid))
    (setf (current-unclaimed grid) (count cell-unclaimed (cells grid)))))

(defun claim-parts (possibilities grid)
  (with-weight-computation (:delay grid)
    (let* ((parts (fill-claimable-parts possibilities grid))
           (most-unclaimed (reduce #'max parts :key #'second))
           (filled-biggest nil))
      (dolist (part parts)
        (destructuring-bind (location unclaimed) part
          (flet ((ff (target)
                   (destructuring-bind (row col) location
                     (flood-fill grid row col cell-fill
                                 (lambda (frow fcol)
                                   (setf (cell-ref frow fcol grid) target))))))
            (cond ((null location))
                  ((or filled-biggest (< unclaimed most-unclaimed))
                   (ff cell-claimed))
                  (t
                   (ff cell-unclaimed)
                   (setf filled-biggest t)))))))))

(defun fill-claimable-parts (possibilities grid)
  (let ((parts (list (list nil 0))))
    (symbol-macrolet ((part-location (first (first parts)))
                      (part-unclaimed (second (first parts))))
      (do () ((null possibilities))
        (let ((filled nil)
              (location (pop possibilities)))
          (destructuring-bind (row col) location
            (flood-fill grid row col cell-unclaimed
                        (lambda (frow fcol)
                          (incf part-unclaimed)
                          (setf (cell-ref frow fcol grid) cell-fill)
                          (setf filled t)))
            (when filled
              (setf part-location location)
              (push (list nil 0) parts))))))
    parts))

(defun flood-fill (grid starting-row starting-col source visitor)
  (let ((locations (list (list starting-row starting-col))))
    (flet ((interesting-p (row col)
             (and (valid-cell-p row col)
                  (= source (cell-ref row col grid)))))
      (do () ((null locations))
        (destructuring-bind (row col) (pop locations)
          (when (interesting-p row col)
            (let ((left (do ((left col (1- left)))
                            ((not (interesting-p row left)) left)))
                  (right (do ((right col (1+ right)))
                             ((not (interesting-p row right)) right))))
              (loop for fill-col from (1+ left) below right do
                    (funcall visitor row fill-col)
                    (when (interesting-p (1- row) fill-col)
                      (push (list (1- row) fill-col) locations))
                    (when (interesting-p (1+ row) fill-col)
                      (push (list (1+ row) fill-col) locations))))))))))


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

(defsubst movement-possible-p (row col player)
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

(defconstant enemy-death-ticks 64)

(defclass enemy ()
  ((pos :initarg :pos :accessor pos)
   (grid :initarg :grid :accessor grid)
   (target-row :initform nil :accessor target-row)
   (target-col :initform nil :accessor target-col)
   (structure :initarg :structure :accessor enemy-structure)
   (death-tick :initform nil :accessor death-tick)))

(defmethod update ((enemy enemy))
  (multiple-value-bind (row col)
      (cell-row/col (pos enemy))
    (cond ((death-tick enemy)
           (when (= (incf (death-tick enemy)) enemy-death-ticks)
             (remove-object enemy)))
          ((= cell-claimed (cell-ref row col (grid enemy)))
           (setf (death-tick enemy) 0))
          ((need-new-target-p row col enemy)
           (setf (values (target-row enemy) (target-col enemy))
                 (choose-target-cell row col enemy)))))
  (when (null (death-tick enemy))
    (enemy-fix-direction enemy)
    (enemy-forward enemy)
    (enemy-check-claiming enemy)))

(defsubst need-new-target-p (row col enemy)
  (or (and (null (target-row enemy))
           (null (target-col enemy)))
      (and (= row (target-row enemy))
           (= col (target-col enemy)))))

(defun choose-target-cell (row col enemy)
  (let* ((neighbors (collect-potential-target-cells row col enemy))
         (weights-sum (reduce #'+ neighbors :key #'first))
         (choice (random weights-sum)))
    (loop for (weight nrow ncol) in neighbors
          summing weight into sum
          when (> sum choice)
          return (values nrow ncol))))

(defsubst angle- (angle-1 angle-2)
  (let ((d1 (normalize-deg (- angle-1 angle-2)))
        (d2 (normalize-deg (- angle-2 angle-1))))
    (if (< d1 d2)
        (values d1 '+)
        (values d2 '-))))

(defsubst head-angle (enemy)
  (first (enemy-structure enemy)))

(defsubst (setf head-angle) (new-value enemy)
  (setf (first (enemy-structure enemy)) new-value))
                            
(defsubst angle-between-positions (source-position target-position)
  (if (vec=~ source-position target-position)
      0.0
      (normalize-deg (vec-angle (vec- source-position target-position)))))

(defsubst angle-between-position-and-cell (source-position target-row target-col)
  (angle-between-positions source-position
                           (cell-center-position target-row target-col)))

(defsubst enemy-target-angle (enemy)
  (angle-between-position-and-cell (pos enemy)
                                   (target-row enemy)
                                   (target-col enemy)))

(defun enemy-fix-direction (enemy)
  (let ((target-angle (enemy-target-angle enemy))
        (head-angle (head-angle enemy)))
    (multiple-value-bind (difference op)
        (angle- target-angle head-angle)
      (let ((new-angle (funcall op head-angle (/ difference 10.0))))
        (setf (head-angle enemy) new-angle)
        (map-into (rest (enemy-structure enemy))
                  (lambda (x)
                    (prog1 (* 2.0 (- head-angle new-angle))
                      (setf head-angle (+ x new-angle))))
                  (rest (enemy-structure enemy)))))))

(defsubst enemy-forward (enemy)
  (vec+= (pos enemy) (vel-vec 0.2 (head-angle enemy))))

(defsubst angle-multiplier (source-angle target-angle)
  (let ((difference (angle- source-angle target-angle)))
    (cond ((< difference 30.0) 1.0)
          ((< difference 60.0) 0.1)
          (t 0.05))))

(defun collect-potential-target-cells (row col enemy)
  (let ((result '())
        (source-angle (head-angle enemy))
        (pos (pos enemy))
        (grid (grid enemy)))
    (do-neighbors (nrow ncol row col)
      (let ((weight (cell-weight nrow ncol grid)))
        (when (plusp weight)
          (let* ((target-angle (angle-between-position-and-cell pos nrow ncol))
                 (m (angle-multiplier source-angle target-angle)))
            (push (list (* weight m) nrow ncol) result)))))
    result))

(defmethod render ((enemy enemy))
  (gl:with-pushed-matrix
    (with-vec (x y (pos enemy))
      (gl:translate x y 0.0))
    (multiple-value-bind (alpha scale)
        (enemy-alpha-and-scale enemy)
      (gl:scale scale scale 1.0)
      (gl:color 1.0 1.0 0.0 alpha))
    (render-structure (enemy-structure enemy))))

(defsubst enemy-alpha-and-scale (enemy)
  (if (null (death-tick enemy))
      (values 1.0 1.0)
      (let ((ratio (float (/ (death-tick enemy) enemy-death-ticks))))
        (values (- 1.0 ratio)
                (+ 1.0 (* 3.0 ratio))))))

(defun render-structure (object)
  (typecase object
    (atom)
    (cons
     (gl:rotate (car object) 0.0 0.0 1.0)
     (render-box-pair)
     (cond ((cdr object)
            (gl:translate 6.0 0.0 0.0)
            (render-arrow)
            (gl:translate 6.0 0.0 0.0)
            (render-structure (cdr object)))
           (t
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

(defparameter *identity-matrix*
  #(1.0d0 0.0d0 0.0d0 0.0d0
    0.0d0 1.0d0 0.0d0 0.0d0
    0.0d0 0.0d0 1.0d0 0.0d0
    0.0d0 0.0d0 0.0d0 1.0d0))

(defun structure-positions (object)
  ;; Eewww, what a disgusting hack!
  (let ((positions '()))
    (flet ((add (x y)
             (multiple-value-bind (x y)
                 (glu:project x y 0.0 :projection *identity-matrix* :viewport #(-1 -1 2 2))
               (push (vec x y) positions))))
      (typecase object
        (atom '())
        (cons
         (gl:rotate (car object) 0.0 0.0 1.0)
         (add 2.0 0.0)
         (add 6.0 0.0)
         (cond ((cdr object)
                (gl:translate 12.0 0.0 0.0)
                (nconc positions (structure-positions (cdr object))))
               (t positions)))))))

(defun structure-positions-toplevel (enemy)
  (gl:with-pushed-matrix
    (with-vec (x y (pos enemy))
      (gl:translate x y 0.0))
    (structure-positions (enemy-structure enemy))))

(defun enemy-check-claiming (enemy)
  (dolist (pos (structure-positions-toplevel enemy))
    (multiple-value-bind (row col)
        (cell-row/col pos)
      (when (and (valid-cell-p row col)
                 (= cell-claiming (cell-ref row col (grid enemy))))
        (enemy-kill-player enemy)))))

(defun enemy-kill-player (enemy)
  (declare (ignore enemy))
  (do-objects (player :type 'player)
    (remove-object player)))


;;;; Game

(defclass consix-window (game-window)
  ()
  (:default-initargs
   :title "CONSIX"))

(define-level (consix :test-order '(player enemy t))
  (grid :named grid)
  (player :row (1- grid-rows) :col (floor grid-cols 2) :grid grid)
  (enemy :pos (vec 0 0) :structure (list 0.0 0.0 0.0) :grid grid))

(defun game ()
  (glut:display-window
   (make-instance 'consix-window :world 'consix)))
