;;;; +----------------------------------------------------------------+
;;;; | CONSIX                                             DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:consix)


;;;; Grid

(defconstant* cell-unclaimed #x00)
(defconstant* cell-claimed   #x01)
(defconstant* cell-claiming  #x02)
(defconstant* cell-edge      #x03)
(defconstant* cell-fill      #x04)

(defconstant* grid-rows 90)
(defconstant* grid-cols 90)
(defconstant* num-cells (* grid-rows grid-cols))

(defconstant* cell-width 2)
(defconstant* cell-height 2)

(defconstant* grid-x-offset (floor (* grid-cols cell-width) -2))
(defconstant* grid-y-offset (floor (* grid-rows cell-height) -2))

(defconstant* cell-weight-radius 1)

(defvar *weight-computation* :compute)

(defvar *render-cell-weights* nil)

(deftype cell () '(unsigned-byte 8))
(deftype weight () 'single-float)

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

(defsubst claimed-percentage (grid)
  (* 100.0
     (- 1.0
        (/ (current-unclaimed grid)
           (total-unclaimed grid)))))
  
(defsubst row-1+ (location) (+ location grid-cols))
(defsubst row-1- (location) (- location grid-cols))
(defsubst col-1+ (location) (+ location 1))
(defsubst col-1- (location) (- location 1))
(defsubst location (row col) (+ col (* row grid-cols)))

(defsubst valid-location-p (location)
  (and (>= location 0) (< location num-cells)))

(defun valid-row+ (location n)
  (do ((inc (* (- (signum n)) grid-cols))
       (new-location (+ location (* n grid-cols))
                     (+ new-location inc)))
      ((valid-location-p new-location) new-location)))

(defun valid-col+ (location n)
  (multiple-value-bind (row col)
      (floor location grid-rows)
    (location row (max 0 (min (1- grid-cols) (+ col n))))))

(defun make-cells ()
  (let ((cells (make-array num-cells
                           :element-type 'cell
                           :initial-element cell-unclaimed)))
    (macrolet ((ref (row col) `(aref cells (location ,row ,col))))
      (dotimes (row grid-rows)
        (setf (ref row 0) cell-edge)
        (setf (ref row (1- grid-cols)) cell-edge))
      (dotimes (col grid-cols)
        (setf (ref 0 col) cell-edge)
        (setf (ref (1- grid-rows) col) cell-edge)))
    cells))

(defun make-weights (grid)
  (compute-cell-weights (make-array num-cells :element-type 'weight) grid))

(defun compute-cell-weights (weights grid)
  (dotimes (location num-cells)
    (setf (aref weights location)
          (compute-cell-weight location grid)))
  weights)

(defun call-with-weight-computation (function value grid)
  (let ((*weight-computation* value))
    (funcall function)
    (when (eq :delay value)
      (compute-cell-weights (weights grid) grid))))
  
(defmacro with-weight-computation ((value grid) &body forms)
  `(call-with-weight-computation (lambda () ,@forms) ,value ,grid))

(defsubst cell-ref (location grid)
  (aref (the (simple-array cell) (cells grid)) location))

(defun (setf cell-ref) (new-value location grid)
  (unless (= new-value (aref (cells grid) location))
    (setf (aref (cells grid) location) new-value)
    (when (eq :compute *weight-computation*)
      (setf (cell-weight location grid) (compute-cell-weight location grid))))
  new-value)

(defsubst map-neighbors (function location radius)
  (declare (type array-index location))
  (let* ((left (valid-row+ (valid-col+ location (- radius)) (- radius)))
         (right (valid-row+ (valid-col+ location radius) (- radius)))
         (invalid-left (row-1+ (the fixnum (valid-row+ (valid-col+ location (- radius)) radius))))
         (nlocation left))
    (declare (type array-index left right nlocation))
    (dotimes (i (square (1+ (* 2 radius))))
      (when (/= location nlocation)
        (funcall function nlocation))
      (setf nlocation (col-1+ nlocation))
      (when (> nlocation right)
        (setf left (row-1+ left))
        (setf right (row-1+ right))
        (when (= left invalid-left)
          (return-from map-neighbors))
        (setf nlocation left)))))

(defmacro do-neighbors ((nlocation-var location &key (radius 1)) &body forms)
  `(map-neighbors (lambda (,nlocation-var) ,@forms) ,location ,radius))

(defmacro do-neighbor-cells ((nlocation-var ncell-var location grid &key (radius 1)) &body forms)
  (once-only (grid)
    `(do-neighbors (,nlocation-var ,location :radius ,radius)
       (let ((,ncell-var (cell-ref ,nlocation-var ,grid)))
         (declare (type cell ,ncell-var))
         ,@forms))))

(defsubst cell-weight (location grid)
  (aref (weights grid) location))

(defun (setf cell-weight) (new-value location grid)
  (unless (= new-value (aref (weights grid) location))
    (setf (aref (weights grid) location) new-value)
    (do-neighbors (nlocation location)
      (setf (cell-weight nlocation grid) (compute-cell-weight nlocation grid))))
  new-value)

(defsubst cell-local-weight (cell)
  (ecase cell
    ((#.cell-edge #.cell-claimed) 0.0)
    ((#.cell-unclaimed #.cell-fill) 0.3)
    ((#.cell-claiming) 1.0)))

(defconstant* distance-weights-offset
  (+ cell-weight-radius (* cell-weight-radius grid-cols)))

(defun compute-distance-weights ()
  (let ((row cell-weight-radius)
        (col cell-weight-radius)
        (location (location cell-weight-radius cell-weight-radius))
        (weights (make-array (* grid-cols (1+ (* cell-weight-radius 2)))
                             :element-type 'single-float
                             :initial-element 0.0)))
    (do-neighbors (nlocation location :radius cell-weight-radius)
      (multiple-value-bind (nrow ncol)
          (floor nlocation grid-rows)
        (let ((index (+ distance-weights-offset (- nlocation location))))
          (setf (aref weights index)
                (/ 1.0 (+ (square (- ncol col))
                          (square (- nrow row))
                          0.1))))))
    weights))

(defsubst cell-neighbors-weight (location grid)
  (declare (optimize (speed 3)))
  (declare (type array-index location))
  (let ((sum 0.0)
        (total-weights 0.0))
    (declare (type single-float sum total-weights))
    (do-neighbor-cells (nlocation ncell location grid :radius cell-weight-radius)
      (let ((w (aref (load-time-value
                      (the (simple-array single-float)
                        (compute-distance-weights)))
                     (+ distance-weights-offset (- nlocation location)))))
        (declare (type single-float w))
        (incf sum (* (the single-float (cell-local-weight ncell)) w))
        (incf total-weights w)))
    (/ sum total-weights)))

(defsubst compute-cell-weight (location grid)
  (let ((local-weight (cell-local-weight (cell-ref location grid))))
    (if (= 0.0 local-weight)
        0.0
        (* local-weight (cell-neighbors-weight location grid)))))

(defsubst cell-center-position (location)
  (multiple-value-bind (row col)
      (floor location grid-rows)
    (vec (+ grid-x-offset (* col cell-width) (floor cell-width 2))
         (+ grid-y-offset (* row cell-height) (floor cell-height 2)))))

(defsubst cell-location (pos)
  (multiple-value-bind (row col)
      (values (floor (- (y pos) grid-y-offset) cell-height)
              (floor (- (x pos) grid-x-offset) cell-width))
    (if (and (>= row 0) (< row grid-rows)
             (>= col 0) (< col grid-cols))
        (location row col)
        nil)))

(defmethod update ((grid grid))
  (cond ((member #\w *keys*)
         (setf *render-cell-weights* t))
        ((member #\W *keys*)
         (setf *render-cell-weights* nil))))

(defmethod render ((grid grid))
  (gl:color 1.0 1.0 1.0)
  (display-text 60 93 "Claimed: ~3,1F %" (claimed-percentage grid))
  (loop for location from 0
        for cell across (cells grid)
        do (multiple-value-bind (row col)
               (floor location grid-rows)
             (multiple-value-bind (r g b a)
                 (if *render-cell-weights*
                     (let ((w (cell-weight location grid)))
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

(defun disclaim-cells (grid)
  (with-weight-computation (:delay grid)
    (dotimes (location num-cells)
      (symbol-macrolet ((cell (cell-ref location grid)))
        (when (= cell-claiming cell)
          (setf cell cell-unclaimed))))))

(defun claim-cells (grid)
  (let ((unclaimed-neighbors '()))
    (dotimes (location num-cells)
      (symbol-macrolet ((cell (cell-ref location grid)))
        (when (= cell cell-claiming)
          (setf cell cell-edge)
          (do-neighbor-cells (nlocation ncell location grid)
            (when (= ncell cell-unclaimed)
              (push nlocation unclaimed-neighbors))))))
    (when unclaimed-neighbors
      (claim-parts unclaimed-neighbors grid))
    (let ((new-unclaimed (count cell-unclaimed (cells grid))))
      (prog1 (- (current-unclaimed grid) new-unclaimed)
        (setf (current-unclaimed grid) new-unclaimed)))))

(defun claim-parts (possibilities grid)
  (with-weight-computation (:delay grid)
    (let* ((parts (fill-claimable-parts possibilities grid))
           (most-unclaimed (reduce #'max parts :key #'second))
           (filled-biggest nil))
      (dolist (part parts)
        (destructuring-bind (location unclaimed) part
          (flet ((ff (target)
                   (flood-fill grid location cell-fill
                               (lambda (flocation)
                                 (setf (cell-ref flocation grid) target)))))
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
          (flood-fill grid location cell-unclaimed
                      (lambda (flocation)
                        (incf part-unclaimed)
                        (setf (cell-ref flocation grid) cell-fill)
                        (setf filled t)))
          (when filled
            (setf part-location location)
            (push (list nil 0) parts)))))
    parts))

(defun flood-fill (grid starting-location source visitor)
  (let ((locations (list starting-location)))
    (flet ((interesting-p (location)
             (and (valid-location-p location)
                  (= source (cell-ref location grid)))))
      (do () ((null locations))
        (let ((location (pop locations)))
          (when (interesting-p location)
            ;; We assume that we'll always find an uninteresting
            ;; location before flowing into the wrong row.
            (let ((left (do ((left (col-1- location) (col-1- left)))
                            ((not (interesting-p left)) left)))
                  (right (do ((right (col-1+ location) (col-1+ right)))
                             ((not (interesting-p right)) right))))
              (loop for fill-location from (col-1+ left) below right do
                    (funcall visitor fill-location)
                    (let ((up (row-1- fill-location))
                          (down (row-1+ fill-location)))
                      (when (interesting-p up) (push up locations))
                      (when (interesting-p down) (push down locations)))))))))))


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

(defconstant* player-movement-steps 3)
(defconstant* player-life-bonus 500000)

(defclass player ()
  ((pos :accessor pos)
   (initial-location :initarg :loc :accessor initial-location)
   (loc :initarg :loc :accessor loc)
   (grid :initarg :grid :accessor grid)
   (halo :initform (make-instance 'halo) :accessor halo)
   (movement-actions :initform '() :accessor movement-actions)
   (claiming :initform nil :accessor claiming-p)
   (lives :initarg :lives :accessor lives)
   (score :initform 0 :accessor score)))

(defmethod initialize-instance :after ((player player) &rest initargs)
  (declare (ignore initargs))
  (setf (pos player) (cell-center-position (initial-location player))))

(defmethod update ((player player))
  (when (null (movement-actions player))
    (maybe-queue-movement-actions player))
  (when-let (action (pop (movement-actions player)))
    (funcall action))
  (halo-update (halo player)))

(defmethod render ((player player))
  (gl:color 1.0 1.0 1.0)
  (display-text -90 93 "Score: ~9,'0D" (score player))
  (flet ((draw (x y)
           (gl:with-pushed-matrix
             (gl:translate x y 0.0)
             (dotimes (i 5)
               (gl:color 0.0 0.0 1.0 (* i 0.2))
               (draw-circle (- 5 i) 30 t))
             (gl:color 0.0 0.0 1.0 (halo-value (halo player)))
             (draw-circle 5))))
    (with-vec (x y (pos player))
      (draw x y))
    (dotimes (i (lives player))
      (draw -95.0 (- 85.0 (* i 10.0))))))

(defun maybe-queue-movement-actions (player)
  (flet ((goto (location)
           (let* ((target (cell-center-position location))
                  (step (vec/ (vec- target (pos player))
                              (float player-movement-steps))))
             (setf (movement-actions player)
                   (loop for i from 1 to player-movement-steps
                         if (< i player-movement-steps)
                         collect (lambda () (vec+= (pos player) step))
                         else
                         collect (lambda ()
                                   (setf (pos player) target)
                                   (setf (loc player) location)
                                   (player-changed-cell player)))))))
    (when-let (move (find-if (lambda (move)
                               (movement-possible-p move player))
                             (requested-moves (loc player))))
      (goto move))))

(defun requested-moves (location)
  (mapcan (lambda (key)
            (case key
              (:key-left (list (valid-col+ location -1)))
              (:key-right (list (valid-col+ location 1)))
              (:key-up (list (valid-row+ location 1)))
              (:key-down (list (valid-row+ location -1)))))
          *keys*))

(defsubst movement-possible-p (location player)
  (or (= cell-edge (cell-ref location (grid player)))
      (and (member #\Space *keys*)
           (= cell-unclaimed (cell-ref location (grid player))))))

(defun player-changed-cell (player)
  (symbol-macrolet ((cell (cell-ref (loc player) (grid player))))
    (case cell
      (#.cell-unclaimed
       (setf cell cell-claiming)
       (setf (claiming-p player) t))
      (#.cell-edge
       (when (claiming-p player)
         (increment-score (* 10 (claim-cells (grid player))) player)
         (setf (claiming-p player) nil)))
      (t (warn "Player changed to a cell it shouldn't have changed to (~D)." cell)))))

(defun player-die (player)
  (cond ((plusp (lives player))
         (decf (lives player))
         (disclaim-cells (grid player))
         (setf (claiming-p player) nil)
         (setf (loc player) (initial-location player))
         (setf (pos player) (cell-center-position (initial-location player)))
         (setf (movement-actions player) '()))
        (t (outer-world))))

(defun increment-score (increment player)
  (symbol-macrolet ((score (score player)))
    (let ((new-score (+ score increment)))
      (when (/= (floor new-score player-life-bonus)
                (floor score player-life-bonus))
        (incf (lives player)))
      (setf score new-score))))


;;;; Enemy

(defconstant* enemy-death-ticks 64)
(defconstant* enemy-growth-completion-ticks 64)
(defconstant* enemy-flexibility 1.5)
(defconstant* enemy-max-size 8)

(defclass enemy ()
  ((pos :initarg :pos :accessor pos)
   (grid :initarg :grid :accessor grid)
   (target-location :initform nil :accessor target-location)
   (structure :initarg :structure :accessor enemy-structure)
   (death-tick :initform nil :accessor death-tick)
   (growth-tick :initform nil :accessor growth-tick)))

(defmethod update ((enemy enemy))
  (let ((location (cell-location (pos enemy))))
    (assert (not (null location)))
    (cond ((death-tick enemy)
           (when (= (incf (death-tick enemy)) enemy-death-ticks)
             (remove-object enemy)))
          ((= cell-claimed (cell-ref location (grid enemy)))
           (enemy-die enemy))
          ((need-new-target-p location enemy)
           (setf (target-location enemy) (choose-target-cell location enemy)))))
  (when (null (death-tick enemy))
    (enemy-fix-direction enemy)
    (enemy-forward enemy)
    (enemy-check-claiming enemy)
    (enemy-check-growth enemy)))

(defsubst need-new-target-p (location enemy)
  (or (null (target-location enemy)) (= location (target-location enemy))))

(defun choose-target-cell (location enemy)
  (let* ((neighbors (collect-potential-target-cells location enemy))
         (weights-sum (reduce #'+ neighbors :key #'first))
         (choice (random weights-sum)))
    (loop for (weight nlocation) in neighbors
          summing weight into sum
          when (> sum choice)
          return nlocation)))

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

(defsubst angle-between-position-and-location (source-position target-location)
  (angle-between-positions source-position (cell-center-position target-location)))

(defsubst enemy-target-angle (enemy)
  (angle-between-position-and-location (pos enemy) (target-location enemy)))

(defun enemy-fix-direction (enemy)
  (let ((target-angle (enemy-target-angle enemy))
        (head-angle (head-angle enemy)))
    (multiple-value-bind (difference op)
        (angle- target-angle head-angle)
      (let ((new-angle (funcall op head-angle (/ difference 10.0))))
        (setf (head-angle enemy) new-angle)
        (map-into (rest (enemy-structure enemy))
                  (lambda (x)
                    (prog1 (* enemy-flexibility (- head-angle new-angle))
                      (setf head-angle (+ x new-angle))))
                  (rest (enemy-structure enemy)))))))

(defsubst enemy-forward (enemy)
  (vec+= (pos enemy) (vel-vec 0.2 (head-angle enemy))))

(defsubst angle-multiplier (source-angle target-angle)
  (let ((difference (angle- source-angle target-angle)))
    (cond ((< difference 30.0) 1.0)
          ((< difference 60.0) 0.1)
          (t 0.05))))

(defun collect-potential-target-cells (location enemy)
  (let ((result '())
        (source-angle (head-angle enemy))
        (pos (pos enemy))
        (grid (grid enemy)))
    (do-neighbors (nlocation location)
      (let ((weight (cell-weight nlocation grid)))
        (when (plusp weight)
          (let* ((target-angle (angle-between-position-and-location pos nlocation))
                 (m (angle-multiplier source-angle target-angle)))
            (push (list (* weight m) nlocation) result)))))
    result))

(defmethod render ((enemy enemy))
  (gl:with-pushed-matrix
    (with-vec (x y (pos enemy))
      (gl:translate x y 0.0))
    (multiple-value-bind (alpha scale)
        (enemy-alpha-and-scale enemy)
      (gl:scale scale scale 1.0)
      (gl:color 1.0 1.0 0.0 alpha))
    (render-structure (enemy-structure enemy) (growth-tick enemy))))

(defsubst enemy-alpha-and-scale (enemy)
  (if (null (death-tick enemy))
      (values 1.0 1.0)
      (let ((ratio (float (/ (death-tick enemy) enemy-death-ticks))))
        (values (- 1.0 ratio)
                (+ 1.0 (* 3.0 ratio))))))

(defun render-structure (object growth-tick)
  (typecase object
    (atom)
    (cons
     (gl:rotate (car object) 0.0 0.0 1.0)
     (render-box-pair)
     (cond ((cdr object)
            (when (and growth-tick (consp (cdr object)) (null (cddr object)))
              (let ((growth-scale (float (/ growth-tick enemy-growth-completion-ticks))))
                (gl:scale growth-scale growth-scale 1.0)))
            (gl:translate 6.0 0.0 0.0)
            (render-arrow)
            (gl:translate 6.0 0.0 0.0)
            (render-structure (cdr object) growth-tick))
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

(defun structure-positions (object growth-tick)
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
                (if (and growth-tick (consp (cdr object)) (null (cddr object)))
                    positions
                    (nconc positions (structure-positions (cdr object) growth-tick))))
               (t positions)))))))

(defun structure-positions-toplevel (enemy)
  (gl:with-pushed-matrix
    (with-vec (x y (pos enemy))
      (gl:translate x y 0.0))
    (structure-positions (enemy-structure enemy) (growth-tick enemy))))

(defun enemy-check-claiming (enemy)
  (dolist (pos (structure-positions-toplevel enemy))
    (let ((location (cell-location pos)))
      (when (and location (= cell-claiming (cell-ref location (grid enemy))))
        (enemy-kill-player enemy)))))

(defun enemy-kill-player (enemy)
  (declare (ignore enemy))
  (do-objects (player :type 'player)
    (player-die player)))

(defun enemy-die (enemy)
  (setf (death-tick enemy) 0)
  (do-objects (player :type 'player)
    (increment-score (floor (score player) 10) player)))

(defun enemy-grow (enemy)
  (when (< (length (enemy-structure enemy)) enemy-max-size)
    (setf (growth-tick enemy) 0)
    (appendf (enemy-structure enemy) (list (lastcar (enemy-structure enemy))))))
          
(defun enemy-check-growth (enemy)
  (when (growth-tick enemy)
    (setf (growth-tick enemy)
          (if (= enemy-growth-completion-ticks (growth-tick enemy))
              nil
              (1+ (growth-tick enemy))))))


;;;; Level timer

(defconstant* enemy-growth-ticks 1000)

(defclass level-timer ()
  ())

(defmethod update ((timer level-timer))
  (when (and (plusp *tick*)
             (zerop (mod *tick* enemy-growth-ticks)))
    (do-objects (enemy :type 'enemy)
      (enemy-grow enemy))))


;;;; Game

(defclass consix-window (game-window)
  ()
  (:default-initargs
   :title "CONSIX"))

(define-level (consix :test-order '(player enemy t))
  (grid :named grid)
  (level-timer)
  (player :lives 3 :loc (location (1- grid-rows) (floor grid-cols 2)) :grid grid)
  (enemy :pos (vec -10 0) :structure (list 0.0) :grid grid)
  (enemy :pos (vec +10 0) :structure (list 0.0) :grid grid))

(defun game ()
  (glut:display-window
   (make-instance 'consix-window :world 'consix)))
