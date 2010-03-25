;;;; +----------------------------------------------------------------+
;;;; | CONSIX                                             DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:gob)


;;;; Utilities

(defmacro defsubst (name lambda-list &body forms)
  "Define an inline function at top level."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@forms)))

(defconstant single-pi (coerce pi 'single-float))

(defsubst rad (deg)
  (/ (* single-pi deg) 180.0))

(defsubst deg (rad)
  (/ (* 180.0 rad) single-pi))

(defsubst normalize-deg (deg)
  (loop while (>= deg 360.0) do (decf deg 360.0))
  (loop while (< deg 0.0) do (incf deg 360.0))
  deg)

(defsubst sind (deg)
  (sin (rad deg)))

(defsubst cosd (deg)
  (cos (rad deg)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun collect-results (fn &rest args)
    (let ((collection '()))
      (apply fn
             (lambda (&rest results)
               (push results collection))
             args)
      (nreverse collection)))
  
  (defun call-with-circle-multipliers (fn &optional (segments 30))
    ;; http://github.com/sykopomp/until-it-dies/blob/master/src/primitives.lisp
    ;; Stole implementation of draw-circle and modified it a bit
    (let* ((theta (* 2.0 (/ single-pi segments)))
           (tangential-factor (tan theta))
           (radial-factor (- 1.0 (cos theta))))
      (loop with x = 1.0
            with y = 0.0
            repeat segments
            do (funcall fn x y)
            (let ((tx (- y))
                  (ty x))
              (incf x (* tx tangential-factor))
              (incf y (* ty tangential-factor)))
            (let ((rx (- x))
                  (ry (- y)))
              (incf x (* rx radial-factor))
              (incf y (* ry radial-factor)))))))

(define-compiler-macro draw-circle (&whole form radius &optional (segments 30) (filledp nil))
  (if (integerp segments)
      (once-only (radius)
        `(gl:with-primitives (if ,filledp :triangle-fan :line-loop)
           (loop for (x y) in ',(collect-results #'call-with-circle-multipliers segments)
                 do (gl:vertex (* ,radius x) (* ,radius y)))))
      form))
      
(defun draw-circle (radius &optional (segments 30) (filledp nil))
  (gl:with-primitives (if filledp :triangle-fan :line-loop)
    (call-with-circle-multipliers
     (lambda (x y) (gl:vertex (* x radius) (* y radius)))
     segments)))

(defsubst mod+ (n m p)
  (mod (+ n m) p))
  
(defun call-with-star-multipliers (fn points density)
  (let ((xs (make-array points :element-type 'single-float))
        (ys (make-array points :element-type 'single-float)))
    (let ((i 0))
      (call-with-circle-multipliers
       (lambda (x y)
         (setf (aref xs i) x)
         (setf (aref ys i) y)
         (incf i))
       points))
    (dotimes (i points)
      (let ((j (mod+ i density points)))
        (funcall fn
                 (aref xs i) (aref ys i)
                 (aref xs j) (aref ys j))))))

(define-compiler-macro draw-star (&whole form radius points density)
  (if (and (integerp points) (integerp density))
      (once-only (radius)
        `(gl:with-primitive :lines
           (loop for (x1 y1 x2 y2) in ',(collect-results #'call-with-star-multipliers points density) do
                 (gl:vertex (* ,radius x1) (* ,radius y1))
                 (gl:vertex (* ,radius x2) (* ,radius y2)))))
      form))

(defun draw-star (radius points density)
  (gl:with-primitive :lines
    (call-with-star-multipliers
     (lambda (x1 y1 x2 y2)
       (gl:vertex (* x1 radius) (* y1 radius))
       (gl:vertex (* x2 radius) (* y2 radius)))
     points density)))

(defun call-with-curve-multipliers (fn &optional (segments 20))
  (funcall fn 1.0 0.0 0.0 0.0)
  (loop with step = (/ 1.0 segments)
        repeat (- segments 2)
        for u = step then (+ u step)
        for v = (- 1.0 u)
        for am = (* 1.0 v v v)
        for bm = (* 3.0 v v u)
        for cm = (* 3.0 v u u)
        for dm = (* 1.0 u u u)
        do (funcall fn am bm cm dm))
  (funcall fn 0.0 0.0 0.0 1.0))

(define-compiler-macro draw-cubic-curve (&whole form ax ay bx by cx cy dx dy &optional (segments 20))
  (if (integerp segments)
      (once-only (ax ay bx by cx cy dx dy)
        `(gl:with-primitive :line-strip
           (loop for (am bm cm dm) in ',(collect-results #'call-with-curve-multipliers segments)
                 do (gl:vertex (+ (* am ,ax) (* bm ,bx) (* cm ,cx) (* dm ,dx))
                               (+ (* am ,ay) (* bm ,by) (* cm ,cy) (* dm ,dy))))))
      form))

(defun draw-cubic-curve (ax ay bx by cx cy dx dy &optional (segments 20))
  (gl:with-primitive :line-strip
    (call-with-curve-multipliers
     (lambda (am bm cm dm)
       (gl:vertex (+ (* am ax) (* bm bx) (* cm cx) (* dm dx))
                  (+ (* am ay) (* bm by) (* cm cy) (* dm dy))))
     segments)))

(defsubst square (x)
  (* x x))

(defmacro define-symmetric (name ((a class1) (b class2)) &body forms)
  `(progn
     (defmethod ,name ((,a ,class1) (,b ,class2)) ,@forms)
     ,@(unless (equal class1 class2)
         `((defmethod ,name ((,b ,class2) (,a ,class1)) (,name ,a ,b))))
     ',name))


;;;; Configuration

(defparameter *frames-per-second* 60)
(defparameter *tick-duration* (floor 1000 *frames-per-second*))
(defparameter *draw-collision-shape-for-type* 'nil)
(defparameter *draw-tick* nil)


;;;; 2D vectors

(defsubst vec (x y) (cons x y))

(defsubst x (vec) (car vec))

(defsubst y (vec) (cdr vec))

(defsubst (setf x) (new-x vec)
  (setf (car vec) new-x))

(defsubst (setf y) (new-y vec)
  (setf (cdr vec) new-y))

(defsubst copy-vec (v)
  (vec (x v) (y v)))

(defsubst vec-assign (v x y)
  (setf (x v) x)
  (setf (y v) y)
  v)

(defsubst unit (&optional (dir 0.0))
  (if (consp dir)
      (vec/ dir (vec-mag dir))
      (vec (- (cosd dir)) (- (sind dir)))))

(defsubst vec-clear (vec)
  (setf (x vec) 0.0)
  (setf (y vec) 0.0)
  vec)

(defsubst vec-mul (v1 v2)
  (+ (* (x v1) (x v2))
     (* (y v1) (y v2))))

(defsubst vec+ (v1 v2)
  (vec (+ (x v1) (x v2))
       (+ (y v1) (y v2))))

(defsubst vec+= (v1 v2)
  (incf (x v1) (x v2))
  (incf (y v1) (y v2))
  v1)

(defsubst vec- (v1 v2)
  (vec (- (x v1) (x v2))
       (- (y v1) (y v2))))

(defsubst vec-= (v1 v2)
  (decf (x v1) (x v2))
  (decf (y v1) (y v2))
  v1)

(defsubst vec* (v a)
  (vec (* (x v) a)
       (* (y v) a)))

(defsubst vec*= (v a)
  (setf (x v) (* (x v) a))
  (setf (y v) (* (y v) a))
  v)

(defsubst vec/ (v a)
  (vec (/ (x v) a)
       (/ (y v) a)))

(defsubst vec/= (v a)
  (setf (x v) (/ (x v) a))
  (setf (y v) (/ (y v) a))
  v)

(defsubst vec-mag (v)
  (sqrt (+ (square (x v)) (square (y v)))))

(defsubst vec-distance (v1 v2)
  (sqrt (vec-distance-sq v1 v2)))

(defsubst vec-distance-sq (v1 v2)
  (+ (square (- (x v1) (x v2)))
     (square (- (y v1) (y v2)))))

(defsubst vec-contains (v1 v2 &optional (r 1.0))
  (vec-contains-xy v1 (x v2) (y v2) r))

(defsubst vec-contains-xy (v x y &optional (r 1.0))
  (and (>= x (* (- (x v)) r))
       (<= x (* (x v) r))
       (>= y (* (- (y v)) r))
       (<= y (* (y v) r))))

(defmacro with-vec ((x y vec &optional (update nil)) &body forms)
  (once-only (vec)
    `(,(if update
           'symbol-macrolet
           'let)
       ((,x (car ,vec))
        (,y (cdr ,vec)))
       ,@forms)))

(defsubst vec=~ (v1 v2 &optional (epsilon 0.1))
  (flet ((=~ (a b) (< (abs (- a b)) epsilon)))
    (and (=~ (x v1) (x v2))
         (=~ (y v1) (y v2)))))

(defsubst vel-vec (mag dir)
  (vec*= (unit dir) mag))

(defsubst vec-angle (vec)
  (deg (atan (y vec) (x vec))))


;;;; Collision detection

(defclass collidable-object ()
  ())

(defgeneric collide-p (a b))

(defmethod collide-p :around (a b)
  (or (eql a b)
      (call-next-method)))

(defclass point-collidable-object (collidable-object)
  ((pos :initarg :pos :accessor pos)))

(defclass circle-collidable-object (collidable-object)
  ((collision-radius :initarg :collision-radius :accessor collision-radius)
   (pos :initarg :pos :accessor pos)))

(defsubst close-enough-p (pa ra pb rb)
  (< (vec-distance-sq pa pb) (square (+ ra rb))))

(define-symmetric collide-p ((a circle-collidable-object)
                             (b circle-collidable-object))
  (close-enough-p (pos a) (collision-radius a)
                  (pos b) (collision-radius b)))

(define-symmetric collide-p ((a circle-collidable-object)
                             (b point-collidable-object))
  (close-enough-p (pos a) (collision-radius a)
                  (pos b) 1))

(defclass line-segment-collidable-object (collidable-object)
  ((start-pos :initarg :start-pos :accessor start-pos)
   (end-pos :initarg :end-pos :accessor end-pos)))

(defun closest-point-on-segment (start-pos end-pos circle-pos)
  (let* ((seg-v (vec- end-pos start-pos))
         (pt-v (vec- circle-pos start-pos))
         (seg-v-unit (unit seg-v))
         (proj (vec-mul pt-v seg-v-unit)))
    (cond ((<= proj 0) start-pos)
          ((>= proj (vec-mag seg-v)) end-pos)
          (t (vec+= (vec* seg-v-unit proj) start-pos)))))

(defsubst segment-collides-with-circle-p (start-pos end-pos circle-pos circle-radius)
  (let ((closest (closest-point-on-segment start-pos end-pos circle-pos)))
    (<= (vec-mag (vec- circle-pos closest)) circle-radius)))

(define-symmetric collide-p ((a line-segment-collidable-object)
                             (b circle-collidable-object))
  (segment-collides-with-circle-p (start-pos a) (end-pos a) (pos b) (collision-radius b)))

(defclass box-collidable-object (collidable-object)
  ((top-left :initarg :top-left :accessor top-left)
   (bottom-right :initarg :bottom-right :accessor bottom-right)))

(defsubst box-collides-with-point-p (top-left bottom-right pos)
  (with-vec (x1 y1 top-left)
    (with-vec (x2 y2 bottom-right)
      (with-vec (x y pos)
        (and (<= x1 x x2)
             (<= y2 y y1))))))

(define-symmetric collide-p ((a box-collidable-object)
                             (b point-collidable-object))
  (box-collides-with-point-p (top-left a) (bottom-right a) (pos b)))

(define-symmetric collide-p ((a box-collidable-object)
                             (b circle-collidable-object))
  (or (box-collides-with-point-p (top-left a) (bottom-right a) (pos b))
      (with-vec (x1 y1 (top-left a))
        (with-vec (x2 y2 (bottom-right a))
          (let ((tl (top-left a))
                (tr (vec x2 y1))
                (bl (vec x1 y2))
                (br (bottom-right a))
                (cp (pos b))
                (cr (collision-radius b)))
            (or (segment-collides-with-circle-p tl tr cp cr)
                (segment-collides-with-circle-p tl bl cp cr)
                (segment-collides-with-circle-p br tr cp cr)
                (segment-collides-with-circle-p br bl cp cr)))))))


;;;; Game object protocol

(defclass pickable-object (collidable-object)
  ())

(defclass draggable-object (pickable-object)
  ())

(defclass selectable-object (pickable-object)
  ())

(defclass clickable-object (pickable-object)
  ())

(defgeneric update (object)
  (:method (object)
    (declare (ignore object))))

(defgeneric render (object)
  (:method (object)
    (declare (ignore object))))

(defgeneric select (object op pos)
  (:method ((object null) op pos)
    (declare (ignore op pos))))


;;;; Game world

(defparameter *half-world-dimensions* (vec 100.0 100.0))

(defvar *world*)
(defvar *tick*)
(defvar *keys* '())

(defclass world ()
  ((objects-to-delete :initform '() :accessor objects-to-delete)
   (objects :accessor objects)
   (order-table :initform (make-hash-table) :accessor order-table)
   (tick :initform nil :accessor tick)))

(defmethod shared-initialize :after ((world world) slot-names &rest initargs &key test-order render-order update-order hit-test-order &allow-other-keys)
  (declare (ignore slot-names initargs))
  (unless (slot-boundp world 'objects)
    (when (null test-order)
      (setf test-order (list 't)))
    (when (null render-order)
      (setf render-order (reverse test-order)))
    (when (null update-order)
      (setf update-order (copy-list render-order)))
    (when (null hit-test-order)
      (setf hit-test-order (copy-list test-order)))
    (assert (set-equal test-order render-order))
    (assert (set-equal test-order update-order))
    (assert (set-equal test-order hit-test-order))
    (setf (objects world) (make-array (length test-order) :initial-element '()))
    (setf (gethash :render (order-table world))
          (mapcar (lambda (type) (position type test-order)) render-order))
    (setf (gethash :update (order-table world))
          (mapcar (lambda (type) (position type test-order)) update-order))
    (setf (gethash :hit-test (order-table world))
          (mapcar (lambda (type) (position type test-order)) hit-test-order))
    (setf (gethash :test (order-table world)) test-order)))
  
(defmethod reinitialize-instance :before ((world world) &rest initargs)
  (declare (ignore initargs))
  (setf (tick world) nil)
  (clear-objects world))

(defun ensure-world (world-designator)
  (etypecase world-designator
    (symbol (make-instance world-designator))
    (world world-designator)))

(defun add-object (object &optional (world *world*))
  (push object (aref (objects world) (object-list-index object world)))
  (object-got-added object world))

(defun object-list-index (object world)
  (position-if (lambda (type) (typep object type))
               (gethash :test (order-table world))))

(defgeneric object-got-removed (object world)
  (:method (object world)
    (declare (ignore object world))))

(defgeneric object-got-added (object world)
  (:method (object world)
    (declare (ignore object world))))

(defun remove-object (object &optional (world *world*))
  (push object (objects-to-delete world))
  (object-got-removed object world))

(defun expunge-objects (&optional (world *world*))
  (dolist (object (objects-to-delete world))
    (deletef (aref (objects world) (object-list-index object world))
             object :count 1))
  (setf (objects-to-delete world) '()))

(defun clear-objects (&optional (world *world*))
  (dotimes (i (length (objects world)))
    (setf (aref (objects world) i) '()))
  (setf (objects-to-delete world) '()))

(defun map-objects (function world order type)
  (unless (type= type 'nil)
    (flet ((maybe-call-function (object)
             (when (and (typep object type)
                        (not (member object (objects-to-delete world))))
               (funcall function object))))
      (dolist (index (gethash order (order-table world)))
        (mapc #'maybe-call-function (aref (objects world) index))))))

(defmacro do-objects ((object-var &key (world '*world*) (order :hit-test) (type t) collecting) &body forms)
  (if collecting
      (with-gensyms (collection)
        `(let ((,collection '()))
           (flet ((collect (x) (push x ,collection)))
             (block nil
               (map-objects (lambda (,object-var) ,@forms) ,world ,order ,type)))
           ,collection))
      `(block nil
         (map-objects (lambda (,object-var) ,@forms) ,world ,order ,type))))

(defmethod update ((w world))
  (let ((*world* w)
        (*tick* (tick w)))
    (do-objects (object :order :update)
      (update object))
    (expunge-objects)))

(defgeneric render-collision-shape (object))

(defmethod render ((w world))
  (let ((*world* w))
    (do-objects (object :order :render)
      (render object)
      (when (typep object *draw-collision-shape-for-type*)
        (gl:color 1.0 0.0 0.0)
        (render-collision-shape object)))
    (when *draw-tick*
      (gl:color 1.0 1.0 1.0)
      (display-text -98.0 95.0 (tick w)))))

(defmethod render-collision-shape ((object circle-collidable-object))
  (gl:with-pushed-matrix
    (with-vec (x y (pos object))
      (gl:translate x y 0.0))
    (draw-circle (collision-radius object))))

(defmethod render-collision-shape ((object box-collidable-object))
  (gl:with-primitive :line-loop
    (with-vec (x1 y1 (top-left object))
      (with-vec (x2 y2 (bottom-right object))
        (gl:vertex x1 y1)
        (gl:vertex x2 y1)
        (gl:vertex x2 y2)
        (gl:vertex x1 y2)))))

(defmethod render-collision-shape ((object point-collidable-object))
  (gl:with-pushed-matrix
    (with-vec (x y (pos object))
      (gl:translate x y 0.0))
    (draw-circle 1)))


;;;; Game window

(defclass mouse (point-collidable-object)
  ((selection :initform nil :accessor selection))
  (:default-initargs :pos (vec 0.0 0.0)))

(defun pick-object (mouse)
  (do-objects (object :type 'pickable-object)
    (when (collide-p object mouse)
      (return-from pick-object object))))

(defclass game-window (glut:window)
  ((world :accessor world)
   (world-generator :initarg :world-generator :accessor world-generator)
   (time-to-next-tick :initform nil :accessor time-to-next-tick)
   (mouse :initform (make-instance 'mouse) :accessor mouse))
  (:default-initargs
   :name 'game-window
   :width 800 :height 800
   :mode '(:double :rgb)))

(defmethod initialize-instance :after ((w game-window) &rest initargs &key world)
  (declare (ignore initargs))
  (when world
    (setf (world-generator w)
          (let ((worlds (if (listp world)
                            (mapcar #'ensure-world world)
                            (list (ensure-world world)))))
            (lambda (relation)
              (ecase relation
                (:next (pop worlds))
                (:outer nil))))))
  (next-world w))

(defun find-game-window ()
  (glut:find-window 'game-window))

(defun next-world (&optional (w (find-game-window)))
  (generate-world :next w))

(defun outer-world (&optional (w (find-game-window)))
  (generate-world :outer w))

(defun generate-world (relation &optional (w (find-game-window)))
  (when (null (setf (world w) (funcall (world-generator w) relation)))
    (glut:destroy-current-window)))

(defun this-world-again (&optional (w (find-game-window)))
  (reinitialize-instance (world w)))

(defmethod tick ((w game-window))
  (tick (world w)))

(defmethod (setf tick) (new-value (w game-window))
  (setf (tick (world w)) new-value))

(defmethod glut:display-window :before ((w game-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat)
  (gl:disable :depth-test)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defmethod glut:display ((w game-window))
  (gl:load-identity)
  (gl:clear-color 0 0 0 0)
  (gl:clear :color-buffer)
  (gl:translate .375 .375 0.0)
  (render (world w))
  (glut:swap-buffers))

(defmethod glut:reshape ((w game-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (with-vec (x y *half-world-dimensions*)
    (gl:ortho (- x) x (- y) y 0 1))
  (gl:matrix-mode :modelview))

(defmethod glut:keyboard ((w game-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (outer-world w))
    (t (pushnew key *keys*))))

(defmethod glut:keyboard-up ((w game-window) key x y)
  (declare (ignore x y))
  (deletef *keys* key))

(defmethod glut:special ((w game-window) special-key x y)
  (declare (ignore x y))
  (pushnew special-key *keys*))

(defmethod glut:special-up ((w game-window) special-key x y)
  (declare (ignore x y))
  (deletef *keys* special-key))

(defmethod glut:motion ((w game-window) x y)
  (let ((*world* (world w)))
    (multiple-value-bind (x y) (glu:un-project x y 0.0)
      (vec-assign (pos (mouse w)) x (- y)))
    (select (selection (mouse w)) :move (pos (mouse w)))))

(defmethod glut:passive-motion ((w game-window) x y)
  (let ((*world* (world w)))
    (multiple-value-bind (x y) (glu:un-project x y 0.0)
      (vec-assign (pos (mouse w)) x (- y)))))  

(defgeneric left-button (state mouse selected-object picked-object)
  (:method (state mouse selected-object picked-object)
    (declare (ignore state mouse selected-object picked-object))))

(defmethod glut:mouse ((w game-window) button state x y)
  (glut:motion w x y)
  (let ((*world* (world w)))
    (case button
      (:left-button
       (let ((m (mouse w)))
         (left-button state m (selection m) (pick-object m)))))))

(defun obtain-object (object mouse)
  (when (selection mouse)
    (release-object mouse))
  (setf (selection mouse) object)
  (select (selection mouse) :obtain (pos mouse)))

(defun release-object (mouse)
  (when (selection mouse)
    (select (selection mouse) :release (pos mouse))
    (setf (selection mouse) nil)))

(defmethod left-button ((state (eql :down)) mouse (selected-object selectable-object) (picked-object null))
  (release-object mouse))

(defmethod left-button ((state (eql :down)) mouse selected-object (picked-object selectable-object))
  (declare (ignore selected-object))
  (obtain-object picked-object mouse))

(defmethod left-button ((state (eql :down)) mouse selected-object (picked-object draggable-object))
  (declare (ignore selected-object))
  (obtain-object picked-object mouse))

(defmethod left-button ((state (eql :up)) mouse (selected-object draggable-object) picked-object)
  (declare (ignore picked-object))
  (release-object mouse))

(defmethod left-button ((state (eql :down)) mouse selected-object (picked-object clickable-object))
  (declare (ignore selected-object))
  (select picked-object :obtain (pos mouse)))

(defmethod left-button ((state (eql :up)) mouse selected-object (picked-object clickable-object))
  (declare (ignore selected-object))
  (select picked-object :release (pos mouse)))

(defmethod glut:idle ((w game-window))
  (let ((now (glut:get :elapsed-time)))
    (when (null (tick w))
      (setf (tick w) -1)
      (setf (time-to-next-tick w) now))
    (when (>= now (time-to-next-tick w))
      (incf (tick w))
      (setf (time-to-next-tick w) (+ now *tick-duration*))
      (update (world w))
      (glut:post-redisplay))))

(defun display-text (x y object &rest format-args)
  (let ((string (if (stringp object)
                    (apply #'format nil object format-args)
                    (princ-to-string object))))
    (gl:with-pushed-matrix
      (gl:load-identity)
      (gl:raster-pos x y)
      (glut:bitmap-string glut:+bitmap-8-by-13+ string))))

(defmacro define-level (name-and-initargs &body objects)
  (labels ((object-name (object)
             (getf (cdr object) :named))
           (object-class-name (object)
             (car object))
           (object-initargs (object)
             (let ((list (copy-list (cdr object))))
               (remf list :named)
               list)))
    (let ((object-names (loop for object in objects
                              when (object-name object)
                              collect it)))
      (destructuring-bind (name &rest initargs) (ensure-list name-and-initargs)
        `(progn
           (defclass ,name (world)
             ()
             (:default-initargs ,@initargs))
           (defmethod shared-initialize :after ((world ,name) slot-names &rest initargs)
             (declare (ignore slot-names initargs))
             (let ,object-names
               ,@(loop for object in objects
                       for name = (object-name object)
                       for make = `(make-instance ',(object-class-name object) ,@(object-initargs object))
                       collect `(add-object ,(if name `(setf ,name ,make) make) world))))
         ',name)))))
