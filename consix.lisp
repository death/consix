;;;; +----------------------------------------------------------------+
;;;; | CONSIX                                             DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:consix)


;;;; Game

(defclass consix-window (game-window)
  ()
  (:default-initargs
   :title "Consix"))

(define-level consix)

(defun game ()
  (glut:display-window
   (make-instance 'consix-window
                  :world (make-instance 'consix))))
