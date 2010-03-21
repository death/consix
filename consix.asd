;;;; +----------------------------------------------------------------+
;;;; | CONSIX                                             DEATH, 2010 |
;;;; +----------------------------------------------------------------+

;;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:consix
  :depends-on (#:alexandria #:cl-opengl #:cl-glu #:cl-glut)
  :serial t
  :components
  ((:file "packages")
   (:file "gob")
   (:file "consix")))
