;;;; storage.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.simple-auth)

(defvar *storage* nil)

;;;; generic interface

(defgeneric check-user-password (storage login password))

(defgeneric check-email-exist (storage email))

(defgeneric check-user-exist (storage login))