;;;; init.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.simple-auth)

(restas:define-initialization (context)
  (restas:context-add-variable context
                               '*user-auth-cipher*
                               (ironclad:make-cipher :blowfish 
                                                     :mode :ecb
                                                     :key (restas:context-symbol-value context
                                                                                       '*cookie-cipher-key*))))
