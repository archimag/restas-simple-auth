;;;; packages.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(restas:define-plugin #:restas.simple-auth
  (:use #:cl))

(in-package #:restas.simple-auth)

(defparameter *reCAPTCHA.publick-key* "6LdZjAcAAAAAAGh_MzHcHfJWp6rpI0XUNghGQB1f")

(defparameter *reCAPTCHA.privake-key* "6LdZjAcAAAAAAKJ2GPWTHPh1H1Foc0kyfbwgrFgO")

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/simple-auth.tmpl"
                                                    (asdf:component-pathname (asdf:find-system '#:restas-simple-auth))))

