;;;; storage.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.simple-auth)

(defvar *storage*)

;;;; generic interface

(defgeneric storage-check-user-password (storage login password))

(defgeneric storage-check-email-exist (storage email))

(defgeneric storage-check-user-exist (storage login))

(defgeneric storage-create-forgot-mark (storage login-or-email))

;;;; inner interface

(defun check-user-password (login password)
  (storage-check-user-password *storage* login password))

(defun check-email-exist (email)
  (storage-check-email-exist *storage* email))

(defun check-user-exist (login)
  (storage-check-user-exist *storage* login))

(defun create-forgot-mark (email)
  (storage-create-forgot-mark *storage* email))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; storage in memory 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass memory-storage ()
  ((users :initarg :users :initform nil)
   (invites :initform nil)
   (forgots :initform nil)))


(defmethod storage-check-user-password ((storage memory-storage) login password)
  (if (string= (third (find login
                            (slot-value storage 'users)
                            :key #'first
                            :test #'string=))
               password)
      login))

(defmethod storage-check-user-exist ((storage memory-storage) login)
  (find login
        (slot-value storage 'users)
        :key #'first
        :test #'string=))

(defmethod storage-check-email-exist ((storage memory-storage) email)
  (find email
        (slot-value storage 'users)
        :key #'second
        :test #'string-equal))

(defmethod storage-create-forgot-mark ((storage memory-storage) login-or-email)
  (let* ((info (find login-or-email
                     (slot-value storage 'users)
                     :test #'(lambda (x item)
                               (or (string= x (first item))
                                   (string= x (second item))))))
         (mark (if info (calc-sha1-sum (write-to-string info)))))
    (when mark
      (unless (find mark
                    (slot-value storage 'forgots)
                    :test #'string=)
        (push mark
              (slot-value storage 'forgots)))
      (values mark
              (first info)
              (second info)))))


;;;; default init *storage*

(setf *storage*
      (make-instance 'memory-storage
                     :users (list (list "archimag"
                                        "moskvitin@tabris.ru"
                                        (restas.optional:calc-md5-sum "pantera")))))