;;;; storage.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.simple-auth)

(restas:define-policy #:datastore
  (:interface-package #:restas.simple-auth.policy.datastore)
  (:interface-method-template "~A")
  (:internal-function-template "~A")

  (define-method check-user-password (login password))
  (define-method email-exist-p (email))
  (define-method user-exist-p (login))
  (define-method create-invite (login email password))
  (define-method invite-exist-p (invite))
  (define-method create-account (invite))
  (define-method create-forgot-mark (email))
  (define-method forgot-mark-exist-p (mark))
  (define-method change-password (forgot-mark password)))
  
  

;;;; generic interface

;; (defgeneric storage-check-user-password (storage login password))

;; (defgeneric storage-email-exist-p (storage email))

;; (defgeneric storage-user-exist-p (storage login))

;; (defgeneric storage-create-invite (storage login email password))

;; (defgeneric storage-invite-exist-p (storage invite))

;; (defgeneric storage-create-account (storage invite))

;; (defgeneric storage-create-forgot-mark (storage login-or-email))

;; (defgeneric storage-forgot-mark-exist-p (storage mark))

;; (defgeneric storage-change-password (storage forgot-makr password))

;;;; inner interface

;; (defun check-user-password (login password)
;;   (storage-check-user-password *storage* login password))

;; (defun email-exist-p (email)
;;   (storage-email-exist-p *storage* email))

;; (defun user-exist-p (login)
;;   (storage-user-exist-p *storage* login))

;; (defun create-invite (login email password)
;;   (storage-create-invite *storage* login email password))

;; (defun invite-exist-p (invite)
;;   (storage-invite-exist-p *storage* invite))

;; (defun create-account (invite)
;;   (storage-create-account *storage* invite))

;; (defun create-forgot-mark (email)
;;   (storage-create-forgot-mark *storage* email))

;; (defun forgot-mark-exist-p (mark)
;;   (storage-forgot-mark-exist-p *storage* mark))

;; (defun change-passwowrd (forgot-mark password)
;;   (storage-change-password *storage* forgot-mark password))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; storage in memory 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defclass memory-storage ()
;;   ((users :initarg :users :initform nil)
;;    (invites :initform nil)
;;    (forgots :initform nil)))


;; (defmethod storage-check-user-password ((storage memory-storage) login password)
;;   (if (string= (third (find login
;;                             (slot-value storage 'users)
;;                             :key #'first
;;                             :test #'string=))
;;                password)
;;       login))

;; (defmethod storage-user-exist-p ((storage memory-storage) login)
;;   (find login
;;         (slot-value storage 'users)
;;         :key #'first
;;         :test #'string=))

;; (defmethod storage-email-exist-p ((storage memory-storage) email)
;;   (find email
;;         (slot-value storage 'users)
;;         :key #'second
;;         :test #'string-equal))

;; (defmethod storage-create-invite ((storage memory-storage)  login email password)
;;   (let ((invite (calc-sha1-sum (format nil "~A~A~A" login email password))))
;;     (push (list invite
;;                 login
;;                 email
;;                 password)
;;           (slot-value storage
;;                       'invites))
;;     invite))

;; (defmethod storage-invite-exist-p ((storage memory-storage) invite)
;;   (if (assoc invite
;;              (slot-value storage 'invites)
;;              :test #'string=)
;;       t
;;       nil))

;; (defmethod storage-create-account ((storage memory-storage) invite)
;;   (let ((info (assoc invite
;;                      (slot-value storage 'invites)
;;                      :test #'string=)))
;;     (when info
;;       (push (cdr info)
;;             (slot-value storage 'users))
;;       (setf (slot-value storage 'invites)
;;             (delete info
;;                     (slot-value storage 'invites))))
;;     (cdr info)))

;; (defmethod storage-create-forgot-mark ((storage memory-storage) login-or-email)
;;   (let* ((info (find login-or-email
;;                      (slot-value storage 'users)
;;                      :test #'(lambda (x item)
;;                                (or (string= x (first item))
;;                                    (string= x (second item))))))
;;          (mark (if info (calc-sha1-sum (write-to-string info)))))
;;     (when mark
;;       (unless (find mark
;;                     (slot-value storage 'forgots)
;;                     :test #'string=)
;;         (push (cons mark
;;                     info)
;;               (slot-value storage 'forgots)))
;;       (values mark
;;               (first info)
;;               (second info)))))

;; (defmethod storage-forgot-mark-exist-p ((storage memory-storage) mark)
;;   (find mark
;;         (slot-value storage 'forgots)
;;         :key #'car
;;         :test #'string=))

;; (defmethod storage-change-password ((storage memory-storage) forgot-mark password)
;;   (let ((info (find forgot-mark
;;                      (slot-value storage 'forgots)
;;                      :key #'car
;;                      :test #'string=)))
;;     (setf (third (cdr info))
;;           password)
;;     (setf (slot-value storage 'forgots)
;;           (delete info
;;                   (slot-value storage 'forgots)))))


;; ;;;; default init *storage*

;; (setf *storage* (make-instance 'memory-storage))

;; (create-account (create-invite "archimag" "archimag@gmail.com" (calc-md5-sum "123")))

                     
