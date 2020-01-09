;;
;;  ruby-objects  -  Ruby objects for Common Lisp
;;
;;  Copyright 2019,2020 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :ruby)

(defclass ruby-object ()
  ((class :initarg :class
          :accessor ruby-class
          :type ruby-class)
   (instance-variables :initform nil
                       :accessor instance-variables
                       :type list)))

(defclass ruby-class (ruby-object)
  ((name :reader class-name
         :initarg :name
         :type symbol)
   (super-class :initform nil
                :initarg :super-class
                :accessor super-class
                :type (or null ruby-class))
   (direct-public-methods :initform nil
                          :accessor direct-public-methods
                          :type list)
   (direct-private-methods :initform nil
                           :accessor direct-private-methods
                           :type list)
   (direct-protected-methods :initform nil
                             :accessor direct-protected-methods
                             :type list)
   (class-variables :initform nil
                    :accessor class-variables)))

(defgeneric def-public-method (class name fn))
(defgeneric def-private-method (class name fn))
(defgeneric def-protected-method (class name fn))

(defgeneric find-public-method (class name))
(defgeneric find-method (class name))

(defmethod def-public-method ((class ruby-class)
                              (name symbol)
                              (fn function))
  (let ((found (assoc name (direct-public-methods class))))
    (cond
      (found (setf (cdr found) fn)
             (warn "Redefining ~A#~A" (class-name class) name))
      (t (push (cons name fn) (direct-public-methods class)))))
  name)

(defmethod def-private-method ((class ruby-class)
                               (name symbol)
                               (fn function))
  (let ((found (assoc name (direct-private-methods class))))
    (cond
      (found (setf (cdr found) fn)
             (warn "Redefining ~A#~A" (class-name class) name))
      (t (push (cons name fn) (direct-private-methods class)))))
  name)

(defmethod def-protected-method ((class ruby-class)
                                 (name symbol)
                                 (fn function))
  (let ((found (assoc name (direct-protected-methods class))))
    (cond
      (found (setf (cdr found) fn)
             (warn "Redefining ~A#~A" (class-name class) name))
      (t (push (cons name fn) (direct-protected-methods class)))))
  name)

(defmethod find-public-method ((class ruby-class)
                               (name symbol))
  (let ((found (assoc name (direct-public-methods class))))
    (if found
        (cdr found)
        (let ((super (super-class class)))
          (when super
            (find-public-method super name))))))

(defmethod find-method ((class ruby-class)
                        (name symbol))
  (let ((found (assoc name (direct-public-methods class))))
    (cond (found (values (cdr found) :public))
          (t (setq found (assoc name (direct-protected-methods class)))
             (cond (found (values (cdr found) :protected))
                   (t (setq found (assoc name (direct-private-methods class)))
                      (cond (found (values (cdr found) :private))
                            (t (let ((super (super-class class)))
                                 (when super
                                   (find-method super name)))))))))))

(defparameter *ruby-classes-by-name*
  (make-hash-table :test 'eq))

(defmacro find-ruby-class (name)
  "NAME: a keyword"
  `(gethash ,name *ruby-classes-by-name*))

(let* ((object (make-instance 'ruby-class :name :object))
       (module (make-instance 'ruby-class :name :module :super-class object))
       (class (make-instance 'ruby-class :name :class :super-class module)))
  (setf (ruby-class object) class
        (ruby-class module) class
        (ruby-class class) class
        (find-ruby-class :object) object
        (find-ruby-class :module) module
        (find-ruby-class :class) class))

(defun find-ruby-class! (name)
  (declare (type symbol name))
  (or (find-ruby-class name)
      (error "Ruby class not found: ~A" name)))

(defvar *def*)
(declaim (type function *def*))

(defmacro class (name &body body)
  (let ((super (when (eq '< (first body))
                 (pop body)
                 (intern (string-upcase (pop body)) :keyword)))
        (class (gensym "CLASS-"))
        (name (intern (string-upcase name) :keyword)))
    `(let ((,class (or (let ((class (find-ruby-class ',name)))
                         (when class
                           (format *error-output*
                                   "~&Note: reopening class ~A~%"
                                   ',name)
                           class))
                       (make-instance 'ruby-class
                                      :class (find-ruby-class :class)
                                      :super-class (find-ruby-class :object)
                                      :name ',name))))
       (when ,super
         (setf (super-class ,class)
               (find-ruby-class! ,super)))
       (let ((*def* #'def-public-method)
             (*self* ,class))
         ,@body)
       (setf (find-ruby-class ',name) ,class)
       ',name)))

(defun public ()
  (setf *def* #'def-public-method))

(defun protected ()
  (setf *def* #'def-protected-method))

(defun private ()
  (setf *def* #'def-private-method))

(defmacro def (name args &body body)
  (let ((name (intern (string-upcase name) :keyword))
        (doc (when (stringp (first body)) (pop body))))
    `(labels ((,name ,args
                ,@(when doc `(,doc))
                ,@body))
       (funcall (the function *def*) *self* ',name #',name))))

(defgeneric new (class &rest args))

(defmethod new ((class ruby-class) &rest args)
  (let ((*self* (make-instance 'ruby-object :class class)))
    (when (send* :respond-to? :initialize)
      (apply #'send* :initialize args))
    *self*))

(defmethod new ((class symbol) &rest args)
  (apply #'new (find-ruby-class class) args))
