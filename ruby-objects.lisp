
(in-package :cl-user)

(defpackage :ruby
  (:use :cl)
  (:shadow
   #:class
   #:find-class
   #:find-method)
  (:export
   #:*self*
   #:ruby-class
   #:ruby-object
   #:public-send
   #:public-send*
   #:send
   #:send*))

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
(eval-when (:compile-toplevel)
  (shadow 'find-method))
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

(shadow 'find-method)

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

(eval-when (:compile-toplevel)
  (shadow 'class))

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

(defvar *self*)
(declaim (type ruby-object *self*))

(defun public-send* (method &rest args)
  (let* ((class (ruby-class *self*))
         (fn (find-public-method class method)))
    (unless fn
      (multiple-value-bind (fn type) (find-method class method)
        (if fn
            (error "method is ~A : ~A#~A"
                   type
                   (class-name class)
                   method)
            (error "method missing ~A#~A"
                   (class-name class)
                   method))))
    (apply (the function fn) args)))

(defun send* (method &rest args)
  (let* ((class (ruby-class *self*))
         (fn (find-method class method)))
    (unless fn
      (error "method missing ~A#~A"
             (class-name class)
             method))
    (apply (the function fn) args)))

(defmacro public-send (object method &body args)
  `(let* ((*self* ,object))
     (public-send* ,method ,@args)))

(defmacro send (object method &body args)
  `(let* ((*self* ,object))
     (send* ,method ,@args)))

(defmacro @ (name)
  (let ((name (intern (string-upcase name) :keyword)))
    `(cdr (or (assoc ,name (instance-variables *self*))
              (let ((a (cons ,name nil)))
                (push a (instance-variables *self*))
                a)))))

(class object
  (def public-respond-to? (name)
    (declare (type symbol name))
    (when (find-public-method (ruby-class *self*) name)
      t))
  (def respond-to? (name)
    (declare (type symbol name))
    (when (find-method (ruby-class *self*) name)
      t)))

(class test < object
  (private)
  (def fact (x)
    "This is a recursive implementation of factorial (!)"
    (declare (type integer x))
    (if (< x 1)
        1
        (* x (the integer (send* :fact (1- x))))))
  (def x= (x)
    "setter for x"
    (setf (@ x) x))
  (def x ()
    "getter for x"
    (@ x)))

(defgeneric new (class &rest args))

(defmethod new ((class ruby-class) &rest args)
  (let ((*self* (make-instance 'ruby-object :class class)))
    (when (send* :respond-to? :initialize)
      (apply #'send* :initialize args))
    *self*))

(defmethod new ((class symbol) &rest args)
  (apply #'new (find-ruby-class class) args))

(let ((test (new :test)))
  (format t "~&test fact 5 = ~A~%" (send test :fact 5))
  (format t "test x ~A~%" (send test :x))
  (format t "test x= 5 ~A~%" (send test :x= 5))
  (format t "test x ~A~%" (send test :x)))
