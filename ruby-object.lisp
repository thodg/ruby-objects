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

#+nil
(defun public-send (object method &rest args)
  (let ((*self* object))
    (apply #'public-send* method args)))

#+nil
(defun send (object method &rest args)
  (let ((*self* object))
     (apply #'send* method args)))

(defmacro @ (name)
  (let ((name (intern (string-upcase name) :keyword)))
    `(cdr (or (assoc ,name (instance-variables *self*))
              (let ((a (cons ,name nil)))
                (push a (instance-variables *self*))
                a)))))
