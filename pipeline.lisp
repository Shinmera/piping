#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :piping)

(defclass pipeline ()
  ((pipes :initform (make-hash-table :test 'equal) :accessor pipes)
   (source :initarg :source :initform (error "Source required.") :accessor source))
  (:documentation "A class to hold easy access to pipe segments and the pipe system as a whole."))

(defmethod initialize-instance :after ((pipeline pipeline) &rest rest)
  (declare (ignore rest))
  (%consider (source pipeline) (pipes pipeline)))

(defun %enter (pipe table)
  (if (name pipe)
      (progn
        (if (gethash (name pipe) table)
            (format T "Warning: ~a is already in the table!" (name pipe)))
        (setf (gethash (name pipe) table) pipe))))

(defgeneric %consider (pipe table))

(defmethod %consider ((pipe pipe) table)
  (%enter pipe table)
  (%consider (next pipe) table))

(defmethod %consider ((pipe faucet) table)
  (%enter pipe table))

(defmethod %consider ((pipe splitter) table)
  (%enter pipe table)
  (dolist (next (targets pipe))
    (%consider next table)))

(defmacro build-pipeline (source &rest pipes)
  (labels ((fun (pipes)
             (when pipes
               (let ((type (car pipes))
                     (args NIL))
                 (when (listp type)
                   (if (stringp (second type))
                       (setf args (append `(:name ,(cdr type)) (cddr type)))
                       (setf args (cdr type)))
                   (setf type (car type)))
                 `(make-instance ',type ,@args :next ,(fun (cdr pipes)))))))
    `(progn (connect-next ,source ,(fun pipes))
            (make-instance 'pipeline :source ,source))))

(defmethod print-flow ((pipeline pipeline) stream)
  (print-flow (source pipeline) stream))

(defmethod pass ((pipeline pipeline) message)
  (pass (source pipeline) message))
