#| 
 This file is a part of Piping
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.piping)

(defun make-splitter ()
  (make-array 1 :adjustable T :fill-pointer 0))

(defclass pipeline ()
  ((%pipeline :initarg :pipeline :initform (make-splitter) :accessor pipeline)
   (%names :initarg :names :initform (make-hash-table :test 'equal) :accessor names))
  (:documentation ""))

(defgeneric find-place (pipeline place)
  (:documentation "")
  (:method ((pipeline pipeline) (place list))
    (let ((current (pipeline pipeline)))
      (loop for position in place
            do (setf current (aref current position)))
      current))
  (:method ((pipeline pipeline) (name string))
    (find-place pipeline (gethash name (names pipeline)))))

(defgeneric find-parent (pipeline place)
  (:documentation "")
  (:method ((pipeline pipeline) (place list))
    (let ((parent (pipeline pipeline)))
      (loop for i from 0 below (1- (length place))
            for subset = place then (cdr subset)
            for item = (car subset)
            do (setf parent (aref parent item))
            finally (return (values parent (second subset))))))
  (:method ((pipeline pipeline) (name string))
    (find-parent pipeline (gethash name (names pipeline)))))

(defgeneric add-pipe (pipeline pipe &optional place)
  (:documentation "")
  (:method ((pipeline pipeline) (pipe pipe) &optional place)
    (vector-push-extend pipe (find-place pipeline place)))
  (:method ((pipeline pipeline) (array array) &optional place)
    (vector-push-extend array (find-place pipeline place)))
  (:method ((pipeline pipeline) (array (eql :splitter)) &optional place)
    (vector-push-extend (make-splitter) (find-place pipeline place))))

(defgeneric remove-place (pipeline place)
  (:documentation "")
  (:method ((pipeline pipeline) place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (vector-pop-position parent pos))))

(defgeneric insert-pipe (pipeline pipe place)
  (:documentation "")
  (:method ((pipeline pipeline) (pipe pipe) place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (vector-push-extend-position pipe parent pos)))
  (:method ((pipeline pipeline) (array array) place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (vector-push-extend-position array parent pos)))
  (:method ((pipeline pipeline) (array (eql :splitter)) place)
    (insert-pipe pipeline (make-splitter) place)))

(defgeneric pass (pipe message)
  (:documentation "")
  (:method ((pipeline pipeline) message)
    (pass (pipeline pipeline) message))
  (:method ((array array) message)
    (loop for item across array
          do (setf message (pass item message))
          while message))
  (:method ((pipe pipe) message)
    message))
