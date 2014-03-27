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

(defgeneric find-place (pipe place)
  (:documentation "")
  (:method ((pipe pipe) place)
    (if place
        (error "Cannot descend into pipe!")
        pipe))
  (:method ((array array) (place list))
    (if place
        (find-place (aref array (pop place)) place)
        array))
  (:method ((pipeline pipeline) (place list))
    (find-place (pipeline pipeline) place))
  (:method ((pipeline pipeline) (name string))
    (find-place pipeline (gethash name (names pipeline)))))

(defgeneric find-parent (pipe place)
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

(defgeneric insert (item place &optional position)
  (:documentation "")
  (:method (item (array array) &optional position)
    (if position
        (vector-push-extend-position item array position)
        (vector-push-extend item array))))

(defgeneric withdraw (place &optional position)
  (:documentation "")
  (:method ((array array) &optional position)
    (if position
        (vector-pop-position array position)
        (vector-pop array))))

(defgeneric add-pipe (pipeline pipe &optional place)
  (:documentation "")
  (:method ((pipeline pipeline) (pipe pipe) &optional place)
    (insert pipe (find-place pipeline place)))
  (:method ((pipeline pipeline) (array array) &optional place)
    (insert array (find-place pipeline place)))
  (:method ((pipeline pipeline) (array (eql :splitter)) &optional place)
    (insert (make-splitter) (find-place pipeline place))))

(defgeneric remove-place (pipeline place)
  (:documentation "")
  (:method ((pipeline pipeline) place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (withdraw parent pos))
    (loop for k being the hash-keys of (names pipeline)
          for v being the hash-values of (names pipeline)
          when (and (<= (length place) (length v))
                    (every #'= place v))
            do (remhash k (names pipeline)))))

(defgeneric insert-pipe (pipeline pipe place)
  (:documentation "")
  (:method ((pipeline pipeline) (pipe pipe) place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (insert pipe parent pos)))
  (:method ((pipeline pipeline) (array array) place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (insert array parent pos)))
  (:method ((pipeline pipeline) (array (eql :splitter)) place)
    (insert-pipe pipeline (make-splitter) place)))

(defgeneric replace-pipe (pipeline place pipe)
  (:documentation "")
  (:method ((pipeline pipeline) place (pipe pipe))
    (remove-place pipeline place)
    (insert-pipe pipeline pipe place))
  (:method ((pipeline pipeline) place (array array))
    (remove-place pipeline place)
    (insert-pipe pipeline array place))
  (:method ((pipeline pipeline) place (array (eql :splitter)))
    (replace-pipe pipeline place (make-splitter))))

(defgeneric move-place (pipeline place new-place)
  (:documentation "")
  (:method ((pipeline pipeline) place new-place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (let ((pipe (withdraw parent pos)))
        (insert-pipe pipeline pipe new-place)))
    (loop for k being the hash-keys of (names pipeline)
          for v being the hash-values of (names pipeline)
          when (and (<= (length place) (length v))
                    (every #'= place v))
            do (set-name pipeline (append place (subseq v (length place))) k))))

(defgeneric set-name (pipeline place name)
  (:documentation "")
  (:method ((pipeline pipeline) (place list) name)
    (setf (gethash name (names pipeline)) place)))

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
