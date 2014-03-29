#| 
 This file is a part of Piping
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.piping)

(defun make-splitter ()
  "Creates a new splitter array."
  (make-array 1 :adjustable T :fill-pointer 0))

(defclass pipeline ()
  ((%pipeline :initarg :pipeline :initform (make-splitter) :accessor pipeline)
   (%names :initarg :names :initform (make-hash-table :test 'equal) :accessor names))
  (:documentation "Base pipeline object that is necessary to orchestrate piping systems."))

(defgeneric find-place (pipe place)
  (:documentation "Searches and returns the pipe as designated by PLACE.
A place is a list of signed integers, each denoting the position of the item
within the current splitter or pipe. Subsequent numbers descend into the
item matched by the previous number.

As such, (1 4) matches the following:
 ([] () [] [])
      \- ([] [] [] [] [] [])
                      ^^")
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
  (:documentation "Same as FIND-PLACE, except it returns the parent of the matched item.
As secondary value it returns the position of the matched item within the parent.")
  (:method ((pipe pipe) place)
    (declare (ignore place))
    (error "Cannot descend into pipe!"))
  (:method ((array array) (place list))
    (if (<= (length place) 1)
        (values array (first place))
        (find-parent (aref array (pop place)) place)))
  (:method ((pipeline pipeline) (place list))
    (find-parent (pipeline pipeline) place))
  (:method ((pipeline pipeline) (name string))
    (find-parent pipeline (gethash name (names pipeline)))))

(defgeneric insert (item place &optional position)
  (:documentation "Appends the item to the given place unless a specific position is given.
If position is set, the item is expected to be inserted at the specified position, without
replacing or disturbing any other items.")
  (:method (item (array array) &optional position)
    (if position
        (vector-push-extend-position item array position)
        (vector-push-extend item array))))

(defgeneric withdraw (place &optional position)
  (:documentation "Removes an item from the place, by default from the end unless position is set.
If position is given, it is expected that the specified item is removed without disturbing any
other items and without leaving any empty spaces within the parent.")
  (:method ((array array) &optional position)
    (if position
        (vector-pop-position array position)
        (vector-pop array))))

(defgeneric add-pipe (pipeline pipe &optional place)
  (:documentation "Add a new pipe to the pipeline.
If place is set, the pipe is added to the specified place as per INSERT.
The place specified is expected to be a splitter or similar to append
the pipe to.")
  (:method ((pipeline pipeline) (pipe pipe) &optional place)
    (insert pipe (find-place pipeline place)))
  (:method ((pipeline pipeline) (array array) &optional place)
    (insert array (find-place pipeline place)))
  (:method ((pipeline pipeline) (array (eql :splitter)) &optional place)
    (insert (make-splitter) (find-place pipeline place))))

(defgeneric remove-place (pipeline place)
  (:documentation "Removes the given place (as well as its children, if any).
This also removes any names that either match or go through the specified place.")
  (:method ((pipeline pipeline) place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (withdraw parent pos))
    (loop for k being the hash-keys of (names pipeline)
          for v being the hash-values of (names pipeline)
          when (and (<= (length place) (length v))
                    (every #'= place v))
            do (remhash k (names pipeline)))))

(defgeneric insert-pipe (pipeline pipe place)
  (:documentation "Insert the pipe at the given place.
Note that the pipe is always inserted into the parent as specified by the place
and found by FIND-PARENT and inserted into the position as per INSERT.")
  (:method ((pipeline pipeline) (pipe pipe) place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (insert pipe parent pos)))
  (:method ((pipeline pipeline) (array array) place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (insert array parent pos)))
  (:method ((pipeline pipeline) (array (eql :splitter)) place)
    (insert-pipe pipeline (make-splitter) place)))

(defgeneric replace-pipe (pipeline place pipe)
  (:documentation "Replace a place with a pipe.
This happens simply through REMOVE-PLACE and INSERT-PIPE.")
  (:method ((pipeline pipeline) place (pipe pipe))
    (remove-place pipeline place)
    (insert-pipe pipeline pipe place))
  (:method ((pipeline pipeline) place (array array))
    (remove-place pipeline place)
    (insert-pipe pipeline array place))
  (:method ((pipeline pipeline) place (array (eql :splitter)))
    (replace-pipe pipeline place (make-splitter))))

(defgeneric move-place (pipeline place new-place)
  (:documentation "Moves a place to another while preserving names.
This attempts to fix names associated with the place or deeper places
by changing their place as well.")
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
  (:documentation "Associates a place with a name so it can be accessed more easily.")
  (:method ((pipeline pipeline) (place list) name)
    (setf (gethash name (names pipeline)) place)))

(defgeneric pass (pipe message)
  (:documentation "Pass a message through a pipe.

Note for implementors of this method for custom pipes:
You are expected to return a message object, which will be
used for subsequent passing down the current pipe. If you
return NIL, passing stops. This does not affect passing in
pipes on other splitters.")
  (:method ((pipeline pipeline) message)
    (pass (pipeline pipeline) message))
  (:method ((array array) message)
    (loop for item across array
          do (setf message (pass item message))
          while message))
  (:method ((pipe pipe) message)
    message))
