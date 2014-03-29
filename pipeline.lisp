#| 
 This file is a part of Piping
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.piping)

(defun make-pipe ()
  "Creates a new splitter array."
  (make-array 1 :adjustable T :fill-pointer 0))

(defclass pipeline ()
  ((%pipeline :initarg :pipeline :initform (make-pipe) :accessor pipeline)
   (%names :initarg :names :initform (make-hash-table :test 'equal) :accessor names))
  (:documentation "Base pipeline object that is necessary to orchestrate piping systems."))

(defgeneric find-place (segment place)
  (:documentation "Searches and returns the segment as designated by PLACE.
A place is a list of signed integers, each denoting the position of the item
within the current splitter or segment. Subsequent numbers descend into the
item matched by the previous number.

As such, (1 4) matches the following:
 ([] () [] [])
      \- ([] [] [] [] [] [])
                      ^^")
  (:method ((segment segment) place)
    (if place
        (error "Cannot descend into segment!")
        segment))
  (:method ((array array) (place list))
    (if place
        (find-place (aref array (pop place)) place)
        array))
  (:method ((pipeline pipeline) (place list))
    (find-place (pipeline pipeline) place))
  (:method ((pipeline pipeline) (name string))
    (find-place pipeline (gethash name (names pipeline)))))

(defgeneric find-parent (segment place)
  (:documentation "Same as FIND-PLACE, except it returns the parent of the matched item.
As secondary value it returns the position of the matched item within the parent.")
  (:method ((segment segment) place)
    (declare (ignore place))
    (error "Cannot descend into segment!"))
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

(defgeneric add-segment (pipeline segment &optional place)
  (:documentation "Add a new segment to the pipeline.
If place is set, the pipe is added to the specified place as per INSERT.
The place specified is expected to be a splitter or similar to append
the pipe to.")
  (:method ((pipeline pipeline) (segment segment) &optional place)
    (insert segment (find-place pipeline place)))
  (:method ((pipeline pipeline) (array array) &optional place)
    (insert array (find-place pipeline place)))
  (:method ((pipeline pipeline) (array (eql :pipe)) &optional place)
    (insert (make-pipe) (find-place pipeline place))))

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

(defgeneric insert-segment (pipeline segment place)
  (:documentation "Insert the segment at the given place.
Note that the segment is always inserted into the parent as specified by the place
and found by FIND-PARENT and inserted into the position as per INSERT.")
  (:method ((pipeline pipeline) (segment segment) place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (insert segment parent pos)))
  (:method ((pipeline pipeline) (array array) place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (insert array parent pos)))
  (:method ((pipeline pipeline) (array (eql :pipe)) place)
    (insert-segment pipeline (make-pipe) place)))

(defgeneric replace-segment (pipeline place pipe)
  (:documentation "Replace a place with a pipe.
This happens simply through REMOVE-PLACE and INSERT-PIPE.")
  (:method ((pipeline pipeline) place (segment segment))
    (remove-place pipeline place)
    (insert-segment pipeline segment place))
  (:method ((pipeline pipeline) place (array array))
    (remove-place pipeline place)
    (insert-segment pipeline array place))
  (:method ((pipeline pipeline) place (array (eql :pipe)))
    (replace-segment pipeline place (make-pipe))))

(defgeneric move-place (pipeline place new-place)
  (:documentation "Moves a place to another while preserving names.
This attempts to fix names associated with the place or deeper places
by changing their place as well.")
  (:method ((pipeline pipeline) place new-place)
    (multiple-value-bind (parent pos) (find-parent pipeline place)
      (let ((segment (withdraw parent pos)))
        (insert-segment pipeline segment new-place)))
    (loop for k being the hash-keys of (names pipeline)
          for v being the hash-values of (names pipeline)
          when (and (<= (length place) (length v))
                    (every #'= place v))
            do (set-name pipeline (append place (subseq v (length place))) k))))

(defgeneric set-name (pipeline place name)
  (:documentation "Associates a place with a name so it can be accessed more easily.")
  (:method ((pipeline pipeline) (place list) name)
    (setf (gethash name (names pipeline)) place)))

(defgeneric pass (segment message)
  (:documentation "Pass a message through a segment.

Note for implementors of this method for custom segments:
You are expected to return a message object, which will be
used for subsequent passing down the current segment. If you
return NIL, passing stops. This does not affect passing in
segments on other splitters. Do note that changing the object
itself directly will of course als be reflected at any other
point in the pipeline passing, so muting the passed object
should be avoided wherever possible.")
  (:method ((pipeline pipeline) message)
    (pass (pipeline pipeline) message))
  (:method ((array array) message)
    (loop for item across array
          do (setf message (pass item message))
          while message))
  (:method ((segment segment) message)
    message))
