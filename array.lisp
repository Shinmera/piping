(in-package #:org.tymoonnext.piping)

(defun array-shift (array &key (n 1) (from 0) (to (length array)) (adjust T) (fill NIL f-p))
  "Helper function that shifts a subset of array elements in either direction for a specified amount.
Optionally also extends the array and fills empty space with a given element.

N      --- The amount to be moved. Can either be positive or negative.
FROM   --- Move region start point.
TO     --- Move region end point.
ADJUST --- Whether to adjust the fill pointer and the array bounds.
FILL   --- If provided, empty spaces created by the move will be filled with this element."
  (when (and adjust (array-has-fill-pointer-p array))
    (unless (array-in-bounds-p array (+ (fill-pointer array) n))
      (adjust-array array (+ (fill-pointer array) n)))
    (incf (fill-pointer array) n))
  (if (< 0 n)
      (progn
        (loop repeat (- to from)
              for cursor downfrom (1- to)
              do (setf (aref array (+ cursor n))
                       (aref array cursor)))
        (when f-p
          (loop repeat n
                for cursor from from below to
                do (setf (aref array cursor) fill))))
      (progn
        (loop repeat (- to from)
              for cursor from (+ from n)
              do (setf (aref array cursor)
                       (aref array (- cursor n))))
        (when f-p
          (loop repeat (- n)
                for cursor downfrom (1- to) to from
                do (setf (aref array cursor) fill)))))
  array)

(defun vector-push-extend-position (element vector position)
  "Pushes the element into the specified position and shifts everything
to the right to make space. This is potentially very costly as all
elements after the given position need to be shifted as per ARRAY-SHIFT."
  (array-shift vector :from position)
  (setf (aref vector position) element)
  (fill-pointer vector))

(defun vector-pop-position (vector position)
  "Pops the element at the given position off the vector and returns it.
This is potentially very costly as all elements after the given position
need to be shifted back as per ARRAY-SHIFT."
  (let ((el (aref vector position)))
    (array-shift vector :n -1 :from (1+ position))
    el))
