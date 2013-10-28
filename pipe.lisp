#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :piping)

(defgeneric connect-next (pipe segment)
  (:documentation "Connect the pipe's next with the given segment."))

(defgeneric connect-prev (pipe segment)
  (:documentation "Connect the pipe's previous with the given segment."))

(defgeneric disconnect-prev (pipe)
  (:documentation "Disconnect the previous segment."))

(defgeneric disconnect-next (pipe)
  (:documentation "Disconnect the next segment."))

(defgeneric insert-before (pipe pipe-to-insert)
  (:documentation "Insert a new pipe element in between this and the one before."))

(defgeneric insert-after (pipe pipe-to-insert)
  (:documentation "Insert a new pipe element in between this and the one after."))

(defgeneric remove-before (pipe)
  (:documentation "Pop out the element before this one and return it. Tries to retain the pipeline."))

(defgeneric remove-after (pipe)
  (:documentation "Pop out the element after this one and return it. Tries to retain the pipeline."))

(defgeneric remove-this (pipe)
  (:documentation "Pop out this pipe element and return it. Tries to retain the pipeline."))

(defgeneric replace-this (pipe pipe-to-insert)
  (:documentation "Replace this pipe element with the given one and return the old one."))

(defgeneric print-flow (pipe stream)
  (:documentation "Print this and all following pipes as a flow diagram."))

(defclass pipe (segment)
  ((next :initarg :next :initform NIL :accessor next)
   (prev :initarg :prev :initform NIL :accessor prev))
  (:documentation "Base pipe class."))

(defmethod print-object ((pipe pipe) stream)
  (if (prev pipe) (print-self (prev pipe) stream))
  (print-self pipe stream)
  (if (next pipe) (print-self (next pipe) stream)))

(defmethod print-self ((pipe pipe) stream)
  (format stream "[~:[   ~;~:*~a~]]" (name pipe)))

(defmethod initialize-instance :after ((pipe pipe) &rest rest)
  (declare (ignore rest))
  (if (next pipe) (connect-next pipe (next pipe)))
  (if (prev pipe) (connect-prev pipe (prev pipe))))

(defmethod connect-next ((pipe pipe) (segment segment))
  (setf (next pipe) segment)
  pipe)

(defmethod connect-next ((pipe pipe) (pipe-2 pipe))
  (if (next pipe)
      (setf (prev (next pipe)) NIL))
  (setf (next pipe) pipe-2)
  (setf (prev pipe-2) pipe))

(defmethod connect-prev ((pipe pipe) (segment segment))
  (setf (prev pipe) segment)
  pipe)

(defmethod connect-prev ((pipe pipe) (pipe-2 pipe))
  (if (prev pipe)
      (setf (next (prev pipe)) NIL))
  (setf (prev pipe) pipe-2)
  (setf (next pipe-2) pipe))

(defmethod disconnect-next ((pipe pipe))
  (if (subtypep (type-of (next pipe)) 'pipe)
      (setf (prev (next pipe)) NIL))
  (setf (next pipe) NIL)
  pipe)

(defmethod disconnect-prev ((pipe pipe))
  (if (subtypep (type-of (prev pipe)) 'pipe)
      (setf (next (prev pipe)) NIL))
  (setf (prev pipe) NIL)
  pipe)

(defmethod insert-before ((pipe pipe) (pipe2 pipe))
  (connect-next (prev pipe) pipe2)
  (connect-prev pipe pipe2)
  pipe)

(defmethod insert-after ((pipe pipe) (pipe2 pipe))
  (connect-prev (next pipe) pipe2)
  (connect-next pipe pipe2)
  pipe)

(defmethod remove-before ((pipe pipe))
  (let ((prev (prev pipe)))
    (connect-prev pipe (prev prev))
    (setf (prev prev) NIL
          (next prev) NIL)
    prev))

(defmethod remove-after ((pipe pipe))
  (let ((next (next pipe)))
    (connect-next pipe (next next))
    (setf (prev next) NIL
          (next next) NIL)
    next))

(defmethod remove-this ((pipe pipe))
  (connect-next (prev pipe) (next pipe))
  (setf (prev pipe) NIL
        (next pipe) NIL)
  pipe)

(defmethod replace-this ((pipe pipe) (pipe2 pipe))
  (connect-next (prev pipe) pipe2)
  (connect-prev (next pipe) pipe2)
  (setf (prev pipe) NIL
        (next pipe) NIL)
  pipe)

(defmethod pass ((pipe pipe) message)
  (pass (next pipe) message))

(defmethod print-flow ((pipe pipe) stream)
  (print-self pipe stream)
  (if (next pipe)
      (print-flow (next pipe) stream)))
