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
  (setf (next pipe) segment))

(defmethod connect-next ((pipe pipe) (pipe-2 pipe))
  (setf (next pipe) pipe-2)
  (setf (prev pipe-2) pipe))

(defmethod connect-prev ((pipe pipe) (segment segment))
  (setf (prev pipe) segment))

(defmethod connect-prev ((pipe pipe) (pipe-2 pipe))
  (setf (prev pipe) pipe-2)
  (setf (next pipe-2) pipe))

(defmethod pass ((pipe pipe) message)
  (pass (next pipe) message))

(defmethod print-flow ((pipe pipe) stream)
  (print-self pipe stream)
  (if (next pipe)
      (print-flow (next pipe) stream)))
