#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :piping)

(defclass segment ()
  ((name :initarg :name :initform NIL :accessor name))
  (:documentation "Base segment class."))

(defgeneric print-self (segement stream)
  (:documentation "Print a representation of itself without connecting segments."))

(defgeneric pass (segment message)
  (:documentation "Pass a message through the segment."))

(defmethod print-object ((segment segment) stream)
  (print-self segment stream))

(defmethod print-self ((segment segment) stream)
  (format stream "|~:[   ~;~a~]|" (name segment)))

(defmethod pass ((segment segment) message)
  NIL)
