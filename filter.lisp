#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :piping)

(defclass filter (pipe)
  ((test :initarg :test :initform (constantly T) :accessor test))
  (:documentation "Pipe segment that filters messages according to a test function."))

(defmethod print-self ((filter filter) stream)
  (format stream "[:: ~:[~;~:*~a~]]" (name filter)))

(defmethod pass ((filter filter) message)
  (if (funcall (test filter) message)
      (pass (next filter) message)))
