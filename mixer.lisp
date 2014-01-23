#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :piping)

(defclass mixer (pipe)
  ((mix :initarg :mix :initform #'(lambda (message) message) :accessor mix))
  (:documentation "A pipe segment that actively changes components of the message."))

(defmethod print-self ((mixer mixer) stream)
  (format stream "[** ~:[~;~:*~a~]]" (name mixer))
  mixer)

(defmethod pass ((mixer mixer) message)
  (pass (next mixer) (funcall (mix mixer) message)))
