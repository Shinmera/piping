#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :piping)

(defclass splitter (pipe)
  ((next)
   (targets :initarg :targets :initform () :accessor targets))
  (:documentation "Splits the messages up and passes them to multiple targets."))

(defmethod print-self ((splitter splitter) stream)
  (format stream "[<~a ~:[~;~:*~a~]]" (length (targets splitter)) (name splitter) stream))

(defmethod print-object ((splitter splitter) stream)
  (if (prev splitter) (print-self (prev splitter) stream))
  (print-self splitter stream)
  (format stream "((~{~a~^,  ~}))" (targets splitter)))

(defmethod connect-next ((splitter splitter) (segment segment))
  (error "Splitters cannot be connected to a single next element."))

(defmethod disconnect-next ((splitter splitter))
  (error "Splitters cannot be connected to a single next element."))

(defgeneric connect-new (splitter segment)
  (:documentation "Connect a new element to the splitter."))

(defmethod connect-new ((splitter splitter) (segment segment))
  (pushnew segment (targets splitter)))

(defmethod connect-new ((splitter splitter) (pipe pipe))
  (pushnew pipe (targets splitter))
  (connect-prev pipe splitter))

(defgeneric disconnect (splitter segment)
  (:documentation "Disconnect an existing element from the splitter."))

(defmethod disconnect ((splitter splitter) (segment segment))
  (setf (targets splitter) (delete segment (targets splitter))))

(defmethod disconnect ((splitter splitter) (pipe pipe))
  (setf (targets splitter) (delete pipe (targets splitter)))
  (disconnect-prev pipe))

(defmethod pass ((splitter splitter) message)
  (dolist (segment (targets splitter))
    (pass segment message)))
