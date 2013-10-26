#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :piping)

(defclass source (pipe)
  ((prev))
  (:documentation "A source for the pipe system that feeds messages down the pipeline."))

(defmethod print-object ((source source) stream)
  (print-self source stream)
  (print-self (next source) stream))

(defmethod print-self ((source source) stream)
  (format stream "|~:[~;~:*~a~]>>" (name source)))

(defmethod connect-prev ((source source) (segment segment))
  (error "Cannot connect a previous element to a source."))

(defmethod disconnect-prev ((source source))
  (error "Cannot disconnect a previous element from a source."))
