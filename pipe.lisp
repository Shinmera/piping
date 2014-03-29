#| 
 This file is a part of Piping
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.piping)

(defclass pipe ()
  () (:documentation "Base pipe class."))

(defmethod print-object ((pipe pipe) stream)
  (format stream "[PIPE]" )
  pipe)
