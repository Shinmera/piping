#| 
 This file is a part of Piping
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.piping)

(defclass filter (pipe)
  ((%filter :initarg :filter :initform (error "Filter required.") :accessor filter)))

(defmethod pass ((filter filter) message)
  (when (funcall (filter filter) message)
    message))

(defclass printer (pipe)
  ((%stream :initarg :stream :initform *standard-output* :accessor print-stream)))

(defmethod pass ((printer printer) message)
  (print message (print-stream printer))
  message)
