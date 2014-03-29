#| 
 This file is a part of Piping
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.piping)

(defclass predicate-filter (filter)
  ((%predicate :initarg :predicate :initform (error "Predicate function required.") :accessor predicate))
  (:documentation "Segment that predicate-filters messages according to a predicate function."))

(defmethod pass ((filter predicate-filter) message)
  (when (funcall (predicate filter) message)
    message))

(defclass printer (faucet)
  ((%stream :initarg :stream :initform *standard-output* :accessor print-stream))
  (:documentation "Segment that prints each message it receives."))

(defmethod pass ((printer printer) message)
  (print message (print-stream printer))
  message)

(defclass switch (segmetn)
  ((%value :initarg :value :initform 0 :accessor value)
   (%splitter :initarg :splitter :initform (make-splitter) :accessor splitter))
  (:documentation "A switch that only passes to the pipe as set by its current value."))

(defmethod find-place ((switch switch) place)
  (find-place (splitter switch) place))
(defmethod find-parent ((switch switch) place)
  (find-parent (splitter switch) place))
(defmethod insert (item (switch switch) &optional position)
  (insert item (splitter switch) position))
(defmethod withdraw ((switch switch) &optional position)
  (withdraw (splitter switch) position))

(defmethod pass ((switch switch) message)
  (pass (aref (splitter switch) (value switch)) message))
