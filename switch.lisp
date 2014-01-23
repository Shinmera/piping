#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :piping)

(defclass switch (splitter)
  ((active :initarg :active :initform 0 :accessor active))
  (:documentation "A switch connects to multiple pipes but only passes to one."))

(defmethod print-self ((switch switch) stream)
  (format stream "[<\\~a~:[~;~:* ~a~]]" (length (targets switch)) (name switch))
  switch)

(defgeneric make-active (switch segment-or-index &key)
  (:documentation "Makes the given segment or index active."))

(defmethod make-active ((switch switch) segment &key (test #'eql))
  (setf (active switch) (position segment (targets switch) :test test)))

(defmethod make-active ((switch switch) (index integer) &key)
  (assert (< index (length (targets switch))) (index) "Cannot set index to ~a, switch only has ~a targets" index (length (targets switch)))
  (setf (active switch) index))

(defmethod disconnect :after ((switch switch) segment)
  (if (>= (active switch) (length (targets switch)))
      (setf (active switch) (1- (length (targets switch))))))

(defmethod pass ((switch switch) message)
  (pass (nth (active switch) (targets switch)) message))

(defclass active-switch (switch)
  ((switcher :initarg :switcher :initform (constantly NIL) :accessor switcher))
  (:documentation "A switch that can actively change its target pipe depending on the message."))

(defmethod pass :before ((switch active-switch) message)
  (let ((new-index (funcall (switcher switch) switch message)))
    (if new-index (make-active (targets switch) new-index))))
