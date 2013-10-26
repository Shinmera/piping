#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :piping)

(define-condition valve-closed ()
  ((valve :initarg :valve :accessor valve)
   (message :initarg :message :accessor message))
  (:documentation "Condition signalled when a message arrives at a closed valve."))

(defclass valve (pipe)
  ((opened :initarg :opened :initform T :accessor opened)
   (on-closed :initarg :on-closed :initform :DO-NOTHING :accessor on-closed))
  (:documentation "Valves allow you to regulate the passing process."))

(defmethod print-self ((valve valve) stream)
  (format stream "[~:[|~;-~] ~:[ ~;~:*~a~]]" (opened valve) (name valve)))

(defmethod pass ((valve valve) message)
  (if (opened valve)
      (pass (next valve) message)
      (case (on-closed valve)
        (:DO-NOTHING NIL)
        (:ERROR (error 'valve-closed :message message :valve valve))
        (T (pass (next valve) (on-closed valve))))))

(defmethod open-valve ((valve valve) &optional (status T))
  (setf (opened valve) status))

(defmethod close-valve ((valve valve) &optional (status NIL))
  (setf (opened valve) status))

(defclass active-valve (valve)
  ((open-func :initarg :open-func :initform (constantly NIL) :accessor open-func)
   (close-func :initarg :close-func :initform (constantly NIL) :accessor close-func)) 
  (:documentation "An active valve that closes or opens depending on specific messages."))

(defmethod pass :before ((valve active-valve) message)
  (if (funcall (open-func valve) message)
      (open-valve valve))
  (if (funcall (close-func valve) message)
      (close-valve valve)))
