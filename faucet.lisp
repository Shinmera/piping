#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :piping)

(defclass faucet (pipe)
  ((next))
  (:documentation "An endpoint for pipe messages to arrive at."))

(defmethod print-self ((faucet faucet) stream)
  (format stream ">>~:[~;~:*~a~]|" (name faucet)))

(defmethod print-object ((faucet faucet) stream)
  (if (prev faucet) (print-self (prev faucet) stream))
  (print-self faucet stream))

(defmethod connect-next ((faucet faucet) (segment segment))
  (error "Cannot connect a next element to a faucet."))

(defmethod disconnect-next ((faucet faucet))
  (error "Cannot disconnect a next element from a faucet."))

(defclass string-stream-faucet (faucet)
  ((stream :initarg :stream :initform (error "Stream required!") :accessor faucet-stream))
  (:documentation "A faucet that prints everything to a string stream."))

(defmethod pass ((faucet string-stream-faucet) message)
  (format (faucet-stream faucet) "~a" message)
  (finish-output (faucet-stream faucet)))

(defclass print-faucet (string-stream-faucet)
  ((stream :initform *standard-output*))
  (:documentation "A faucet that simply prints all the messages to stdout"))

(defclass file-faucet (string-stream-faucet)
  ((file :initarg :file :initform (error "Filepath required!") :accessor file)
   (stream :initform NIL :accessor faucet-stream))
  (:documentation "A faucet that prints everything to file."))

(defmethod initialize-instance ((faucet file-faucet) &rest rest)
  (declare (ignore rest))
  (setf (faucet-stream faucet) (open (file faucet))))
