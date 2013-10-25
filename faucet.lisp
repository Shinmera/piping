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
  (print-self (prev faucet) stream)
  (print-self faucet stream))

(defmethod connect-next ((faucet faucet) (segment segment))
  (error "Cannot connect a next element to a faucet."))


(defclass print-faucet (faucet) ()
  (:documentation "A faucet that simply prints all the messages."))

(defmethod pass ((faucet print-faucet) message)
  (print message))
