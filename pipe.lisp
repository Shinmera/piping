#| 
 This file is a part of Piping
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.piping)

(defclass segment () ()
  (:documentation "Base segment class."))

(defmethod print-object ((segment segment) stream)
  (format stream "[PIPE]" )
  segment)

(defclass filter (segment) ()
  (:documentation "Base filter class."))

(defmethod print-object ((filter filter) stream)
  (format stream ":FILTER:")
  filter)

(defclass faucet (segment) ()
  (:documentation "Base faucet class."))

(defmethod print-object ((faucet faucet) stream)
  (format stream ">>FAUCET")
  faucet)
