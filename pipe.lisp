(in-package #:org.tymoonnext.piping)

(defclass segment () ()
  (:documentation "Base segment class."))

(defmethod print-object ((segment segment) stream)
  (format stream "[~a]" (type-of segment))
  segment)

(defclass filter (segment) ()
  (:documentation "Base filter class."))

(defmethod print-object ((filter filter) stream)
  (format stream ":~a:" (type-of filter))
  filter)

(defclass faucet (segment) ()
  (:documentation "Base faucet class."))

(defmethod print-object ((faucet faucet) stream)
  (format stream ">>~a" (type-of faucet))
  faucet)
