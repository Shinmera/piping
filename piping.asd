#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.piping.asd
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.lib.piping.asd)

(defsystem piping
  :name "Piping"
  :version "0.0.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A library to enable simple message pipelines."
  :serial T
  :components ((:file "package")
               (:file "segment")
               (:file "pipe")
               (:file "source")
               (:file "valve")
               (:file "faucet"))
  :depends-on (:local-time
               :bordeaux-threads))

(defsystem piping-doc
  :name "Piping Doc"
  :components ((:file "documentation"))
  :depends-on (:piping :lquery-doc))
