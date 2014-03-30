#|
  This file is a part of Piping
  (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.piping.asd
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.lib.piping.asd)

(defsystem piping
  :name "Piping"
  :version "2.0.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A library to enable simple message pipelines."
  :serial T
  :components ((:file "package")
               (:file "array")
               (:file "pipe")
               (:file "pipeline")
               (:file "extra"))
  :depends-on ())
