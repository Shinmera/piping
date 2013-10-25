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
  :components ((:file "package.lisp")
               (:file "documentation.lisp")
               (:file "segment.lisp")
               (:file "pipe.lisp")
               (:file "source.lisp")
               (:file "valve.lisp")
               (:file "faucet.lisp"))
  :depends-on (:local-time
               :bordeaux-threads))
