#|
  This file is a part of Piping
  (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem piping
  :name "Piping"
  :version "2.0.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "A library to enable simple message pipelines."
  :homepage "https://shinmera.github.io/piping/"
  :bug-tracker "https://github.com/Shinmera/piping/issues"
  :source-control (:git "https://github.com/Shinmera/piping.git")
  :serial T
  :components ((:file "package")
               (:file "array")
               (:file "pipe")
               (:file "pipeline")
               (:file "extra"))
  :depends-on ())
