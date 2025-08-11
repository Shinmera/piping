(defsystem piping
  :name "Piping"
  :version "2.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "A library to enable simple message pipelines."
  :homepage "https://shinmera.com/docs/piping/"
  :bug-tracker "https://shinmera.com/project/piping/issues"
  :source-control (:git "https://shinmera.com/project/piping.git")
  :serial T
  :components ((:file "package")
               (:file "array")
               (:file "pipe")
               (:file "pipeline")
               (:file "extra"))
  :depends-on ())
