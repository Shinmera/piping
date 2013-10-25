#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage :org.tymoonnext.radiance.lib.piping.doc
  (:use :cl :lquery :lquery-doc)
  (:nicknames :piping-doc)
  (:export :build-documentation))

(in-package :org.tymoonnext.radiance.lib.piping.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr :name (symbol-name (nth 0 object)))))

(defun build-documentation ()
  (write-documentation :piping
                       (merge-pathnames "about-template.html" (asdf:system-source-directory :piping))
                       :output-file (merge-pathnames "about.html" (asdf:system-source-directory :piping))
                       :exclude '(:internal)))
