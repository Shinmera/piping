#|
 This file is a part of Piping
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.piping.doc
  (:use #:cl #:lquery #:lquery-doc)
  (:nicknames #:piping-doc)
  (:export #:build-documentation))

(in-package #:org.tymoonnext.piping.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr :name (symbol-name (nth 0 object))))
  ($ template "h3 a" (attr :href (format NIL "#~a" (symbol-name (nth 0 object))))))

(defun build-documentation ()
  ($ (initialize (merge-pathnames "about-template.html" (asdf:system-source-directory :piping))))
  (let ((template ($ "#template")))
    (let ((nodes (lquery-doc::documentate template :piping :exclude '(:internal :method))))
      ($ "#docs" (empty) (append nodes))))
  ($ (write-to-file (merge-pathnames "about.html" (asdf:system-source-directory :piping)))))

