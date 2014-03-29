#| 
 This file is a part of Piping
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:piping
  (:nicknames #:org.tymoonnext.radiance.lib.piping
              #:org.tymoonnext.piping)
  (:use #:cl)
  ;; extra.lisp
  (:export
   #:filter
   #:filter
   #:printer
   #:print-stream
   #:switch
   #:value
   #:splitter)
  ;; pipe.lisp
  (:export
   #:pipe)
  ;; pipeline.lisp
  (:export
   #:make-splitter
   #:pipeline
   #:pipeline
   #:names
   #:find-place
   #:find-parent
   #:insert
   #:withdraw
   #:add-pipe
   #:remove-place
   #:insert-pipe
   #:replace-pipe
   #:move-place
   #:set-name
   #:pass))
