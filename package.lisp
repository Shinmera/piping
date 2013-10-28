#|
  This file is a part of Piping
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.piping
  (:nicknames :piping)
  (:use :cl)
  (:export :faucet
           :print-faucet
           :string-stream-faucet
           :file-faucet

           :filter
           :test
           
           :mixer
           :mix

           :pipe
           :connect-next
           :connect-prev
           :disconnect-next
           :disconnect-prev
           :insert-before
           :insert-after
           :remove-before
           :remove-after
           :remove-this
           :replace-this
           :print-flow
           :next
           :prev
           
           :pipeline
           :pipes
           :build-pipeline

           :segment
           :pass
           :print-self

           :source

           :splitter
           :targets
           :connect-new
           :disconnect

           :switch
           :active-switch
           :make-active
           :active

           :valve
           :valve-closed
           :message
           :opened
           :on-closed
           :open-valve
           :close-valve
           :active-valve
           :open-func
           :close-func))
