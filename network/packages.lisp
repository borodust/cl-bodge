(ge.util:define-package :cl-bodge.network
  (:nicknames :ge.net)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :trivial-gray-streams :claw)
  (:export network-system
           network

           defmessage
           make-message
           field-value
           with-message-fields
           identified-message
           reply-message
           error-message
           ack-message

           message-id
           reply-id
           error-text

           channel
           send-message
           receive-message
           stream-of
           acknowledge
           make-reply-for

           dispatching-channel
           message-flow

           channel-of
           accept-flow
           connect-flow))
