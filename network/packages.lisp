(cl:in-package :cl-bodge.asdf)


(ge.util:define-package :cl-bodge.network
  (:nicknames :ge.net)
  (:use :cl :cl-bodge.utils :cl-bodge.engine)
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
           reply-for-id
           error-text

           conduit
           send-message
           receive-message
           stream-of
           acknowledge
           make-reply-for

           conduit-of
           accept-flow
           connect-flow))
