(in-package :cl-bodge.tests)


(in-suite :bodge-network)

#|
(defmessage this-message (identified-message))


(defmessage that-message (reply-message))


(defmessage another-message (identified-message))


(defclass test-conduit (conduit)
  ((message-received-p :initform nil :accessor message-received-p)))


(defclass server-test-conduit (test-conduit) ())


(defmethod receive-message ((this server-test-conduit) (message this-message))
  (setf (message-received-p this) t)
  (send-message this (make-that-message :for-id (message-id message))))


(defmethod receive-message ((this server-test-conduit) (message another-message))
  (acknowledge message this))


(defclass client-test-conduit (test-conduit)
  ((latch :initarg :latch)))


(defmethod receive-message ((this client-test-conduit) (message that-message))
  (setf (message-received-p message) t))


(defmethod receive-message ((this client-test-conduit) (message ack-message))
  (with-slots (latch) this
    (mt:open-latch latch)))


(test network-communication
  (let* ((latch (mt:make-latch))
         (server (accept "0.0.0.0" 8778
                         (lambda (stream)
                           (make-instance 'server-test-conduit :stream stream))))
         (client (connect "127.0.0.1" 8778
                          (lambda (stream)
                            (make-instance 'client-test-conduit
                                           :stream stream
                                           :latch latch)))))
    (unwind-protect
         (progn
           (send-message (conduit-of client) (make-this-message :id 1))
           (send-message (conduit-of client) (make-another-message :id 3))
           (mt:wait-for-latch latch)
           (is-true (and (message-received-p server)
                         (message-received-p client))))
      (ge.ng:dispose server)
      (ge.ng:dispose client))))
|#
