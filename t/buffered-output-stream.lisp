(in-package :cl-bodge.tests)


(in-suite :bodge-buffered-output-stream)


(test buffered-output-stream-basic-behavior
  (let ((stream (make-instance 'ge.net::buffered-output-stream
                               :buffer (make-array 5 :element-type '(unsigned-byte 8)
                                                   :initial-contents '(0 1 2 3 4))))
        (seq #(3 2 1)))

    (write-sequence seq stream)
    (is (= 3 (file-position stream)))
    (is (equalp #(3 2 1 3 4) (ge.net::buffer-of stream)))

    (write-sequence seq stream :start 1 :end 2)
    (is (equalp #(3 2 1 2 4) (ge.net::buffer-of stream)))

    (file-position stream 2)
    (write-sequence seq stream)
    (is (equalp #(3 2 3 2 1) (ge.net::buffer-of stream)))

    (file-position stream 4)
    (write-byte 3 stream)
    (is (equalp #(3 2 3 2 3) (ge.net::buffer-of stream)))))
