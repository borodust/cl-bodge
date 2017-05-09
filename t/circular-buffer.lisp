(in-package :cl-bodge.tests)


(in-suite :bodge-circular-buffer)


(define-constant +input-array+
    (make-array 10
                :element-type '(unsigned-byte 8)
                :initial-contents #(0 1 2 3 4 5 6 7 8 9))
  :test #'equalp)


(test circular-buffer-filling-and-reading
  (let ((buf (ge.net::make-circular-buffer 10))
        (result (make-array 10 :element-type '(unsigned-byte 8))))
    (flex:with-input-from-sequence (stream +input-array+)
      (is (equal '(10 0) (multiple-value-list (ge.net::fill-buffer buf stream))))
      (is (equal '(0 0) (multiple-value-list (ge.net::fill-buffer buf stream))))
      (is (equal 10 (ge.net::read-buffer buf result)))
      (is (equal 0 (ge.net::read-buffer buf result))))
    (is (equalp +input-array+ result))))


(test circular-buffer-partial-filling-and-reading
  (let ((buf (ge.net::make-circular-buffer 10))
        (result (make-array 10 :element-type '(unsigned-byte 8))))
    (flex:with-input-from-sequence (stream +input-array+ :end 5)
      (is (equal '(5 5) (multiple-value-list (ge.net::fill-buffer buf stream))))
      (is (equal 3 (ge.net::read-buffer buf result :end 3)))
      (is (equal 1 (ge.net::read-buffer buf result :start 9 :end 10)))
      (is (equalp #(0 1 2 0 0 0 0 0 0 3) result))
      (is (equal 1 (ge.net::buffer-filled-size buf))))
    (flex:with-input-from-sequence (stream +input-array+ :end 9)
      (is (equal '(9 0) (multiple-value-list (ge.net::fill-buffer buf stream))))
      (is (equal 10 (ge.net::read-buffer buf result))))
    (is (equalp #(4 0 1 2 3 4 5 6 7 8) result))))
