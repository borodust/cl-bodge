(in-package :cl-bodge.tests)


(in-suite :bodge-text)


(test rated-queue-creation
  (let ((queue (ge.text::make-rated-queue)))
    (ge.text::pushrf 314 queue)
    (is (equal 314 (ge.text::rated-value queue)))))


(test rated-queue-unpromoted
  (let ((queue (ge.text::make-rated-queue)))
    (loop for i from 0 below 10
       do (ge.text::pushrf i queue))

    (is (equal '(9 8 7 6 5 4 3 2 1 0)
               (loop for node = (ge.text::poprf queue) until (null node)
                  collecting (ge.text::rated-value node))))))


(test rated-queue-promoted
  (let* ((queue (ge.text::make-rated-queue))
         (nodes (nreverse (loop for i from 0 below 10
                             collecting (ge.text::pushrf i queue)))))
    (sleep 1)
    (ge.text::promotef (first nodes) queue)
    (ge.text::promotef (third nodes) queue)
    (ge.text::promotef (sixth nodes) queue)
    (ge.text::promotef (tenth nodes) queue)

    (is (equal '(8 6 5 3 2 1)
               (loop for i from 0 below 6
                  for node = (ge.text::poprf queue)
                  collecting (ge.text::rated-value node))))))
