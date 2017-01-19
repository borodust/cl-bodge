(in-package :cl-bodge.tests)


(in-suite :bodge-poiu)


(test rated-queue-creation
  (let ((queue (ge.poiu::make-rated-queue)))
    (ge.poiu::pushrf 314 queue)
    (is (equal 314 (ge.poiu::rated-value queue)))))


(test rated-queue-unpromoted
  (let ((queue (ge.poiu::make-rated-queue)))
    (loop for i from 0 below 10
       do (ge.poiu::pushrf i queue))

    (is (equal '(9 8 7 6 5 4 3 2 1 0)
               (loop for node = (ge.poiu::poprf queue) until (null node)
                  collecting (ge.poiu::rated-value node))))))


(test rated-queue-promoted
  (let* ((queue (ge.poiu::make-rated-queue))
         (nodes (nreverse (loop for i from 0 below 10
                             collecting (ge.poiu::pushrf i queue)))))
    (sleep 1)
    (ge.poiu::promotef (first nodes) queue)
    (ge.poiu::promotef (third nodes) queue)
    (ge.poiu::promotef (sixth nodes) queue)
    (ge.poiu::promotef (tenth nodes) queue)

    (is (equal '(8 6 5 3 2 1)
               (loop for i from 0 below 6
                  for node = (ge.poiu::poprf queue)
                  collecting (ge.poiu::rated-value node))))))
