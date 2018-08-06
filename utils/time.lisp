(cl:in-package :cl-bodge.utils)


(defun epoch-seconds (&optional (timestamp (now)))
  (+ (timestamp-to-unix timestamp) (/ (nsec-of timestamp) 1000000000)))


(defun real-time-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))


(defun universal-time->epoch (universal)
  (epoch-seconds (local-time:universal-to-timestamp universal)))
