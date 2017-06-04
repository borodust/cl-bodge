(in-package :cl-bodge.network)


(define-constant +default-socket-stream-buffer-size (* 15 +max-frame-size+))
(define-constant +default-backlog+ 128)


(declaim (special *stream-read-buffer*
                  *stream-read-buffer-start*
                  *stream-read-buffer-end*))
;;;
;;; Address
;;;
(defhandle ip4-address-handle
    :closeform (free *handle-value*))


(defclass ip4-address (foreign-object)
  ())


(defmethod initialize-instance ((this ip4-address) &key ip port)
  (let* ((addr (alloc '(:struct (%uv:sockaddr-in)))))
    (%uv:ip4-addr ip port addr)
    (call-next-method this :handle (make-ip4-address-handle addr))))


(defun make-ip4-address (ip port)
  (make-instance 'ip4-address :ip ip :port port))


;;;
;;; Sockets
;;;

;;
(defhandle tcp-handle
    :initform (progn (let ((handle (alloc '%uv:tcp-t)))
                       (%uv:tcp-init *loop-handle* handle)
                       handle))
    :closeform (free *handle-value*))


(defclass socket (foreign-object)
  ((read-callbacks :initform (list) :reader socket-read-callbacks-of)
   (connect-callback :initarg :connect-callback :reader socket-connect-callback-of))
  (:default-initargs :handle (make-tcp-handle)))


(defmethod initialize-instance :after ((this socket) &key read-callback)
  (register-read-callback this read-callback))


(definline make-socket (&key on-read on-connect)
  (make-instance 'socket :read-callback on-read :connect-callback on-connect))


(defun register-read-callback (socket on-read)
  (with-slots (read-callbacks) socket
    (when (and on-read (functionp on-read))
      (push on-read read-callbacks))))


(defun remove-read-callback (socket on-read)
  (with-slots (read-callbacks) socket
    (deletef read-callbacks on-read)))


(defun socket-close (socket)
  (%uv:close (handle-value-of socket) (cffi:null-pointer)))


(defcallback allocate-chunk :void ((handle :pointer) (suggested-size %uv:size-t) (buf :pointer))
  (declare (ignore handle suggested-size))
  (c-let ((buf %uv:buf-t :ptr buf))
    (setf (buf :base) (slab-ptr (allocate-slab *allocator*))
          (buf :len) (slab-size-of *allocator*))))


(defcallback read-chunk :void ((stream :pointer) (n-read %uv:ssize-t) (buf :pointer))
  (c-let ((buf %uv:buf-t :ptr buf))
    (unwind-protect
         (progn
           (when (> n-read (slab-size-of *allocator*))
             (error "Insufficient slab buffer"))
           (cond
             ((= n-read %uv:+eof+)
              ;; fixme: do smth useful
              (warn "EOF"))
             ((< n-read 0)
              (%uv:close stream (cffi:null-pointer)))
             ((> n-read 0)
              (socket-read (pointer->socket stream)
                           (slab-buffer (find-slab *allocator* (inhibiting-string-conversion
                                                                (buf :base))))
                           :start 0 :end n-read))))
      (free-slab-by-pointer *allocator* (inhibiting-string-conversion (buf :base))))))


(defcallback on-accept :void ((server-socket-ptr :pointer) (status :int))
  (log:trace "New client connection request received with status ~A" status)
  (c-let ((server-socket %uv:stream-t :ptr server-socket-ptr))
    (when (< status 0)
      (error "New connection error"))
    (let ((server (find-foreign-object *socket-regsitry* server-socket-ptr)))
      (flet ((call-server-callbacks (socket bytes start end)
               (loop for callback in (socket-read-callbacks-of server)
                  do (log-errors (funcall callback socket bytes start end)))))
        (let ((client (make-socket :on-read #'call-server-callbacks)))
          (if (= 0 (%uv:accept server-socket (handle-value-of client)))
              (progn
                (when-let ((connect-callback (socket-connect-callback-of server)))
                  (funcall connect-callback client))
                (%uv:read-start (handle-value-of client)
                                (callback 'allocate-chunk) (callback 'read-chunk))
                (register-socket client))
              (progn
                (%uv:close (handle-value-of client) (cffi:null-pointer))
                (error "Error while accepting client connection"))))))))


(defun socket-accept (server-socket ip port)
  (with-disposable ((addr (make-ip4-address ip port)))
    (let ((sock (handle-value-of server-socket)))
      (%uv:tcp-bind sock (handle-value-of addr) 0)
      (unless (= 0 (%uv:listen sock +default-backlog+ (callback 'on-accept)))
        (error "Listen error"))
      (register-socket server-socket))))


(defun release-write-objects (write-obj)
  (c-let ((buf %uv:buf-t :ptr (write-object-buf write-obj)))
    (free-slab-by-pointer *allocator* (inhibiting-string-conversion (buf :base))))
  (release-object *write-object-pool* write-obj))


(defcallback on-write-finish :void ((req :pointer) (status :int))
  (if (/= status 0)
      (log:error "Error writing to socket. Status: ~A" status)
      (let ((write-obj (find-foreign-object *write-object-registry* req)))
        (release-write-objects write-obj))))


(declaim (ftype (function (socket (simple-array (unsigned-byte 8) (*))
                                  &key (:start fixnum) (:end fixnum))
                          (values fixnum))
                socket-write))
(defun socket-write (socket byte-array &key (start 0) end)
  (let* ((end (if end end (length byte-array)))
         (write-len (- end start)))
    (when (< (slab-size-of *allocator*) write-len)
      (error "Length of subsequence to write is too big. Max length: ~A, actual: ~A"
             (slab-size-of *allocator*) write-len))
    (let ((write-obj (acquire-object *write-object-pool*))
          (slab (allocate-slab *allocator*)))
      (handler-bind ((serious-condition (lambda (e)
                                          (declare (ignore e))
                                          (release-write-objects write-obj))))
        (c-let ((buf-obj %uv:buf-t :ptr (write-object-buf write-obj)))
          (setf (buf-obj :len) write-len
                (buf-obj :base) (slab-ptr slab))
          (replace (slab-buffer slab) byte-array :start2 start :end2 end)
          (%uv:write (write-object-value write-obj) (handle-value-of socket)
                     buf-obj 1 (callback 'on-write-finish)))))
    write-len))


(declaim (ftype (function (socket (simple-array (unsigned-byte 8) (*))
                                  &key (:start fixnum) (:end (or null fixnum)))
                          (values fixnum))
                socket-read))
(defun socket-read (socket byte-array &key (start 0) end)
  (if-let ((read-callbacks (socket-read-callbacks-of socket)))
    (loop for callback in read-callbacks
       do (log-errors
            (funcall callback socket byte-array start (or end (length byte-array))))
       finally (return (- (or end (length byte-array)) start)))
    0))


;;;
;;; Connection
;;;
(defhandle connect-handle
    :initform (calloc '%uv:connect-t)
    :closeform (free *handle-value*))


(defclass connection (foreign-object)
  ((socket :initarg :socket :reader socket-of))
  (:default-initargs :handle (make-connect-handle)))


(definline make-connection (&key on-read on-connect)
  (make-instance 'connection :socket (make-socket :on-read on-read :on-connect on-connect)))


(defcallback on-connect :void ((connect-ptr :pointer) (status :int))
  (log:trace "Connecting to server. Status: ~A" status)
  (c-let ((connection %uv:connect-t :ptr connect-ptr))
    (when (< status 0)
      (error "Connection error"))
    (let ((socket (pointer->socket (connection :handle))))
      (when-let ((connect-callback (socket-connect-callback-of socket)))
        (funcall connect-callback socket)))
    (%uv:read-start (connection :handle) (callback 'allocate-chunk) (callback 'read-chunk))))


(defun connect (connection ip port)
  (with-slots (socket) connection
    (with-disposable ((addr (make-ip4-address ip port)))
      (%uv:tcp-connect (handle-value-of connection) (handle-value-of socket)
                       (handle-value-of addr) (callback 'on-connect))
      (register-socket socket))))


;;;
;;; Socket stream
;;;
(defclass socket-stream (fundamental-binary-input-stream
                         fundamental-binary-output-stream)
  ((socket :initarg :socket :initform (error ":socket missing") :reader socket-of)
   (read-callback :initarg :on-read)
   (self-callback)))


(definline make-socket-stream (socket &key on-read)
  (make-instance 'socket-stream :socket socket :on-read on-read))


(defun read-socket-stream (stream byte-buffer start end)
  (with-slots (read-callback) stream
    (when read-callback
      (let ((*stream-read-buffer* byte-buffer)
            (*stream-read-buffer-start* (or start 0))
            (*stream-read-buffer-end* (or end (length byte-buffer))))
        (funcall read-callback stream)))))


(defmethod initialize-instance :after ((this socket-stream) &key)
  (with-slots (socket self-callback) this
    (flet ((on-read (socket byte-array start end)
             (declare (ignore socket))
             (read-socket-stream this byte-array start end)))
      (setf self-callback #'on-read)
      (register-read-callback socket self-callback))))


(defmethod stream-write-sequence ((stream socket-stream) sequence start end &key)
  (with-slots (socket) stream
    (socket-write socket sequence :start start :end end)
    sequence))


(defmethod stream-write-byte ((stream socket-stream) integer)
  (with-slots (socket) stream
    (let ((stack-vec (make-array 1 :element-type '(unsigned-byte 8)
                                 :initial-element integer)))
      (declare (dynamic-extent stack-vec))
      (socket-write socket stack-vec)
      integer)))


(defmethod stream-read-sequence ((stream socket-stream) sequence start end &key)
  (replace sequence *stream-read-buffer*
           :start1 start :end1 (or end (length sequence))
           :start2 *stream-read-buffer-start* :end2 *stream-read-buffer-end*)
  (+ start (min (- end start) (- *stream-read-buffer-end* *stream-read-buffer-start*))))


(defmethod stream-read-byte ((stream socket-stream))
  (let ((stack-vec (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)))
    (declare (dynamic-extent stack-vec))
    (if (> (read-sequence stack-vec stream) 0)
        (aref stack-vec 0)
        nil)))


(defmethod close ((stream socket-stream) &key abort)
  (declare (ignore abort))
  (with-slots (socket self-callback) stream
    (remove-read-callback socket self-callback)
    (setf socket nil
          self-callback nil)))
