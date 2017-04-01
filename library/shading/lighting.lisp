(in-package :cl-bodge.library.shading)


(defclass light-source () ())
(defgeneric apply-light-source (light-source object))


(defclass directional-light-source (light-source)
  ((direction :initarg :direction :initform (error ":direction missing") :type vec3
              :reader direction-of)
   (ambient :initarg :ambient-color :initform (error ":ambient-color missing") :type vec4)
   (diffuse :initarg :diffuse-color :initform (error ":diffuse-color missing") :type vec4)
   (prefix :initarg :prefix :initform nil :type (or null string))
   (direction-uniform-name :initform "" :type string)
   (ambient-uniform-name :initform "" :type string)
   (diffuse-uniform-name :initform "" :type string)))


(defmethod initialize-instance :after ((this directional-light-source) &key)
  (with-slots (prefix direction-uniform-name ambient-uniform-name diffuse-uniform-name)  this
    (flet ((prefixify (string)
             (if (or (null prefix) (equal "" prefix))
                 string
                 (concatenate 'string prefix "." string))))
      (setf direction-uniform-name (prefixify "direction")
            ambient-uniform-name (prefixify "ambient")
            diffuse-uniform-name (prefixify "diffuse")))))


(defmethod apply-light-source ((this directional-light-source) (applier function))
  (with-slots (direction ambient diffuse
                         direction-uniform-name ambient-uniform-name diffuse-uniform-name)
      this
    (funcall applier direction-uniform-name direction)
    (funcall applier ambient-uniform-name ambient)
    (funcall applier diffuse-uniform-name diffuse)))


(definline make-directional-light-source (direction ambient-color diffuse-color
                                                    &optional parameter-prefix)
  (make-instance 'directional-light-source
                 :direction direction
                 :ambient-color ambient-color
                 :diffuse-color diffuse-color
                 :prefix parameter-prefix))


(defclass point-light-source (light-source) ())
(defclass spotlight-source (light-source) ())
