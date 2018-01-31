(in-package :cl-bodge.math)

(define-constant +double-infinity+
    (progn
      #+sbcl sb-ext:double-float-positive-infinity
      #+clozure 1D++0
      #+abcl ext:double-float-positive-infinity
      #+allegro excl::*infinity-double*
      #+cmu ext:double-float-positive-infinity
      #+(and ecl (not infinity-not-available)) si:double-float-positive-infinity
      #+lispworks #.(read-from-string "10E999")
      #+scl ext:double-float-positive-infinity
      #-(or sbcl clozure abcl allegro cmu ecl lispworks scl)
      most-positive-double-float)
  :test 'eql)

(define-constant +single-infinity+
    (progn
      #+sbcl sb-ext:single-float-positive-infinity
      #+clozure 1S++0
      #+abcl ext:single-float-positive-infinity
      #+allegro excl::*infinity-single*
      #+cmu ext:single-float-positive-infinity
      #+(and ecl (not infinity-not-available)) si:single-float-positive-infinity
      #+lispworks (coerce infinity$$ 'single-float)
      #+scl ext:single-float-positive-infinity
      #-(or sbcl clozure abcl allegro cmu ecl lispworks scl)
      most-positive-single-float)
  :test 'eql)

(defmethod lerp ((this number) (that number) (f number))
  (+ this (* (- that this) (f f))))
