(cl:in-package :cl-bodge.distribution.build)

(ge.rsc:unmount-all)

(flet ((load-engine-assets ()
         (ge.rsc:mount-container "/_engine/"
                                 (ge.ng:merge-working-pathname (bodge-asset-path))
                                 "/_engine/")))
  (pushnew #'load-engine-assets ge.ng:*engine-startup-hooks*))
