(defpackage :hunchentoot-simple-server
  (:use :cl :alexandria :hunchentoot)
  (:export :main))

(in-package :hunchentoot-simple-server)
(rename-package :shasht :json)
