(in-package :hunchentoot-simple-server)
(use-package :alexandria)

(setq hunchentoot:*dispatch-table*
      (list (create-prefix-dispatcher "/hello-world" 'hello-world)
            'dispatch-easy-handlers))

(defun hello-world ()
  "HELLO WORLD")
