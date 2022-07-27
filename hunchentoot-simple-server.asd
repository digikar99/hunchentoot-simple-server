

(defsystem "hunchentoot-simple-server"
  :depends-on ("hunchentoot"
               "str"
               "alexandria"
               "unix-opts"
               "swank")
  :components ((:file "utils")
               (:file "server"))
  :entry-point "hunchentoot-simple-server:main"
  :perform (program-op (o c)
             (dolist (thread (uiop:symbol-call :bt :all-threads))
               #+sbcl (progn
                        (require :sb-sprof)
                        (require :sb-cltl2)
                        (require :sb-introspect)
                        (require :sb-bsd-sockets)
                        (require :sb-posix))
               (unless (eq thread (uiop:symbol-call :bt :current-thread))
                 (uiop:symbol-call :bt :destroy-thread thread))
               (uiop:dump-image "htss" :executable t))))
