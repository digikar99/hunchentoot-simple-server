(defsystem "hunchentoot-simple-server"
  :depends-on ("hunchentoot"
               "str"
               "alexandria"
               "unix-opts"
               "swank"
               "shasht")
  :components ((:file "utils")
  :serial t
  :components ((:file "package")
               (:file "utils")
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
               #+sbcl (sb-ext:disable-debugger)
               ;; Leave the compression to tar.gz and the likes, since
               ;; storage and file size should not be a problem in 2023.
               ;; Only network can be a problem. Or perhaps, do it anyways.
               (uiop:dump-image "htss" :executable t :compression 22))))
