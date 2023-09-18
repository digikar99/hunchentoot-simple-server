(in-package :hunchentoot-simple-server)

(defvar *acceptor*)

(expand-opts
  (:help "Show this help text")
  (:no-ssl "Start the server without SSL")
  (:server-address "<address>:<port> on which to run the server [default: localhost:8000]"
                   #'identity nil #\s)
  (:swank-port "Port number at which to start swank" #'identity nil #\p)
  (:server-root "The root directory from which to serve files (default: current-directory)" #'identity
                nil #\d))

(defun continue-unless-help (help)
  (when help
    (opts:describe :prefix "Usage: htss [options] additional-server-files"
                   :suffix "additional-server-files: additional lisp files to load")
    (uiop:quit)))

(defun may-be-start-server (server-address no-ssl server-root)
  (format t "Will run the server on ~A~%" server-address)
  (destructuring-bind (address port)
      (split-sequence:split-sequence #\: server-address)
    (setq port (parse-integer port))
    (if no-ssl
        (setq *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                        :document-root server-root
                                        :address address
                                        :port port))
        (setq *acceptor* (make-instance 'hunchentoot:easy-ssl-acceptor
                                        ;; FIXME: Complete SSL
                                        :document-root server-root
                                        :address address
                                        :port port)))
    (start *acceptor*)))

(defun load-additional-files (files)
  (when files
    (loop :for file :in files
          :do (format t "Loading ~A... " file)
              (load file)
              (format t "Done.~%")
              (finish-output))))

(defun wait-on-hunchentoot-thread ()
  (bt:join-thread (find-if (lambda (thread)
                             (str:containsp "hunchentoot-listener" (bt:thread-name thread)))
                           (bt:all-threads))))

(defun may-be-start-swank (swank-port)
  (if swank-port
      (progn
        (format t "Will start swank on ~A~%" swank-port)
        (swank:create-server :port (parse-integer swank-port) :dont-close t))
      (format t "Skipping swank as the port was not supplied~%")) )

(defun main ()
  (handler-case
      (multiple-value-bind (parsed-args server-files) (opts:get-opts)
        (destructuring-bind (&key help
                               (server-address "localhost:8000")
                               swank-port
                               no-ssl
                               (server-root "./"))
            parsed-args

          (continue-unless-help help)
          (may-be-start-server server-address no-ssl server-root)
          (may-be-start-swank swank-port)
          (load-additional-files server-files)
          (wait-on-hunchentoot-thread)))
    (error (e)
      (format *error-output* "~A~%" e)
      (trivial-backtrace:print-backtrace e))))
