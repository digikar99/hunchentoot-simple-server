
(defpackage :hunchentoot-simple-server
  (:use :cl :alexandria :hunchentoot)
  (:export :main))

(in-package :hunchentoot-simple-server)

(defmacro expand-opts (&body descriptions)
  "Each description should be of the format
  (name-as-keyword description &optional parser required short).
So that something as short as the following gets the work done.
  (expand-opts
    (:help \"Show this help text.\")
    (:port \"Port number on which to run the server.\" #'parse-integer t)
    (:swank-port \"Port number at which to start swank [default: 8080]\" #'parse-integer)
    (:debug \"Run in debug mode if specified\" #'identity))"
  `(opts:define-opts
     ,@(let* ((used-short-names (make-hash-table))
              (get-unique-short-name
                (lambda (name)
                  (loop for ch across name
                        do (when (and (not (gethash ch used-short-names))
                                      (alpha-char-p ch))
                             (setf (gethash ch used-short-names) t)
                             (return ch))
                        finally
                           (error "Could not find a unique short name for ~D" name)))))
         (loop for description in descriptions
               collect
               (destructuring-bind (name description &optional
                                                       parser required short)
                   description
                 (let ((name-string (string-downcase (symbol-name name))))
                   `(:name ,name
                     :description ,description
                     :short ,(or short (funcall get-unique-short-name
                                                name-string))
                     :required ,required
                     :long ,name-string
                     :arg-parser ,parser)))))))
