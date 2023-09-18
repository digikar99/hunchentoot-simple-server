(defpackage #:htss-simple-comments
  (:use :cl :alexandria :hunchentoot :json))

(in-package #:htss-simple-comments)

(defconstant +comment-storage-root+ #P"comments/"
  "The place where comments will be stored.")

(defconstant +comment-api-root+ "/simple-comments/"
  "The root of the API from where the comments will be posted.
The equivalent COMMENT_API_ROOT variable in simple-comments.js also needs to be changed.")

(defconstant +comment-char-limit+ 500
  "The maximum number of characters allowed in the comment.")

(defconstant +disable-captcha+ nil
  "If T, captcha won't be enabled.")

(push (create-prefix-dispatcher +comment-api-root+
                                'simple-comment-dispatch)
      hunchentoot:*dispatch-table*)

(defun simple-comment-dispatch ()
  (let ((request-uri (request-uri*)))
    (assert (str:starts-with? +comment-api-root+ request-uri))
    (let ((simple-comment-uri (subseq request-uri (length +comment-api-root+))))
      (multiple-value-bind (api-uri api-args)
          (let ((end (position #\? simple-comment-uri)))
            (values (subseq simple-comment-uri 0 end)
                    (when end (subseq simple-comment-uri end))))
        (print (list api-uri api-args))
        (funcall (eswitch (api-uri :test #'string=)
                   ("add-comment" 'add-comment)
                   ("view-comment" 'view-comment))
                 api-args)))))

(defun plist-keylist (plist)
  (loop :for item :in plist
        :for i :from 0
        :if (evenp i)
          :collect (intern (string-upcase (cl-slug:slugify item))
                           :keyword)
        :else
          :collect item))

(defun canonicalize-dirname (dirname)
  (let ((namestr (namestring dirname)))
    (print namestr)
    (cond ((or (string= namestr "")
               (string= namestr "/"))
           +comment-storage-root+)
          ((ends-with #\/ namestr)
           dirname)
          (t
           (pathname (uiop:strcat namestr #\/))))))

(defun add-comment (&rest args)
  "Ignore POST-PARAMETERS"
  (declare (ignore args))
  (assert (eq :post (request-method*)))
  (destructuring-bind (&key comment-data page parent name
                       &allow-other-keys)
      (let* ((json:*read-default-object-format* :plist)
             (plist (json:read-json (raw-post-data :force-text t))))
        (plist-keylist plist))
    (let* ((dirname (merge-pathnames (str:join #\/
                                               (mapcar #'cl-slug:slugify
                                                       (str:split #\/ page
                                                                  :omit-nulls t)))
                                     +comment-storage-root+))
           (dirname (canonicalize-dirname dirname))
           (comment-data (sanitize:sanitize comment-data sanitize:*basic-sanitize*)))
      (when comment-data
        (ensure-directories-exist dirname)
        (setq comment-data (if (< +comment-char-limit+ (length comment-data))
                               (subseq comment-data 0 +comment-char-limit+)
                               comment-data))
        (let ((id (merge-pathnames (format nil "~A.txt"
                                           (length (uiop:directory-files
                                                    (namestring dirname))))
                                   dirname)))
          (print (list :here id))
          (with-open-file (f id :direction :output
                                :if-does-not-exist :create)
            (write-line page f)
            (write-line parent f)
            (write-line name f)
            (write-line (write-to-string (local-time:timestamp-to-unix (local-time:now))) f)
            (write-string comment-data f)))))))

(defun comment-file-path (short-file-name)
  (merge-pathnames short-file-name +comment-storage-root+))

(defun comment-file-tree (dirname)
  "Given a dirname, returns a nested alist whose each element is a cons, with
CAR being a comment, and CDR containings its children"
  (ensure-directories-exist dirname)
  (let ((files (uiop:directory-files dirname))
        (comment-node-table (make-hash-table :test #'equal)))
    (print files)
    (labels ((parent-filename (filename)
               (with-open-file (f (comment-file-path filename) :direction :input)
                 (read-line f)
                 (format nil "~A.txt" (parse-integer (read-line f)))))
             (traverse (filename)
               (print filename)
               (cons filename
                     (mapcar #'traverse (gethash filename comment-node-table)))))
      (dolist (fname files)
        (let ((parent (parent-filename fname)))
          (push (file-namestring fname) (gethash parent comment-node-table))))
      (print comment-node-table)
      (force-output)
      (mapcar #'traverse
              (gethash "-1.txt" comment-node-table)))))

(defun html-comment-tree-from-file-tree (dirname comment-file-tree)
  (labels ((id-from-filename (filename)
             (let ((end (position #\. filename)))
               (subseq filename 0 end)))
           (format-comment (filename)
             (with-open-file (f (merge-pathnames filename dirname) :direction :input)
               (let ((id (id-from-filename filename))
                     (page   (read-line f))
                     (parent (read-line f))
                     (name   (read-line f))
                     (datetime (read-line f))
                     (comment (read-stream-content-into-string f)))
                 (declare (ignore page parent))
                 (cl-who:with-html-output-to-string (h nil)
                   (cl-who:htm (:div :class "comment"
                                     :id (format nil "comment-~A" id)
                                     (:div :class "comment-title"
                                           (:span :class "comment-who"
                                                  (cl-who:str name))
                                           " commented on "
                                           (:span :class "comment-datetime"
                                                  (cl-who:str datetime))
                                           ":")
                                     (:div :class "comment-body"
                                           (cl-who:str comment))
                                     (:div :class "comment-reply"
                                           (:button :value "Reply"
                                                    :onclick "enable_reply_box(event)"
                                                    "Reply"))))))))
           (traverse (node)
             (let* ((current  (car node))
                    (children (cdr node))
                    (comment-html  (format-comment current))
                    (children-html (apply #'cl-who:conc
                                          (mapcar #'traverse children))))
               (cl-who:with-html-output-to-string (h nil)
                 (cl-who:htm (:div :class "comment-wrapper"
                                   (cl-who:str comment-html)
                                   (cl-who:str children-html)))))))
    (cl-who:with-html-output-to-string (h nil)
      (:h3 "Comments")
      (cl-who:htm (:div :class "comment-add-comment"
                        (:button :value "Add Comment"
                                 :onclick "enable_toplevel_reply_box(event)"
                                 "Add Comment")))
      (cl-who:str (apply #'cl-who:conc
                         (mapcar #'traverse comment-file-tree))))))

(defun view-comment (&rest args)
  "Given a page, and a comment id, this returns and formats all the comments with that comment as the root. When comment id is -1, then it fetches all the comments of the page.

Rely very much on post-parameters, because we want users to be able to link to specific comments."
  (declare (ignore args))
  (assert (eq :get (request-method*)))
  (destructuring-bind (&key page parent &allow-other-keys)
      (print (plist-keylist (alist-plist (get-parameters*))))
    (let* ((dirname (merge-pathnames (str:join #\/
                                               (mapcar #'cl-slug:slugify
                                                       (str:split #\/ page)))
                                     +comment-storage-root+))
           (dirname (canonicalize-dirname dirname))
           (comment-file-tree (print (comment-file-tree dirname))))
      (print (html-comment-tree-from-file-tree dirname comment-file-tree)))))
