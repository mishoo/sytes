;;;; sytes.lisp

(in-package #:sytes)

(export '(register-syte
          unregister-syte
          syte-request-handler
          reset-caches))

;;; "sytes" goes here. Hacks and glory await!

(defparameter *syte-names* (make-hash-table :test #'equal))
(defparameter *current-syte* nil)
(defparameter *syte-toplevel-context* (tmpl:make-context :name "SYTES"))

(defun reset-caches ()
  (tmpl:clear-cache))

(defmacro report-time-spent (name &body body)
  (let ((t1 (gensym)))
    `(let ((,t1 (get-internal-real-time)))
       (prog1
           (progn ,@body)
         (tbnl:log-message* :TIME "~A: ~3$s"
                            ,name
                            (/ (- (get-internal-real-time) ,t1)
                               internal-time-units-per-second))))))

(with-open-file (in (merge-pathnames "template/instance.syt"
                                     (asdf:component-pathname
                                      (asdf:find-system :sytes))))
  (funcall (tmpl::compile (tmpl::parse in) :context *syte-toplevel-context*)
           *syte-toplevel-context*))

(defclass syte ()
  ((names :accessor syte-names
          :initarg :names)
   (root :accessor syte-root
         :initarg :root)
   (context :accessor syte-context
            :initarg :context))
  (:default-initargs
   :names (error "Syte names are missing")
    :root (error "Syte root is missing")))

(defun maybe-exec-boot (syte &key
                               (boot ".boot.syt")
                               (root (syte-root syte)))
  (let ((boot (merge-pathnames boot root)))
    (when (probe-file boot)
      (let ((ctx (syte-context syte)))
        (multiple-value-bind (tmpl was-cached)
            (tmpl::compile-file boot
                                :parent-context ctx
                                :sub-context ctx)
          (unless was-cached
            (funcall (tmpl:template-function tmpl) ctx)))))))

(defmethod initialize-instance :after ((syte syte) &key names root context &allow-other-keys)
  (unless context
    (setf root
          (setf (syte-root syte) (fad:pathname-as-directory (truename root))))
    (setf context
          (setf (syte-context syte) (tmpl:make-context :name (car names) :root root :parent *syte-toplevel-context*)))))

(defun register-syte (syte)
  (loop for name in (syte-names syte)
     do (setf (gethash name *syte-names*) syte)))

(defun unregister-syte (syte)
  (loop for name in (syte-names syte)
     do (remhash name *syte-names*)))

(defun url-to-file (file &optional (syte *current-syte*))
  (let* ((root (syte-root syte))
         (pos (position-if-not (lambda (x) (char= x #\/)) file)))
    (if pos
        (setf file (subseq file pos))
        (setf file ""))
    (setf file (merge-pathnames file root))
    (cond
      ((fad:directory-exists-p file)
       (setf file (fad:pathname-as-directory file)
             file (merge-pathnames "index.syt" file)))
      (t
       (when (fad:directory-pathname-p file)
         (setf file (fad:pathname-as-file file)))
       (unless (probe-file file)
         (setf file (make-pathname :defaults file :type "syt")))))
    file))

(defgeneric syte-request-handler (syte request)
  (:method ((syte (eql nil)) request)
    (format nil "No syte defined for host ~A" (hostname request)))
  (:method ((syte syte) request)
    (maybe-exec-boot syte)
    ;; process the requested file
    (let* ((script (tbnl:script-name request))
           (file script)
           (root (syte-root syte))
           (pos (position-if-not (lambda (x) (char= x #\/)) file)))
      (if pos
          (setf file (subseq file pos))
          (setf file ""))
      (setf file (merge-pathnames file root))
      (cond
        ((fad:directory-exists-p file)
         (if (fad:directory-pathname-p file)
             (setf file (merge-pathnames "index.syt" file))
             (tbnl:redirect (format nil "~A/" script))))
        (t
         (when (fad:directory-pathname-p file)
           (setf file (fad:pathname-as-file file)))
         (unless (probe-file file)
           (setf file (make-pathname :defaults file :type "syt")))))
      (or (tmpl:exec-template-request file (syte-root syte) (syte-context syte))
          (setf (tbnl:return-code*) tbnl:+http-not-found+)))))

(defmethod syte-request-handler :around ((syte syte) request)
  (report-time-spent (format nil "~A/~A"
                             (syte-names syte)
                             (tbnl:script-name request))
    (call-next-method)))

;;; hunchentoot stuff

(defclass sytes-request (tbnl:request)
  ())

(defclass sytes-acceptor (tbnl:acceptor)
  ()
  (:default-initargs
   :error-template-directory nil
   :access-log-destination "/tmp/sytes.log"
    :message-log-destination "/tmp/sytes.messages"
    :port 7379
    :request-class 'sytes-request))

(defparameter *acceptor* nil)

(defun start-server (&key (port 7379))
  (setf *acceptor* (make-instance 'sytes-acceptor :port port))
  (tbnl:start *acceptor*))

(defun stop-server ()
  (tbnl:stop *acceptor*)
  (setf *acceptor* nil))

(defgeneric hostname (request)
  (:method ((request sytes-request))
    (let* ((host (tbnl:host request))
           (split (split-sequence #\: host)))
      (car split))))

(defmethod tbnl:acceptor-dispatch-request ((acceptor sytes-acceptor) request)
  (setf (tbnl:content-type*) "text/html; charset=UTF-8")
  ;; (tbnl:start-session)
  (let* ((host (hostname request))
         (syte (gethash host *syte-names*))
         (*current-syte* syte))
    (syte-request-handler syte request)))

(defmethod tbnl:acceptor-status-message ((acceptor sytes-acceptor) code &key &allow-other-keys)
  (unless (= code 404)
    (call-next-method)))


;;; some more primitives

(tmpl:def-primitive "require"
    (lambda (name)
      (let* ((current tmpl:*current-template*)
             (filename (tmpl:template-filename current))
             (tmpl (tmpl:compile-file (merge-pathnames name filename)
                                      :parent-context (syte-context *current-syte*)))
             (ctx (tmpl:make-context :name (tmpl:template-filename tmpl)
                                     :parent (tmpl:template-context tmpl))))
        (funcall (tmpl:template-function tmpl) ctx)
        ctx)))

(tmpl:def-primitive "absurl"
    (lambda (relink)
      (let* ((current tmpl:*current-template*)
             (filename (tmpl:template-filename current))
             (directory (make-pathname :directory (pathname-directory filename)))
             (root (syte-root *current-syte*))
             (relbase (list* :absolute
                             (cdr (pathname-directory (enough-namestring filename root)))))
             (absolute-url (merge-pathnames relink (make-pathname :directory relbase)))
             (absolute-file (merge-pathnames relink directory)))
        (namestring
         (cond
           ((fad:directory-exists-p absolute-file)
            (fad:pathname-as-directory absolute-url))
           ((and (fad:file-exists-p absolute-file)
                 (awhen (pathname-type absolute-file)
                   (string-equal it "syt")))
            (make-pathname :directory (pathname-directory absolute-url)
                           :name (pathname-name absolute-url)))
           (t
            absolute-url))))))

(tmpl:def-primitive "sameurl"
    (lambda (url1 &optional url2)
      (let ((file1 (url-to-file url1))
            (file2 (if url2
                       (url-to-file url2)
                       (tmpl:template-filename tmpl:*request-template*))))
        (when (fad:file-exists-p file1)
          (setf file1 (truename file1)))
        (when (fad:file-exists-p file2)
          (setf file2 (truename file2)))
        (equal file1 file2))))

(tmpl:def-primitive "http/set-status"
    (lambda (status)
      (when (tmpl:my-symbol-p status)
        (setf status (tmpl:my-symbol-name status)))
      (when (stringp status)
        (let* ((name (format nil "+HTTP-~A+" (string-upcase status)))
               (sym (find-symbol name :hunchentoot)))
          (when sym (setf status (symbol-value sym)))))
      (setf (tbnl:return-code*) status)))

(tmpl:def-primitive "http/script-name"
    (lambda ()
      (tbnl:script-name*)))

(tmpl:def-primitive "http/parameter"
    (lambda (name)
      (tbnl:parameter name)))

(defun def-syte-primitive (syte name func)
  (tmpl:def-primitive name func (syte-context syte)))


;;; entry point for buildapp

#+sbcl
(progn
  (defparameter *running* t)
  (defun main (argv)
    (declare (ignore argv))
    (sb-daemon:daemonize :output "/tmp/sytes.output"
                         :error "/tmp/sytes.error"
                         :exit-parent t
                         :sigterm (lambda (sig)
                                    (declare (ignore sig))
                                    (setf *running* nil)))
    (setf *running* t)
    (start-server)
    (loop while *running* do (sleep 1))))
