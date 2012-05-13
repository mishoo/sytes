;;;; sytes.lisp

(in-package #:sytes)

(export '(register-syte
          unregister-syte
          syte-request-handler))

;;; "sytes" goes here. Hacks and glory await!

(defparameter *syte-names* (make-hash-table :test #'equal))
(defparameter *current-syte* nil)
(defparameter *syte-toplevel-context* (tmpl:make-context :name "SYTES"))

(with-open-file (in (merge-pathnames "template/instance.syt"
                                     (asdf:component-pathname
                                      (asdf:find-system :sytes))))
  (funcall (tmpl::compile (tmpl::parse in) :context *syte-toplevel-context*)))

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

(defmethod initialize-instance :after ((syte syte) &key names root context &allow-other-keys)
  (unless context
    (setf root
          (setf (syte-root syte) (truename root)))
    (setf context
          (setf (syte-context syte) (tmpl:make-context :name (car names) :root root :parent *syte-toplevel-context*)))
    (let ((boot (merge-pathnames ".boot.syt" root)))
      (when (probe-file boot)
        (with-open-file (in boot)
          (funcall (tmpl::compile (tmpl::parse in :context context))))))))

(defun register-syte (syte)
  (loop for name in (syte-names syte)
     do (setf (gethash name *syte-names*) syte)))

(defun unregister-syte (syte)
  (loop for name in (syte-names syte)
     do (remhash name *syte-names*)))

(defgeneric syte-request-handler (syte request)
  (:method ((syte (eql nil)) request)
    (format nil "No syte defined for host ~A" (hostname request)))
  (:method ((syte syte) request)
    (let* ((file (tbnl:script-name request))
           (root (syte-root syte))
           (pos (position-if-not (lambda (x) (char= x #\/)) file)))
      (if pos
          (setf file (subseq file pos))
          (setf file ""))
      (setf file (merge-pathnames file root))
      (aif (fad:directory-exists-p file)
           (setf file (merge-pathnames "index.syt" file))
           (unless (probe-file file)
             (setf file (make-pathname :defaults file :type "syt"))))
      (or (tmpl:exec-template-request file (syte-root syte) (syte-context syte))
          (setf (tbnl:return-code*) tbnl:+http-not-found+)))))

;;; hunchentoot stuff

(defclass sytes-request (tbnl:request)
  ())

(defclass sytes-acceptor (tbnl:acceptor)
  ()
  (:default-initargs
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


;;; some more primitives

(tmpl:def-primitive "%import"
    (lambda (name)
      (tmpl:template-context
       (tmpl:compile-file name (syte-context *current-syte*)))))

(defun def-syte-primitive (syte name func)
  (tmpl:def-primitive name func (syte-context syte)))

(tmpl:def-primitive "http/set-status"
    (lambda (status)
      (when (tmpl:my-symbol-p status)
        (setf status (tmpl:my-symbol-name status)))
      (when (stringp status)
        (let* ((name (format nil "+HTTP-~A+" (string-upcase status)))
               (sym (find-symbol name :hunchentoot)))
          (when sym (setf status (symbol-value sym)))))
      (setf (tbnl:return-code*) status)))
