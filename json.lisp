(in-package #:sytes)

(defparameter *json-notify-before* nil)
(defparameter *json-notify-after* nil)

(export 'syte+json)
(defclass syte+json ()
  ((json-handlers :accessor syte-json-handlers
                  :initarg :json-handlers
                  :initform (make-hash-table :test #'equal))))

;;; some utils

(defun dashes-to-underscores (str)
  (substitute #\_ #\- str))

(defun underscores-to-dashes (str)
  (substitute #\- #\_ str))

(defun json-identifier-name-to-lisp (str)
  (string-upcase
   (underscores-to-dashes str)))

(defun lisp-identifier-name-to-json (str)
  (string-downcase
   (dashes-to-underscores str)))

(defparameter *json-standard* nil)

(defmethod json:encode-json ((value local-time:timestamp) &optional stream)
  (if *json-standard*
      (json:encode-json (local-time:format-rfc3339-timestring nil value) stream)
      (progn
        (write-string "new Date(" stream)
        (json:encode-json (+ (* 1000 (local-time:timestamp-to-unix value))
                             (local-time:timestamp-millisecond value)) stream)
        (write-string ")" stream))))

(export 'with-standard-json)
(defmacro with-standard-json (&body body)
  `(let ((*json-standard* t))
     ,@body))

(defmethod json:encode-json ((value (eql :null)) &optional stream)
  (write-string "null" stream))

(defclass cl-json-always-hash ()
  ((data :initarg :data :type list)))

(defmethod json:encode-json ((this cl-json-always-hash) &optional stream)
  (let ((val (slot-value this 'data)))
    (if (consp (car val))
        (json:encode-json-alist val stream)
        (json:encode-json-plist val stream))))

(export 'json-hash)
(defun json-hash (data)
  (make-instance 'cl-json-always-hash :data data))

(defclass cl-json-already-json ()
  ((data :initarg :data :type string)))

(defmethod json:encode-json ((this cl-json-already-json) &optional stream)
  (with-slots (data) this
    (write-string data stream)))

(export 'json-already)
(defun json-already (data)
  (make-instance 'cl-json-already-json :data data))

(export 'define-json-handler)
(defmacro define-json-handler (syte name args &body body)
  `(flet ((,name ,args ,@body))
     (setf (gethash (symbol-name ',name) (syte-json-handlers ,syte)) #',name)))

(export 'json-push-notify)
(defun json-push-notify (type data &optional (how :after))
  (ecase how
    (:after (push (vector nil type data) *json-notify-after*))
    (:before (push (vector nil type data) *json-notify-before*))))

(defgeneric execute-json-requests (syte requests)
  (:method ((syte syte+json) requests)
    (loop with before and after
          for (id cmd . args) in requests
          for ret = (let ((*json-notify-before* nil)
                          (*json-notify-after* nil)
                          (func (gethash (string-upcase cmd) (syte-json-handlers syte))))
                      (vector id cmd
                              (cond
                                ((functionp func)
                                 (handler-case
                                     (prog1
                                         (apply func args)
                                       (setf before *json-notify-before*
                                             after *json-notify-after*))
                                   (rpc-error (ex)
                                     ex)))
                                (t
                                 (make-condition 'rpc-error :code 'nope :text "Unknown command")))))
          when before nconc it
            collect ret
          when after nconc it)))

;;;

(export 'rpc-error)
(define-condition rpc-error (error)
  ((code :initarg :code :initform nil :accessor code)
   (text :initarg :text :initform "NOTEXT" :accessor text)
   (info :initarg :info :initform "NOINFO" :accessor info)))

(defmethod json:encode-json ((e rpc-error) &optional stream)
  (json:encode-json (json-hash `((error . ((code . ,(code e))
                                           (text . ,(text e))
                                           (info . ,(info e))))))
                    stream))

(export 'syte-on-error)
(defgeneric syte-on-error (syte ex))

(export 'syte-enable-json-handler)
(defgeneric syte-enable-json-handler (syte)
  (:method ((syte syte+json))
    (def-url-handler (syte "^/@json(/std)?$" *json-standard*)
      (let ((json:*json-identifier-name-to-lisp* #'json-identifier-name-to-lisp)
            (json:*lisp-identifier-name-to-json* #'lisp-identifier-name-to-json))
        (let* ((data (tbnl:raw-post-data))
               (response (block response
                           (handler-bind ((rpc-error
                                            (lambda (ex)
                                              (syte-on-error syte ex)
                                              (return-from response ex)))
                                          (json:json-syntax-error
                                            (lambda (ex)
                                              (syte-on-error syte ex)
                                              (return-from response
                                                (make-condition 'rpc-error :code 'syntax :text "Syntax error in JSON"))))
                                          (error
                                            (lambda (ex)
                                              (when tbnl:*catch-errors-p*
                                                (syte-on-error syte ex)
                                                (let ((backtrace (trivial-backtrace:print-backtrace ex :output nil)))
                                                  (return-from response
                                                    (make-condition 'rpc-error :code 'fatal :text backtrace)))))))
                             (unless data
                               (error 'rpc-error :code :nodata :text "No POST data"))
                             (unless (stringp data)
                               (setq data (trivial-utf-8:utf-8-bytes-to-string data)))
                             (setf data (json:decode-json-from-string data))
                             (execute-json-requests syte data)))))
          `(:content-type "text/javascript; charset=UTF-8"
            :content ,(json:encode-json-to-string response)))))))
