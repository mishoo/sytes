(in-package #:sytes.template)

(defstruct template
  (filename)
  (context)
  (timestamp)
  (function))

(defparameter *compile-cache* (make-hash-table :test #'equal))
(defparameter *autohandler* "autohandler.syt")
(defparameter *dhandler* "dhandler.syt")
(defparameter *compiler-lock* (bt:make-lock "SYTES.COMPILER"))
(defparameter *attributes* nil)

(def-primitive "*attributes*" (lambda () *attributes*))

(defun full-filename (filename context)
  (let ((root (context-root-up context)))
    (if root
        (merge-pathnames filename (fad:pathname-as-directory root))
        filename)))

(defun default-global-context (filename)
  `((,(tops "*autohandler*") . ,*autohandler*)
    (,(tops "*template*") . ,filename)))

(defun compile-file (filename &key (parent-context *current-context*) sub-context)
  (unless (pathnamep filename)
    (setf filename (full-filename filename parent-context)))
  (bt:with-lock-held (*compiler-lock*)
    (let ((timestamp (file-write-date filename))
          (cached (gethash filename *compile-cache*))
          (was-cached t))
      (unless (and cached (<= timestamp (template-timestamp cached)))
        (setf was-cached nil)
        (with-open-file (in filename)
          (let* ((ctx (or sub-context
                          (make-context :name filename
                                        :parent parent-context
                                        :global (default-global-context filename))))
                 (*current-context* ctx)
                 (func (let ((*token-start* *default-token-start*)
                             (*token-stop* *default-token-stop*))
                         (compile (parse in :template-name filename :context ctx)))))
            (if cached
                (setf (template-timestamp cached) timestamp
                      (template-function cached) func
                      (template-context cached) ctx)
                (setf cached
                      (setf (gethash filename *compile-cache*)
                            (make-template :timestamp timestamp
                                           :filename (truename filename)
                                           :context ctx
                                           :function func)))))))
      (values cached was-cached))))

(defun exec-template-request (filename rootdir parent-context &key base-comp)
  (unless (probe-file filename)
    (setf filename (find-file-up *dhandler* rootdir filename)))
  (when filename
    (let* ((tmpl (compile-file filename :parent-context parent-context))
           (ctx (template-context tmpl)))
      ;; at this point the template is compiled, but not run
      ;; if it replaces the *autohandler* variable, we should find it here.
      (let ((ah (aif (lookup-var (tops "*autohandler*") ctx t)
                     (cdr it)
                     *autohandler*)))
        (cond
          ((setf ah (find-file-up ah rootdir (template-filename tmpl)))
           ;; have autohandler
           (let ((call-me (lambda (&rest args)
                            (apply (template-function tmpl) "call-next" base-comp args))))
             (exec-template-request ah rootdir parent-context :base-comp call-me)))
          (t
           (let ((*attributes* (make-hash-table :test #'equal)))
             (funcall (template-function tmpl) "call-next" base-comp))))))))

(defun find-file-up (lookup root start)
  (when (probe-file start)
    (setf start (truename start)))
  (let* ((relative (parse-namestring (enough-namestring start root)))
         (directory (pathname-directory relative))
         (last (file-namestring relative)))
    (when (string= last lookup)
      (setf directory (butlast directory))
      (unless directory (return-from find-file-up nil)))
    (loop for dir = (reverse (rest directory)) then (cdr dir)
       for test-pathname = (merge-pathnames
                            (make-pathname :directory (list* :relative
                                                             (reverse dir))
                                           :defaults lookup)
                            root)
       thereis (probe-file test-pathname)
       while dir)))
