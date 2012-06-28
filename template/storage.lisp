(in-package #:sytes.template)

(defstruct template
  (filename)
  (context)
  (timestamp)
  (function))

(defparameter *current-template* nil)
(defparameter *request-template* nil)
(defparameter *compile-cache* (make-hash-table :test #'equal))
(defparameter *autohandler* "autohandler.syt")
(defparameter *dhandler* "dhandler.syt")
(defparameter *compiler-lock* (bt:make-lock "SYTES.COMPILER"))
(defparameter *attributes* nil)

(defun clear-cache ()
  (bt:with-lock-held (*compiler-lock*)
    (setf *compile-cache* (make-hash-table :test #'equal))))

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
                                        :global (tmpl:make-keyval :data (default-global-context filename)))))
                 (*current-context* ctx)
                 (tmpl (make-template :filename (truename filename)
                                      :context ctx))
                 (func (let ((*token-start* *default-token-start*)
                             (*token-stop* *default-token-stop*)
                             (*current-template* tmpl))
                         (setf cached
                               (setf (gethash filename *compile-cache*) tmpl))
                         (compile (parse in :template-name filename)))))
            (setf (template-timestamp cached) timestamp
                  (template-function cached) func))))
      (values cached was-cached))))

(defun exec-template-request (filename rootdir parent-context &key base-comp variables)
  (unless (probe-file filename)
    (setf filename (find-file-up *dhandler* rootdir filename)))
  (when filename
    (let* ((tmpl (compile-file filename :parent-context parent-context))
           (ctx (make-context :name filename
                              :parent (template-context tmpl))))
      (flet ((doit ()
               ;; at this point the template is compiled, but not run
               ;; if it replaces the *autohandler* variable, we should find it here.
               (let ((ah (aif (lookup-var (tops "*autohandler*") ctx t)
                              (cdr it)
                              *autohandler*)))
                 (cond
                   ((and ah (setf ah (find-file-up ah rootdir (template-filename tmpl))))
                    ;; have autohandler
                    (let ((call-me (lambda (&rest args)
                                     (let ((*current-template* tmpl))
                                       (apply (template-function tmpl) ctx (tops "call-next") base-comp (append args variables))))))
                      (exec-template-request ah rootdir parent-context :base-comp call-me :variables variables)))
                   (t
                    (let ((*attributes* (make-hash-table :test #'equal))
                          (*current-template* tmpl))
                      (apply (template-function tmpl) ctx (tops "call-next") base-comp variables)))))))
        (if base-comp
            (doit)
            (let ((*request-template* tmpl))
              (doit)))))))

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
