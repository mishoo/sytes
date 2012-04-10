(in-package #:sytes.template)

(defparameter *compile-cache* (make-hash-table :test #'equal))

(defun full-filename (filename context)
  (let ((root (context-root-up context)))
    (if root
        (merge-pathnames filename (fad:pathname-as-directory root))
        filename)))

(defun compile-file (filename context)
  (setf filename (full-filename filename context))
  (let ((timestamp (file-write-date filename))
        (cached (gethash filename *compile-cache*)))
    (when (and cached (<= timestamp (car cached)))
      (return-from compile-file (cdr cached)))
    (with-open-file (in filename)
      (let* ((ctx (make-context :name filename :parent context))
             (*current-context* ctx)
             (func (compile (parse in :template-name filename :context ctx))))
        (setf (gethash filename *compile-cache*) (cons timestamp func))
        func))))

