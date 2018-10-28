(in-package #:sytes)

(export '(syte+git
          static-file-revision
          make-static-file-link
          static-root))

(defclass syte+git ()
  ((static-root :initarg :static-root :accessor static-root)
   (static-file-revisions :accessor static-file-revisions)))

(defun init-static-file-revisions (site)
  (let* ((output (trim (shell (format nil "cd ~A; git ls-tree -r HEAD -- ."
                                      (static-root site)))))
         (hash (make-hash-table :test #'equal)))
    (setf (static-file-revisions site) hash)
    (loop for line in (ppcre:split "\\s*\\n+\\s*" output)
          for a = (ppcre:split "\\s+" line)
          for revision = (elt a 2)
          for filename = (elt a 3)
          do (setf (gethash filename hash) revision))))

(defmethod initialize-instance :after ((site syte+git) &key)
  (init-static-file-revisions site))

(defmethod static-file-revision ((site syte+git) file)
  (gethash file (static-file-revisions site)))

(defmethod make-static-file-link ((site syte+git) file)
  (let ((rev (static-file-revision site file)))
    (if rev
        (format nil "/s/~A~~~A" file rev)
        (format nil "/s/~A" file))))

(tmpl:def-primitive "static-file-link"
    (lambda (file)
      (make-static-file-link *current-syte* file)))

;;; running programs (frankly, only tested on SBCL)

#-(or sbcl ccl)
(defun run-program (&rest args)
  (error "Don't know how to run external process!"))

#+ccl
(progn
  (defun run-program (program args &key search input output error (wait t))
    (declare (ignore search))
    (ccl:run-program program args :input input :output output :error error :wait wait :sharing :external))

  (defun process-exit-code (proc)
    (multiple-value-bind (status code) (ccl:external-process-status proc)
      (unless (eq status :running)
        code)))

  (defun process-input (process)
    (ccl:external-process-input-stream process))

  (defun process-output (process)
    (ccl:external-process-output-stream process)))

(defun trim (str)
  (string-trim '(#\Space
                 #\Newline
                 #\Tab
                 #\Page
                 #\Line_Separator
                 #\Paragraph_Separator
                 #\NO-BREAK_SPACE)
               str))

(defun shell (cmd)
  (with-output-to-string (out)
    (with-input-from-string (in cmd)
      (with-output-to-string (err)
        (run-program "/bin/sh" ()
                     :input in
                     :output out
                     :error err)))))
