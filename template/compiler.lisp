(in-package #:sytes.template)

(defparameter *part-compilers* (make-hash-table))

(defmacro def-compiler (type (&rest args) &body body)
  (let ((fname (intern (format nil "COMP-~A" type))))
    `(labels ((,fname ,args
                ,@body))
       (setf (gethash ,type *part-compilers*) #',fname))))

(defun compile (x)
  (let* ((type (car x))
         (args (cdr x))
         (comp (gethash type *part-compilers*)))
    (apply comp args)))

(def-compiler :text (&rest args)
  (let ((body (mapcar #'compile args)))
    (lambda (output context)
      (dolist (x body)
        (funcall x output context)))))

(def-compiler :cat (str)
  (lambda (output context)
    (declare (ignore context))
    (write-string str output)
    nil))

(def-compiler :list (&rest elements)
  (lambda (output context)
    ))
