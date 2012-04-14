(in-package #:sytes.template)

(defstruct (my-symbol (:print-object print-my-symbol))
  (name nil :type string)
  (macro nil :type boolean))

(defun print-my-symbol (sym stream)
  (write-string (my-symbol-name sym) stream))

(defparameter *toplevel-context* nil)
(defparameter *current-context* nil)

(defstruct (context (:constructor
                     make-context (&key (name "UNKNOWN")
                                        (symbols (make-hash-table :test #'equal))
                                        (parent *toplevel-context*)
                                        (root (and parent (context-root parent)))
                                        (env)
                                        (global))))
  (name)
  (symbols)
  (parent)
  (root)
  (env)
  (global))

(defun context-root-up (context)
  (loop while (and context (null (context-root context)))
     do (setf context (context-parent context)))
  (and context (context-root context)))

(setf *toplevel-context* (make-context :name "TOPLEVEL"))
(setf *current-context* *toplevel-context*)

(defun my-symbol-in-context (name context &optional nointern)
  (when (my-symbol-p name) (return-from my-symbol-in-context name))
  ;; actually intern them all in the toplevel context; this becomes
  ;; too messy.
  (setf context *toplevel-context*)
  (when (zerop (length name))
    (error "Missing symbol name"))
  (let ((syms (context-symbols context)))
    (cond
      ((string-equal name "t") t)
      ((string-equal name "nil") nil)
      (t
       (or (gethash name syms)
           (awhen (context-parent context)
             (my-symbol-in-context name it t))
           (and (not nointern)
                (setf (gethash name syms) (make-my-symbol :name name))))))))

(defun tops (name)
  (my-symbol-in-context name *toplevel-context*))

(defun lookup-var (sym &optional (context *current-context*) noerror)
  (or (assoc sym (context-env context))
      (assoc sym (context-global context))
      (awhen (context-parent context)
        (lookup-var sym it noerror))
      (if noerror
          nil
          (error "Undefined variable ~A in context ~A"
                 sym (context-name context)))))

(defun defvar-context (sym value &optional (context *current-context*))
  (setf (context-env context)
        (acons sym value (context-env context))))

(defun defglobal-context (sym value &optional (context *current-context*))
  (let ((cell (assoc sym (context-global context))))
    (if cell
        (setf (cdr cell) value)
        (setf (context-global context)
              (acons sym value (context-global context))))))

(defun setvar-context (sym value &optional (context *current-context*))
  (let ((cell (lookup-var sym context)))
    (setf (cdr cell) value)))

(defun defsetvar-context (sym value &optional (context *current-context*))
  (let ((cell (or (assoc sym (context-env context))
                  (assoc sym (context-global context)))))
    (if cell
        (setf (cdr cell) value)
        (defvar-context sym value context))))

(defmacro with-extended-context ((env names values &optional (context '*current-context*)) &body body)
  (let ((save-env (gensym))
        (ctx (gensym)))
    `(let* ((,ctx ,context)
            (,save-env (context-env ,ctx)))
       (setf (context-env ,ctx) ,env)
       (loop with name = ,names
          with val = ,values
          while name
          do (cond
               ((consp name)
                (defvar-context (car name) (car val) ,ctx)
                (setf val (cdr val)
                      name (cdr name)))
               (t
                (defvar-context name val ,ctx)
                (return))))
       (unwind-protect (progn ,@body)
         (setf (context-env ,ctx) ,save-env)))))
