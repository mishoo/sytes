(in-package #:sytes.template)

(defstruct (keyval)
  (data))

(defun keyval-find (kv key &optional (error t))
  (let ((cell (assoc key (keyval-data kv))))
    (when (and error (not cell))
      (error "Undefined key: ~A" key))
    cell))

(defun keyval-set (kv key val)
  (setf (cdr (keyval-find kv key)) val))

(defun keyval-def (kv key &optional val)
  (setf (keyval-data kv)
        (acons key val (keyval-data kv)))
  val)

(defun keyval-push (kv defs)
  (setf (keyval-data kv)
        (nconc defs (keyval-data kv))))

(defun keyval-defset (kv key val)
  (aif (keyval-find kv key nil)
       (setf (cdr it) val)
       (keyval-def kv key val)))

(defparameter *toplevel-context* nil)
(defparameter *current-context* nil)

(defstruct (context (:constructor
                        make-context (&key
                                        (parent *toplevel-context*)
                                        (name (if parent (context-name parent)
                                                  "UNKNOWN"))
                                        (root (and parent (context-root parent)))
                                        (env (make-keyval))
                                        (global (make-keyval))
                                        (macros (make-keyval)))))
  (name)
  (parent)
  (root)
  (env)
  (global)
  (macros))

(defun is-macro (symbol &optional (context *current-context*))
  (or (awhen (keyval-find (context-macros context) symbol nil)
        (cdr it))
      (awhen (context-parent context)
        (is-macro symbol it))))

(defun add-macro (symbol func &optional (context *current-context*))
  (keyval-defset (context-macros context) symbol func))

(defun context-root-up (context)
  (loop while (and context (null (context-root context)))
     do (setf context (context-parent context)))
  (and context (context-root context)))

(setf *toplevel-context* (make-context :name "TOPLEVEL"))
(setf *current-context* *toplevel-context*)

(defun tops (name)
  (if (symbolp name)
      name
      (intern name :sytes.%runtime%)))

(defun lookup-var (sym &optional (context *current-context*) noerror noparent)
  (block nil
    (awhen (keyval-find (context-env context) sym nil)
      (return it))
    (awhen (keyval-find (context-global context) sym nil)
      (return it))
    (unless noparent
      (awhen (context-parent context)
        (return (lookup-var sym it noerror))))
    (unless noerror
      (error "Undefined variable ~A in context ~A"
             sym (context-name context)))))

(defun defvar-context (sym value &optional (context *current-context*))
  (keyval-def (context-env context) sym value))

(defun defglobal-context (sym value &optional (context *current-context*))
  (keyval-defset (context-global context) sym value))

(defun setvar-context (sym value &optional (context *current-context*))
  (let ((cell (lookup-var sym context nil t)))
    (setf (cdr cell) value)))

(defun defsetvar-context (sym value &optional (context *current-context*))
  (let ((cell (or (keyval-find (context-env context) sym nil)
                  (keyval-find (context-global context) sym nil))))
    (if cell
        (setf (cdr cell) value)
        (defvar-context sym value context))))

(defun extend-context (definitions &optional (context *current-context*))
  (let* ((ctx (copy-context context))
         (env (copy-keyval (context-env ctx))))
    (setf (context-env ctx) env)
    (keyval-push env definitions)
    ctx))
