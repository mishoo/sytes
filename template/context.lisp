(in-package #:sytes.template)

(defstruct (keyval)
  (data))

(declaim (inline keyval-find))
(defun keyval-find (kv key &optional (error t))
  (let ((cell (assoc key (keyval-data kv))))
    (when (and error (not cell))
      (error "Undefined key: ~A" key))
    cell))

(declaim (inline keyval-set))
(defun keyval-set (kv key val)
  (setf (cdr (keyval-find kv key)) val))

(declaim (inline keyval-def))
(defun keyval-def (kv key &optional val)
  (setf (keyval-data kv)
        (acons key val (keyval-data kv)))
  val)

(declaim (inline keyval-push))
(defun keyval-push (kv defs)
  (setf (keyval-data kv)
        (nconc defs (keyval-data kv))))

(declaim (inline keyval-defset))
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

(declaim (inline context-inherit))
(defun context-inherit (ctx parent)
  (setf (context-parent ctx) parent))

(declaim (inline is-macro))
(defun is-macro (symbol &optional (context *current-context*))
  (aif (keyval-find (context-macros context) symbol nil)
       (cdr it)
       (awhen (context-parent context)
         (is-macro symbol it))))

(declaim (inline add-macro))
(defun add-macro (symbol func &optional (context *current-context*))
  (keyval-defset (context-macros context) symbol func))

(defun context-root-up (context)
  (when context
    (or (context-root context)
        (context-root-up (context-parent context)))))

(setf *toplevel-context* (make-context :name "TOPLEVEL"))
(setf *current-context* *toplevel-context*)

(declaim (inline tops))
(defun tops (name)
  (if (symbolp name)
      name
      (intern name :sytes.%runtime%)))

(defun lookup-var (sym &optional (context *current-context*) noerror noparent orig-context)
  (block nil
    (awhen (keyval-find (context-env context) sym nil)
      (return it))
    (awhen (keyval-find (context-global context) sym nil)
      (return it))
    (unless noparent
      (awhen (context-parent context)
        (return (lookup-var sym it noerror nil (or orig-context context)))))
    (unless noerror
      (error "Undefined variable ~A in context ~A"
             sym (context-name orig-context)))))

(declaim (inline defvar-context))
(defun defvar-context (sym value &optional (context *current-context*))
  (keyval-def (context-env context) sym value))

(declaim (inline defglobal-context))
(defun defglobal-context (sym value &optional (context *current-context*))
  (keyval-defset (context-global context) sym value))

(declaim (inline setvar-context))
(defun setvar-context (sym value &optional (context *current-context*))
  (let ((cell (lookup-var sym context nil t)))
    (setf (cdr cell) value)))

(declaim (inline defsetvar-context))
(defun defsetvar-context (sym value &optional (context *current-context*))
  (let ((cell (or (keyval-find (context-env context) sym nil)
                  (keyval-find (context-global context) sym nil))))
    (if cell
        (setf (cdr cell) value)
        (defvar-context sym value context))))

(declaim (inline extend-context))
(defun extend-context (definitions &optional (context *current-context*))
  (let* ((ctx (copy-context context))
         (env (copy-keyval (context-env ctx))))
    (setf (context-env ctx) env)
    (keyval-push env definitions)
    ctx))
