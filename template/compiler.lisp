(in-package #:sytes.template)

(defparameter +s-quote+ (tops "quote"))
(defparameter +s-lambda+ (tops "lambda"))
(defparameter +s-progn+ (tops "progn"))
(defparameter +s-if+ (tops "if"))
(defparameter +s-set+ (tops "&set!"))
(defparameter +s-def+ (tops "&def!"))
(defparameter +s-defglobal+ (tops "&defglobal!"))
(defparameter +s-defmacro+ (tops "&defmacro!"))
(defparameter +s-eval-now+ (tops "eval-now"))

(defun comp-constant (x)
  (lambda () x))

(defun comp-sequence (list)
  (if (cdr list)
      (let ((list (mapcar #'comp-exp list)))
        (lambda ()
          (loop for exp in list
             for value = (funcall exp)
             finally (return value))))
      (comp-exp (car list))))

(defun comp-lambda (args body)
  (let ((body (comp-sequence body)))
    (lambda ()
      (let ((env (context-env *current-context*)))
        (lambda (&rest values)
          (with-extended-context (env args values)
            (funcall body)))))))

(defun comp-set (name value)
  (let ((value (comp-exp value)))
    (lambda ()
      (setvar-context name (funcall value)))))

(defun comp-def (name value)
  (let ((value (comp-exp value)))
    (lambda ()
      (defsetvar-context name (funcall value))
      nil)))

(defun comp-defglobal (name value)
  (let ((value (comp-exp value)))
    (lambda ()
      (defglobal-context name (funcall value))
      nil)))

(defun comp-if (predicate then else)
  (let ((predicate (comp-exp predicate))
        (then (comp-exp then))
        (else (comp-exp else)))
    (lambda ()
      (if (funcall predicate)
          (funcall then)
          (funcall else)))))

(defun comp-funcall (name args)
  (let ((name (comp-exp name))
        (args (mapcar #'comp-exp args)))
    (lambda ()
      (apply (funcall name)
             (mapcar #'funcall args)))))

(defun comp-ref (name)
  (lambda ()
    (cdr (lookup-var name))))

(defun comp-defmacro (name args body)
  (let ((func (comp-lambda args body)))
    (lambda ()
      (defglobal-context name (funcall func))
      (setf (my-symbol-macro name) t)
      nil)))

(defun comp-eval-now (exprs)
  (loop for x in exprs
     for f = (comp-exp x)
     do (funcall f))
  (lambda () nil))

(defun comp-exp (x)
  (cond
    ((or (numberp x)
         (null x)
         (eq x t)
         (stringp x)) (comp-constant x))
    ((listp x)
     (let ((x (car x))
           (args (cdr x)))
       (if (my-symbol-p x)
           (cond
             ((eq x +s-quote+) (comp-constant (car args)))
             ((eq x +s-progn+) (comp-sequence args))
             ((eq x +s-lambda+) (comp-lambda (car args) (cdr args)))
             ((eq x +s-set+) (comp-set (car args) (cadr args)))
             ((eq x +s-def+) (comp-def (car args) (cadr args)))
             ((eq x +s-defglobal+) (comp-defglobal (car args) (cadr args)))
             ((eq x +s-if+) (comp-if (car args) (cadr args) (caddr args)))
             ((eq x +s-defmacro+) (comp-defmacro (car args) (cadr args) (cddr args)))
             ((eq x +s-eval-now+) (comp-eval-now args))
             ((my-symbol-macro x)
              (let* ((code (apply (cdr (lookup-var x)) args)))
                (comp-exp code)))
             (t
              (comp-funcall x args)))
           (comp-funcall x args))))
    ((my-symbol-p x)
     (comp-ref x))
    (t (error "Unsupported syntax: ~A" x))))

(defun compile (exp &key (context *current-context*))
  (let* ((ctx context)
         (*current-context* context)
         (exp (comp-exp exp)))
    (lambda (&rest defs)
      (let ((*current-context* ctx)
            (env (context-env ctx)))
        (unwind-protect
             (progn
               (loop for (name val) on defs by #'cddr do
                    (defvar-context (my-symbol-in-context name ctx)
                        val ctx))
               (funcall exp))
          (setf (context-env ctx) env))))))

(defgeneric das-eq (x y)
  (:method ((x string) (y string))
    (string= x y))
  (:method ((x number) (y number))
    (= x y))
  (:method (x y)
    (eq x y)))

(defun def-primitive (name func &optional (ctx *toplevel-context*))
  (defglobal-context (tops name) func ctx))

;; basic library

(def-primitive "+" #'+)
(def-primitive "-" #'-)
(def-primitive "/" #'/)
(def-primitive "*" #'*)
(def-primitive "=" #'=)
(def-primitive "<" #'<)
(def-primitive ">" #'>)
(def-primitive "<=" #'<=)
(def-primitive ">=" #'>=)
(def-primitive "/=" #'/=)
(def-primitive "string=" #'string=)
(def-primitive "string<" #'string<)
(def-primitive "string>" #'string>)
(def-primitive "string<=" #'string<=)
(def-primitive "string>=" #'string>=)
(def-primitive "string/=" #'string/=)
(def-primitive "1+" #'1+)
(def-primitive "1-" #'1-)
(def-primitive "list" #'list)
(def-primitive "list*" #'list*)
(def-primitive "append" #'append)
(def-primitive "nconc" #'nconc)
(def-primitive "nthcdr" #'nthcdr)
(def-primitive "cons" #'cons)
(def-primitive "eq" #'das-eq)
(def-primitive "mapcar" #'mapcar)
(def-primitive "mapc" #'mapc)
(def-primitive "reverse" #'reverse)
(def-primitive "nreverse" #'nreverse)
(def-primitive "apply" #'apply)

(def-primitive "rplaca" #'rplaca)
(def-primitive "rplacd" #'rplacd)
(def-primitive "car" #'car)
(def-primitive "cdr" #'cdr)
(def-primitive "caar" #'caar)
(def-primitive "cadr" #'cadr)
(def-primitive "cdar" #'cdar)
(def-primitive "cddr" #'cddr)
(def-primitive "caaar" #'caaar)
(def-primitive "caadr" #'caadr)
(def-primitive "cadar" #'cadar)
(def-primitive "caddr" #'caddr)
(def-primitive "cdaar" #'cdaar)
(def-primitive "cdadr" #'cdadr)
(def-primitive "cddar" #'cddar)
(def-primitive "cdddr" #'cdddr)
(def-primitive "caaaar" #'caaaar)
(def-primitive "caaadr" #'caaadr)
(def-primitive "caadar" #'caadar)
(def-primitive "caaddr" #'caaddr)
(def-primitive "cadaar" #'cadaar)
(def-primitive "cadadr" #'cadadr)
(def-primitive "caddar" #'caddar)
(def-primitive "cadddr" #'cadddr)
(def-primitive "cdaaar" #'cdaaar)
(def-primitive "cdaadr" #'cdaadr)
(def-primitive "cdadar" #'cdadar)
(def-primitive "cdaddr" #'cdaddr)
(def-primitive "cddaar" #'cddaar)
(def-primitive "cddadr" #'cddadr)
(def-primitive "cdddar" #'cdddar)
(def-primitive "cddddr" #'cddddr)

(def-primitive "function?" #'functionp)
(def-primitive "number?" #'numberp)
(def-primitive "string?" #'stringp)
(def-primitive "cons?" #'consp)
(def-primitive "list?" #'listp)
(def-primitive "symbol?"
    (lambda (x)
      (or (null x) (eq x t) (my-symbol-p x))))

(def-primitive "echo-raw"
    (lambda (&rest stuff)
      (dolist (x stuff)
        (when x (format t "~A" x)))))

(def-primitive "echo-esc"
    (lambda (&rest stuff)
      (dolist (x stuff)
        (when x (format t "~A" x)))))

(def-primitive "make-hash"
    (lambda ()
      (make-hash-table :test #'equal)))

(labels ((name-of (x)
           (cond
             ((eq x t) "t")
             ((null x) "nil")
             (t (etypecase x
                  (string x)
                  (my-symbol (my-symbol-name x)))))))

  (def-primitive "get-hash"
      (lambda (hash key)
        (gethash (name-of key) hash)))

  (def-primitive "set-hash"
      (lambda (hash key value)
        (setf (gethash (name-of key) hash) value)))

  (defun same-name (a b)
    (string= (name-of a) (name-of b)))

  (def-primitive "%dot-lookup"
      (lambda (main &rest props)
        (block out
          (dolist (i props)
            (setf main
                  (etypecase main
                    (cons (cdr (assoc i main :test #'same-name)))
                    (hash-table (gethash (name-of i) main))
                    (context (lookup-var i main t))
                    (null (return-from out nil)))))
          main)))

  (def-primitive "%dot-set"
      (lambda (main value &rest props)
        (loop for i on props
           for prop = (name-of (car i))
           do (cond
                ((null (cdr i))
                 (setf (gethash prop main) value))
                (t
                 (let ((tmp (gethash prop main)))
                   (unless tmp
                     (setf tmp (make-hash-table :test #'equal)
                           (gethash prop main) tmp))
                   (setf main tmp)))))
        value)))

(def-primitive "fmt" (lambda (fmt &rest args)
                       (apply #'format nil fmt args)))

(def-primitive "format" (lambda (stream fmt &rest args)
                          (apply #'format stream fmt args)))

(let ((i 0))
  (def-primitive "gensym"
      (lambda (&optional (name "S"))
        (make-my-symbol :name (format nil "~A~D" name (incf i))))))

(def-primitive "macroexpand-1"
    (lambda (exp)
      (if (and (listp exp)
               (my-symbol-p (car exp))
               (my-symbol-macro (car exp)))
          (apply (cdr (lookup-var (car exp))) (cdr exp))
          exp)))

(def-primitive "capture-output"
    (lambda (func &rest args)
      (with-output-to-string (*standard-output*)
        (apply func args))))

(with-open-file (in (merge-pathnames "template/toplevel.syt"
                                     (asdf:component-pathname
                                      (asdf:find-system :sytes))))
  (funcall (compile (parse in))))
