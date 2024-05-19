(in-package #:sytes.template)

(defparameter +s-quote+ (tops "quote"))
(defparameter +s-lambda+ (tops "lambda"))
(defparameter +s-lambda-char+ (tops "λ"))
(defparameter +s-progn+ (tops "progn"))
(defparameter +s-if+ (tops "if"))
(defparameter +s-set+ (tops "&set!"))
(defparameter +s-def+ (tops "&def!"))
(defparameter +s-defmacro+ (tops "&defmacro!"))
(defparameter +s-defglobal+ (tops "&defglobal!"))
(defparameter +s-eval-now+ (tops "eval-when-compile"))
(defparameter +s-catch+ (tops "catch"))
(defparameter +s-throw+ (tops "throw"))
(defparameter +s-compile+ (tops "compile"))

(defun comp-constant (x)
  (lambda (@ctx)
    (declare (ignore @ctx))
    x))

(defun comp-sequence (list)
  (let ((list (mapcar #'comp-exp list)))
    (lambda (@ctx)
      (labels ((rec (list)
                 (when list
                   (if (cdr list)
                       (progn
                         (funcall (car list) @ctx)
                         (rec (cdr list)))
                       (funcall (car list) @ctx)))))
        (rec list)))))

(defun bindings (args values &optional ret)
  (if args
      (if (consp args)
          (bindings (cdr args) (cdr values)
                    (cons (cons (car args) (car values)) ret))
          (cons (cons args values) ret))
      ret))

(defun comp-lambda (args body)
  (let ((body (comp-sequence body)))
    (lambda (@ctx)
      (lambda (&rest values)
        (let* ((ctx (copy-context @ctx))
               (env (copy-keyval (context-env ctx))))
          (setf (context-env ctx) env)
          (setf (keyval-data env)
                (bindings args values (keyval-data env)))
          (funcall body ctx))))))

(defun comp-set (name value)
  (let ((value (comp-exp value)))
    (lambda (@ctx)
      (setvar-context name (funcall value @ctx) @ctx))))

(defun comp-def (name value)
  (let ((value (comp-exp value)))
    (lambda (@ctx)
      (defsetvar-context name (funcall value @ctx) @ctx))))

(defun comp-if (predicate then else)
  (let ((predicate (comp-exp predicate))
        (then (comp-exp then))
        (else (comp-exp else)))
    (lambda (@ctx)
      (if (funcall predicate @ctx)
          (funcall then @ctx)
          (funcall else @ctx)))))

(defun comp-funcall (name args)
  (let ((name (comp-exp name))
        (args (mapcar #'comp-exp args)))
    (lambda (@ctx)
      (apply (funcall name @ctx)
             (mapcar (lambda (arg)
                       (funcall arg @ctx))
                     args)))))

(defun comp-ref (name)
  (lambda (@ctx)
    (cdr (lookup-var name @ctx))))

(defun comp-defmacro (name args body)
  (let ((func (comp-lambda args body)))
    (lambda (@ctx)
      (add-macro name (funcall func @ctx) *current-context*)
      name)))

(defun comp-defglobal (name value)
  (let ((name (comp-exp name))
        (value (comp-exp value)))
    (lambda (@ctx)
      (defglobal-context
          (funcall name @ctx)
          (funcall value @ctx)
        @ctx))))

(defun comp-eval-now (exprs)
  (let (val)
    (loop for x in exprs
          for f = (comp-exp x)
          do (setf val (funcall f *current-context*)))
    (lambda (@ctx)
      (declare (ignore @ctx))
      val)))

(defun comp-catch (tag body)
  (let ((tag (comp-exp tag))
        (body (comp-sequence body)))
    (lambda (@ctx)
      (catch (funcall tag @ctx)
        (funcall body @ctx)))))

(defun comp-throw (tag value)
  (let ((tag (comp-exp tag))
        (value (comp-exp value)))
    (lambda (@ctx)
      (throw (funcall tag @ctx)
        (funcall value @ctx)))))

(defun comp-compile (expr use-inner-context)
  (let ((expr (comp-exp expr)))
    (lambda (@ctx)
      (let ((expr (funcall expr @ctx)))
        (assert (and (listp expr)
                     (or (eq (car expr) +s-lambda+)
                         (eq (car expr) +s-lambda-char+)))
                nil
                "COMPILE only takes a LAMBDA expression")
        (funcall (comp-exp expr)
                 (if use-inner-context
                     @ctx
                     *current-context*))))))

(defun comp-exp (x)
  (cond
    ((or (numberp x)
         (null x)
         (eq x t)
         (stringp x)
         (characterp x)
         (keywordp x)
         (functionp x)) (comp-constant x))
    ((listp x)
     (let ((x (car x))
           (args (cdr x)))
       (if (symbolp x)
           (cond
             ((eq x +s-quote+) (comp-constant (car args)))
             ((eq x +s-progn+) (comp-sequence args))
             ((eq x +s-lambda+) (comp-lambda (car args) (cdr args)))
             ((eq x +s-lambda-char+) (comp-lambda (car args) (cdr args)))
             ((eq x +s-set+) (comp-set (car args) (cadr args)))
             ((eq x +s-def+) (comp-def (car args) (cadr args)))
             ((eq x +s-if+) (comp-if (car args) (cadr args) (caddr args)))
             ((eq x +s-defmacro+) (comp-defmacro (car args) (cadr args) (cddr args)))
             ((eq x +s-defglobal+) (comp-defglobal (car args) (cadr args)))
             ((eq x +s-eval-now+) (comp-eval-now args))
             ((eq x +s-catch+) (comp-catch (car args) (cdr args)))
             ((eq x +s-throw+) (comp-throw (car args) (cadr args)))
             ((eq x +s-compile+) (comp-compile (car args) (cadr args)))
             (t
              (aif (is-macro x *current-context*)
                   (let* ((code (apply it args)))
                     (comp-exp code))
                   (comp-funcall x args))))
           (comp-funcall x args))))
    ((symbolp x)
     (comp-ref x))
    (t (error "Unsupported syntax: ~A" x))))

(defun compile (exp &key (context *current-context*))
  (let* ((*current-context* context)
         (exp (comp-exp exp)))
    (lambda (ctx &rest defs)
      (let ((*current-context* ctx))
        (funcall exp (extend-context
                      (loop for (name val) on defs by #'cddr collect (cons (tops name) val))
                      ctx))))))

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
(def-primitive "assoc" #'assoc)
(def-primitive "nconc" #'nconc)
(def-primitive "nthcdr" #'nthcdr)
(def-primitive "cons" #'cons)
(def-primitive "eq" #'das-eq)
(def-primitive "zerop" #'zerop)
(def-primitive "mapcar" #'mapcar)
(def-primitive "mapc" #'mapc)
(def-primitive "reverse" #'reverse)
(def-primitive "nreverse" #'nreverse)
(def-primitive "copy-list" #'copy-list)
(def-primitive "apply" #'apply)
(def-primitive "not" #'not)
(def-primitive "null?" #'null)
(def-primitive "member" (lambda (item list &key (test #'das-eq))
                          (member item list :test test)))
(def-primitive "vector" #'vector)

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
(def-primitive "length" #'length)
(def-primitive "elt" #'elt)

(def-primitive "function?" #'functionp)
(def-primitive "number?" #'numberp)
(def-primitive "string?" #'stringp)
(def-primitive "cons?" #'consp)
(def-primitive "list?" #'listp)
(def-primitive "symbol?"
    (lambda (x)
      (or (null x) (eq x t)
          (and (symbolp x)
               (eq (symbol-package x)
                   (find-package :sytes.%runtime%))))))

(def-primitive "intern" #'tops)

(def-primitive "make-hash"
    (lambda (&rest props)
      (let ((hash (make-hash-table :test #'equal)))
        (loop for (key val) on props by #'cddr
              do (setf (gethash key hash) val))
        hash)))

(labels ((name-of (x)
           (cond
             ((eq x t) "t")
             ((null x) "nil")
             (t (etypecase x
                  (string x)
                  (symbol (symbol-name x)))))))

  (def-primitive "get-hash"
      (lambda (hash key)
        (gethash (name-of key) hash)))

  (def-primitive "set-hash"
      (lambda (hash key value)
        (setf (gethash (name-of key) hash) value)))

  (defun same-name (a b)
    (string= (name-of a) (name-of b)))

  (defgeneric fetch-property (main key)
    (:method ((main standard-object) key)
      (setf key (name-of key))
      (loop for slot in (closer-mop:class-slots (class-of main))
            for name = (closer-mop:slot-definition-name slot)
            when (and (slot-boundp main name)
                      (string-equal key (name-of name)))
              do (return (slot-value main name)))))

  (def-primitive "&dot-lookup"
      (lambda (main &rest props)
        (block out
          (dolist (i props)
            t1 (when (functionp main)
                 (setf main (funcall main))
                 (go t1))
            (setf main
                  (etypecase main
                    (cons (cdr (assoc i main :test #'same-name)))
                    (hash-table (gethash (name-of i) main))
                    (context (cdr (lookup-var i main t)))
                    (standard-object (fetch-property main i))
                    (null (return-from out nil)))))
          main)))

  (def-primitive "%dot-set"
      (lambda (main value &rest props)
        (loop for i on props
              for prop = (name-of (car i))
              do (loop while (functionp main)
                       do (setf main (funcall main)))
                 (cond
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
        (gensym (format nil "~A~D" name (incf i))))))

(def-primitive "macroexpand-1"
    (lambda (exp)
      (aif (and (listp exp)
                (symbolp (car exp))
                (is-macro (car exp)))
           (apply it (cdr exp))
           exp)))

(def-primitive "capture-output"
    (lambda (func &rest args)
      (with-output-to-string (*standard-output*)
        (apply func args))))

(labels ((strcat (args out)
           (typecase args
             (list (dolist (a args)
                     (when a
                       (typecase a
                         (string (write-string a out))
                         (character (write-char a out))
                         (list (strcat a out))
                         (t (format out "~A" a))))))
             (string (write-string args out))
             (character (write-char args out))
             (t (format out "~A" args)))))
  (def-primitive "strcat"
      (lambda (&rest args)
        (with-output-to-string (out)
          (strcat args out))))
  (def-primitive "print"
      (lambda (&rest args)
        (strcat args *standard-output*)))
  (def-primitive "trim"
      (lambda (&rest args)
        (string-trim '(#\Space
                       #\Newline
                       #\Return
                       #\Linefeed
                       #\Tab
                       #\Page
                       #\Line_Separator
                       #\Paragraph_Separator
                       #\NO-BREAK_SPACE)
                     (with-output-to-string (out)
                       (strcat args out)))))
  (def-primitive "string-trim" #'string-trim)
  (def-primitive "string-left-trim" #'string-left-trim)
  (def-primitive "string-right-trim" #'string-right-trim)
  (def-primitive "string-upcase"
      (lambda (&rest args)
        (string-upcase (with-output-to-string (out)
                         (strcat args out)))))
  (def-primitive "string-downcase"
      (lambda (&rest args)
        (string-downcase (with-output-to-string (out)
                           (strcat args out)))))
  (def-primitive "string-capitalize"
      (lambda (&rest args)
        (string-capitalize (with-output-to-string (out)
                             (strcat args out)))))
  (def-primitive "esc"
      (lambda (&rest args)
        (tbnl:escape-for-html (with-output-to-string (out)
                                (strcat args out)))))

  (def-primitive "join"
      (lambda (separator list)
        (with-output-to-string (out)
          (loop for i in list
                for first = t then nil
                unless first
                  do (strcat separator out)
                do (strcat i out))))))

(def-primitive "sort" #'stable-sort)

(def-primitive "json-encode"
    (lambda (obj)
      (json:encode-json-to-string obj)))

(def-primitive "regexp-split" #'ppcre:split)
(def-primitive "regexp-replace"
    (lambda (rx str replacement)
      (ppcre:regex-replace-all
       rx str
       (lambda (str start end match-start match-end reg-starts reg-ends)
         (declare (ignorable str start end match-start match-end reg-starts reg-ends))
         (etypecase replacement
           (function (let ((match (subseq str match-start match-end)))
                       (apply replacement match
                              (map 'list (lambda (i j)
                                           (subseq str i j))
                                   reg-starts reg-ends))))
           (string replacement)
           (character (string replacement)))))))

(def-primitive "parse-syte"
    (lambda (str &optional (name "UNKNOWN-TEMPLATE"))
      (with-input-from-string (in str)
        (parse in :template-name name))))

(with-open-file (in (merge-pathnames "template/toplevel.syt"
                                     (asdf:component-pathname
                                      (asdf:find-system :sytes))))
  (funcall (compile (parse in)) *toplevel-context*))

(defmacro define-fetcher ((object type) prop &body body)
  (let ((sym (gensym)))
    `(defmethod fetch-property ((,object ,type) (,sym (eql ',(tops prop))))
       ,@body)))
