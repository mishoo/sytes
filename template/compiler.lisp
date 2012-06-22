(in-package #:sytes.template)

(defparameter +s-quote+ (tops "quote"))
(defparameter +s-lambda+ (tops "lambda"))
(defparameter +s-progn+ (tops "progn"))
(defparameter +s-if+ (tops "if"))
(defparameter +s-set+ (tops "&set!"))
(defparameter +s-def+ (tops "&def!"))
(defparameter +s-defmacro+ (tops "&defmacro!"))
(defparameter +s-eval-now+ (tops "eval-when-compile"))
(defparameter +s-catch+ (tops "catch"))
(defparameter +s-throw+ (tops "throw"))

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
      (defsetvar-context name nil)
      (setvar-context name (funcall value)))))

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
      name)))

(defun comp-eval-now (exprs)
  (loop for x in exprs
        for f = (comp-exp x)
        do (funcall f))
  (lambda () nil))

(defun comp-catch (tag body)
  (let ((tag (comp-exp tag))
        (body (comp-sequence body)))
    (lambda ()
      (catch (funcall tag)
        (funcall body)))))

(defun comp-throw (tag value)
  (let ((tag (comp-exp tag))
        (value (comp-exp value)))
    (lambda ()
      (throw (funcall tag)
        (funcall value)))))

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
       (if (my-symbol-p x)
           (cond
             ((eq x +s-quote+) (comp-constant (car args)))
             ((eq x +s-progn+) (comp-sequence args))
             ((eq x +s-lambda+) (comp-lambda (car args) (cdr args)))
             ((eq x +s-set+) (comp-set (car args) (cadr args)))
             ((eq x +s-def+) (comp-def (car args) (cadr args)))
             ((eq x +s-if+) (comp-if (car args) (cadr args) (caddr args)))
             ((eq x +s-defmacro+) (comp-defmacro (car args) (cadr args) (cddr args)))
             ((eq x +s-eval-now+) (comp-eval-now args))
             ((eq x +s-catch+) (comp-catch (car args) (cdr args)))
             ((eq x +s-throw+) (comp-throw (car args) (cadr args)))
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
  (let* ((*current-context* context)
         (exp (comp-exp exp)))
    (lambda (ctx &rest defs)
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
(def-primitive "copy-list" #'copy-list)
(def-primitive "apply" #'apply)
(def-primitive "not" #'not)
(def-primitive "null?" #'null)
(def-primitive "member" (lambda (item list &key (test #'das-eq))
                          (member item list :test test)))

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
                  (my-symbol (my-symbol-name x)))))))

  (def-primitive "get-hash"
      (lambda (hash key)
        (gethash (name-of key) hash)))

  (def-primitive "set-hash"
      (lambda (hash key value)
        (setf (gethash (name-of key) hash) value)))

  (defun same-name (a b)
    (string= (name-of a) (name-of b)))

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

(labels ((strcat (args out)
           (dolist (a args)
             (when a
               (typecase a
                 (string (write-string a out))
                 (character (write-char a out))
                 (list (strcat a out))
                 (t (format out "~A" a)))))))
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
        (with-output-to-string (out)
          (string-upcase (strcat args out)))))
  (def-primitive "string-downcase"
      (lambda (&rest args)
        (with-output-to-string (out)
          (string-downcase (strcat args out)))))
  (def-primitive "string-capitalize"
      (lambda (&rest args)
        (with-output-to-string (out)
          (string-capitalize (strcat args out)))))
  (def-primitive "esc"
      (lambda (&rest args)
        (tbnl:escape-for-html (with-output-to-string (out)
                                (strcat args out))))))

(def-primitive "sort" #'stable-sort)

(def-primitive "regexp-split" #'ppcre:split)
(def-primitive "regexp-replace"
    (lambda (rx str replacement)
      (ppcre:regex-replace-all
       rx str
       (lambda (str start end match-start match-end reg-starts reg-ends)
         (declare (ignorable str start end match-start match-end reg-starts reg-ends))
         (etypecase replacement
           (function (let ((match (subseq str match-start match-end)))
                       (apply replacement match (loop :for i :across reg-starts
                                                      :for j :across reg-ends
                                                      :collect (subseq str i j)))))
           (string replacement)
           (character (string replacement)))))))

(def-primitive "&defglobal!"
    (lambda (name value)
      (defglobal-context name value)))

(with-open-file (in (merge-pathnames "template/toplevel.syt"
                                     (asdf:component-pathname
                                      (asdf:find-system :sytes))))
  (funcall (compile (parse in)) *toplevel-context*))
