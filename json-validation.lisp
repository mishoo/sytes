(in-package #:sytes)

(define-condition rpc-error-validation (rpc-error) ())

(defmethod json:encode-json ((e rpc-error-validation) &optional stream)
  (json:encode-json (json-hash `((code . ,(code e))
                                 (text . ,(text e))
                                 (info . ,(info e))))
                    stream))

(export 'validate-fields)
(defmacro validate-fields (&body fields)
  (let ((errors (gensym))
        (ex (gensym)))
    `(let ((,errors nil))
       ,@(loop for (field . args) in fields collect
               `(handler-case (validate ,field ,@args :field-name ',field)
                  (rpc-error-validation (,ex)
                    (push ,ex ,errors))))
       (when ,errors
         (error 'rpc-error
                :text "Validation error"
                :code "VALIDATION"
                :info (reverse ,errors))))))

(defgeneric validate (field how
                      &rest args
                      &key text code field-name info
                      &allow-other-keys))

(defmethod validate ((field string) (how (eql :required))
                     &key
                       (text "This field is required")
                       (code :bad-data)
                       field-name
                       (info field-name))
  (validate field "\\S" :text text :code code :info info :field-name field-name))

(defmethod validate ((field null) (how (eql :required))
                     &key
                       (text "This field is required")
                       (code :bad-data)
                       field-name
                       (info field-name))
  (error 'rpc-error-validation :text text :code code :info info))

(defmethod validate (field (how (eql :required)) &key &allow-other-keys)
  t)

(defmethod validate ((field string) (regex string)
                     &key (text "Validation error") (code :bad-data)
                       field-name (info field-name))
  (unless (ppcre:scan regex field)
    (error 'rpc-error-validation :text text :code code :info info)))

(defmethod validate ((field string) (how (eql :email))
                     &key
                       (text "This doesn't look like an email address")
                       (code :bad-data)
                       field-name
                       (info field-name))
  (validate field "^$|^([^@\\n\\r,]+)@((?:[-a-z0-9]+\\.)+[a-z]{2,})$"
            :text text :code code :info info))

(defmethod validate ((field null) (how (eql :email)) &key &allow-other-keys)
  t)

(defmethod validate ((field string) (handler function)
                     &key (text "Validation error") (code :bad-data)
                       field-name (info field-name))
  (awhen (funcall handler field-name field)
    (when (stringp it)
      (setf text it))
    (error 'rpc-error-validation :text text :code code :info info)))
