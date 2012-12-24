(in-package #:sytes.template)

(defparameter *default-token-start* #\{)
(defparameter *default-token-stop* #\})
(defparameter *token-start* *default-token-start*)
(defparameter *token-stop* *default-token-stop*)
(defparameter *list-terminator* #\))

(defun parse (input &key (template-name "UNKNOWN-TEMPLATE"))
  (let ((line 1)
        (col 0)
        (tokline 0)
        (tokcol 0)
        (indentation 0)
        (past-indent nil)
        (in-qq 0))
    (labels
        ((croak (msg &rest args)
           (error "Error in ~A (~A,~A): ~A" template-name tokline tokcol
                  (apply #'format nil msg args)))

         (peek (&optional eof-error-p)
           (peek-char nil input eof-error-p))

         (next (&optional eof-error-p)
           (let ((ch (read-char input eof-error-p)))
             (cond
               ((eql ch #\Newline)
                (setf line (1+ line)
                      col 0
                      indentation 0
                      past-indent nil))
               (ch
                (incf col)
                (unless past-indent
                  (if (eql ch #\Space)
                      (incf indentation)
                      (setq past-indent t)))))
             ch))

         (read-while (pred &optional eof-error-p)
           (with-output-to-string (ret)
             (loop while (funcall pred (peek eof-error-p))
                   do (write-char (next) ret))))

         (skip-whitespace ()
           (read-while (lambda (ch)
                         (member ch '(#\Space
                                      #\Newline
                                      #\Return
                                      #\Linefeed
                                      #\Tab
                                      #\Page
                                      #\Line_Separator
                                      #\Paragraph_Separator
                                      #\NO-BREAK_SPACE)))))

         (skip (ch)
           (unless (char= (next) ch)
             (croak "Expecting '~C'" ch)))

         (read-escaped (start stop addesc)
           (skip-whitespace)
           (skip start)
           (with-output-to-string (ret)
             (loop for ch = (next)
                   with escaped = nil
                   do (cond
                        ((not ch) (croak "Unterminated string or regexp"))
                        (escaped (write-char ch ret)
                                 (setf escaped nil))
                        ((char= ch #\\)
                         (setf escaped t)
                         (when addesc
                           (write-char ch ret)))
                        ((char= ch stop) (return))
                        (t (write-char ch ret))))))

         (read-string ()
           (read-escaped #\" #\" nil))

         (read-symbol-chunk (&optional noerror)
           (let ((sym (read-while (lambda (ch)
                                    (when ch
                                      (not (member ch `(,*token-start*
                                                        ,*token-stop*
                                                        #\( #\) #\[ #\] #\` #\' #\, #\{ #\} #\\ #\| #\"
                                                        #\Space
                                                        #\Newline
                                                        #\Return
                                                        #\Linefeed
                                                        #\Tab
                                                        #\Page
                                                        #\Line_Separator
                                                        #\Paragraph_Separator
                                                        #\NO-BREAK_SPACE))))))))
             (when (and (not noerror)
                        (zerop (length sym)))
               (croak "Apparently can't deal with character ~A" (peek)))
             sym))

         (read-symbol ()
           (let ((sym (read-symbol-chunk)))
             (handler-case
                 (parse-number:parse-number sym)
               (error ()
                 ;; handle dot syntax
                 (let ((path (split-sequence:split-sequence #\. sym)))
                   (cond
                     ((cdr path)
                      `(,(tops "&dot-lookup")
                        ,(tops (car path))
                        ,@(mapcar (lambda (x)
                                    (list (tops "quote")
                                          (tops x)))
                                  (cdr path))))
                     ((char= (char sym 0) #\:)
                      (read-from-string sym))
                     ((string-equal sym "nil") nil)
                     ((string-equal sym "t") t)
                     (t
                      (tops sym))))))))

         (skip-comment ()
           (skip #\;)
           (read-while (lambda (ch)
                         (and ch (not (member ch '(#\Newline #\Line_Separator #\Linefeed)))))))

         (read-until (end &optional out &aux (n (length end)))
           (labels ((rec (i)
                      (when (< i n)
                        (let ((ch (next)))
                          (unless ch (croak "Expecting ~A" end))
                          (when out (write-char ch out))
                          (rec (if (char= ch (char end i))
                                   (1+ i)
                                   0))))))
             (rec 0)))

         (skip-multiline-comment ()
           (read-until "|#"))

         (read-list (*list-terminator* &optional dont-skip-terminator)
           (loop with ret = nil
                 with p = nil
                 with end-char = *list-terminator*
                 do
                    (tagbody
                     again
                       (skip-whitespace)
                       (let ((ch (peek)))
                         (unless ch (croak "Unterminated list"))
                         (cond
                           ((char= ch end-char)
                            (unless dont-skip-terminator
                              (next))
                            (return ret))
                           ((char= ch #\.)
                            (next)
                            (setf (cdr p) (read-token t))
                            (skip-whitespace)
                            (skip end-char)
                            (return ret))
                           (t
                            (let ((tok (read-token)))
                              (if (eq tok 'end-of-list)
                                  (go again)
                                  (let ((cell (list tok)))
                                    (if p
                                        (setf (cdr p) cell)
                                        (setf ret cell))
                                    (setf p cell))))))))))

         (read-quote ()
           (skip #\')
           (list (tops "quote") (read-token t)))

         (read-qq ()
           (skip #\`)
           (incf in-qq)
           (unwind-protect
                (list (tops "quasiquote") (read-token t))
             (decf in-qq)))

         (read-comma ()
           (skip #\,)
           (when (zerop in-qq)
             (croak "Comma outside quasiquote"))
           (cond
             ((char= (peek) #\@)
              (next)
              (list (tops "splice") (read-token t)))
             (t
              (list (tops "unquote") (read-token t)))))

         (read-regexp ()
           (let ((str (read-escaped #\/ #\/ t))
                 (mods (string-downcase (read-while (lambda (ch)
                                                      (member ch '(#\m #\s #\i)))))))
             (ppcre:create-scanner str :case-insensitive-mode (find #\i mods)
                                       :multi-line-mode (find #\m mods)
                                       :single-line-mode (find #\s mods))))

         (read-sharp ()
           (case (peek)
             (#\\
              (next)
              (let ((first (next))
                    (name (read-symbol-chunk t)))
                (read-from-string (format nil "#\\~A~A" first name))))
             (#\/ (read-regexp))
             (#\( (next) (list* (tops "vector") (read-list #\))))
             (#\: (next) (make-symbol (read-symbol)))
             (otherwise (croak "Unsupported sharp syntax #~A" (peek)))))

         (read-token (&optional croak)
           (skip-whitespace)
           (setf tokline line
                 tokcol col)
           (let ((ch (peek)))
             (cond
               ((char= ch *list-terminator*)
                (when croak
                  (croak "Premature list terminator"))
                'end-of-list)
               ((char= ch *token-start*)
                (next)
                (prog1
                    (list* (tops "strcat") (read-text))
                  (skip *token-stop*)))
               ((char= ch #\;) (skip-comment) (read-token croak))
               ((char= ch #\") (read-string))
               ((char= ch #\() (next) (read-list #\)))
               ((char= ch #\[) (next) (read-list #\]))
               ((char= ch #\') (read-quote))
               ((char= ch #\`) (read-qq))
               ((char= ch #\,) (read-comma))
               ((char= ch #\#)
                (next)
                (if (char= (peek) #\|)
                    (progn
                      (next)
                      (skip-multiline-comment)
                      (read-token croak))
                    (read-sharp)))
               (ch (read-symbol)))))

         (read-text-chunk ()
           (setf tokline line
                 tokcol col)
           (with-output-to-string (ret)
             (loop for ch = (peek)
                   do (cond
                        ((not ch) (return ret))
                        ((char= ch #\~)
                         (next)
                         (let ((ch (peek)))
                           (cond
                             ((member ch '(#\Newline #\Linefeed #\Line_Separator))
                              (next))
                             (t (write-char #\~ ret)))))
                        ((char= ch #\\)
                         (next)
                         (let ((ch (peek)))
                           (cond
                             ((eql ch *token-start*)
                              (next) (write-char *token-start* ret))
                             ((eql ch *token-stop*)
                              (next) (write-char *token-stop* ret))
                             ((eql ch #\~)
                              (next)
                              (if (member (peek) '(#\Newline #\Linefeed #\Line_Separator))
                                  (write-char #\~ ret)
                                  (write-string "\\~" ret)))
                             ((and (eql ch #\;) (= col 1))
                              (next) (write-char #\; ret))
                             (t (write-char #\\ ret)))))
                        ((char= ch *token-start*) (return ret))
                        ((char= ch *token-stop*) (return ret))
                        ((and (char= ch #\;) (= col 0))
                         (skip-comment)
                         (next))
                        (t (write-char (next) ret))))))

         (read-directive ()
           (skip #\.)
           (let ((dir (read-symbol-chunk)))
             (cond
               ((string-equal dir "syntax")
                (let ((start (read-string))
                      (stop (read-string)))
                  (skip-whitespace)
                  (skip *token-stop*)
                  (setf *token-start* (aref start 0)
                        *token-stop* (aref stop 0))
                  nil))
               ;; ((string-equal dir "raw")
               ;;  (let ((stop (read-string)))
               ;;    ))
               (t (error "Unrecognized directive “~A”" dir)))))

         (read-text ()
           (loop for ch = (peek) with ret = '()
                 do (cond
                      ((not ch) (return (nreverse ret)))
                      ((char= ch *token-start*)
                       (next)
                       (cond
                         ((char= (peek) #\.)
                          (awhen (read-directive)
                            (push it ret)))
                         ((char= (peek) #\[)
                          (next)
                          (let ((tok (read-list #\])))
                            (skip-whitespace)
                            (skip *token-stop*)
                            (push `(,(tops "progn") ,tok nil) ret)))
                         (t
                          (skip-whitespace)
                          (let* ((esc (or (and (char= (peek) #\\) (next))
                                          (char= (peek) *token-start*))))
                            (skip-whitespace)
                            (unless (char= (peek) *token-stop*)
                              (let ((tok (read-token t)))
                                (tagbody
                                 REPEAT
                                   (skip-whitespace)
                                   (when (char= (peek) #\|)
                                     (next)
                                     (let ((filters (loop until (char= (peek) *token-stop*)
                                                          unless (peek)
                                                            do (croak "Expecting '~C'" *token-stop*)
                                                          collect (prog1 (read-token t) (skip-whitespace)))))
                                       (setf tok (list* (tops "&filter") tok filters))))
                                   (unless (char= (peek) *token-stop*)
                                     (setf tok (if (listp tok)
                                                   (nconc tok (list (read-token t)))
                                                   (list tok (read-token t))))
                                     (go REPEAT)))
                                (push (if esc (list (tops "esc") tok) tok) ret)))
                            (skip-whitespace)
                            (skip *token-stop*)))))
                      ((char= ch *token-stop*)
                       (return (nreverse ret)))
                      (t
                       (push (read-text-chunk) ret))))))

      (list* (tops "strcat") (read-text)))))
