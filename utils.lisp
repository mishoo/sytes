(in-package #:sytes)

(defmacro with-rebinds (names &body body)
  (let ((gensyms (mapcar (lambda (_)
                           (declare (ignore _))
                           (gensym)) names)))
    `(let (,@(mapcar (lambda (g) `(,g (gensym))) gensyms))
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ,(let (,@(mapcar (lambda (n g) `(,n ,g)) names gensyms))
             ,@body)))))

(defun read-whole-file-utf8 (path)
  (with-open-file (s path :element-type 'unsigned-byte)
    (let* ((len (file-length s))
           (data (make-array len :element-type 'unsigned-byte)))
      (read-sequence data s)
      (trivial-utf-8:utf-8-bytes-to-string data))))

(defun starts-with (seq1 seq2)
  (and (>= (length seq1) (length seq2))
       (loop for c1 across seq1
             for c2 across seq2
             always (eq c1 c2))))
