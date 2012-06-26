(in-package #:sytes)

(defmacro with-rebinds (names &body body)
  (let ((gensyms (mapcar (lambda (_)
                           (declare (ignore _))
                           (gensym)) names)))
    `(let (,@(mapcar (lambda (g) `(,g (gensym))) gensyms))
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ,(let (,@(mapcar (lambda (n g) `(,n ,g)) names gensyms))
             ,@body)))))

