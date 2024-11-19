(in-package #:cl-testing)

(defmacro describe-group (group-name &body body)
  (let ((group-name-gensym (gensym)))
    `(let ((,group-name-gensym ,group-name))
       (format t "~&~A:~%" ,group-name-gensym)
       (loop for i from 0 to (length (string ,group-name-gensym)) do
             (format t "="))
       (format t "~%")
       ,@body)))

(describe-group "A group of mathematics tests"
    (+ 3 3))
