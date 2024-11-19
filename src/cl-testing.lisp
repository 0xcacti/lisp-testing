(in-package #:cl-testing)

(defmacro describe-group (group-name &body body)
  (let ((group-name-gensym (gensym)))
    `(let ((,group-name-gensym ,group-name))
       ,@body)))





