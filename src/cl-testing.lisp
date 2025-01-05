(in-package #:cl-testing)

(defparameter *colors*
  `(:black   ,(format nil "~C[30m" #\Escape)
    :red     ,(format nil "~C[31m" #\Escape)
    :green   ,(format nil "~C[32m" #\Escape)
    :yellow  ,(format nil "~C[33m" #\Escape)
    :blue    ,(format nil "~C[34m" #\Escape)
    :magenta ,(format nil "~C[35m" #\Escape)
    :cyan    ,(format nil "~C[36m" #\Escape)
    :white   ,(format nil "~C[37m" #\Escape)
    :reset   ,(format nil "~C[0m" #\Escape)))

(defmacro cformat (color &rest body)
  `(progn
    (format t "~a" (getf *colors* ,color))
    ,@body
    (format t "~a" (getf *colors* :reset))))

(defmacro describe-group (group-name &body body)
  (let ((group-name-gensym (gensym)))
    `(let ((,group-name-gensym ,group-name))
       (format t "~&~A:~%" ,group-name-gensym)
       (loop for i from 0 to (length (string ,group-name-gensym)) do
             (format t "="))
       (format t "~%")
       ,@body)))

(defmacro it (test-name &body body)
  (let ((test-name-gensym (gensym)))
    `(let ((,test-name-gensym ,test-name))
       (format t "~&~A |> " ,test-name-gensym)
       (handler-case 
           (progn
             ,@body 
             (cformat :green (format t "PASSED~%")))
         (error (e)
           (cformat :red (format t "FAILED: ~A~%" e)))))))

(describe-group "A group of mathematics tests" 
  (it "should add two numbers" 
    (assert (= (+ 3 4) 6)))

  (it "should subtract two numbers"
    (assert (= (- 3 4) -1)))
  )
