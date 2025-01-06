(in-package #:cl-testing)

(defun get-color-code (color)
    (case color
      (:black   (format nil "~C[30m" #\Escape))
      (:red     (format nil "~C[31m" #\Escape))
      (:green   (format nil "~C[32m" #\Escape))
      (:yellow  (format nil "~C[33m" #\Escape))
      (:blue    (format nil "~C[34m" #\Escape))
      (:magenta (format nil "~C[35m" #\Escape))
      (:cyan    (format nil "~C[36m" #\Escape))
      (:white   (format nil "~C[37m" #\Escape))
      (:reset   (format nil "~C[0m" #\Escape))
      (t        (error "Unknown color code ~A" color))))

(defun supports-ansi-p ()
  "More comprehensive ANSI support check"
  (and 
   #+(or sbcl ccl ecl)
   (interactive-stream-p *standard-output*)
   (or (uiop:getenv "COLORTERM")
       (search "256color" (or (uiop:getenv "TERM") ""))
       (search "xterm" (or (uiop:getenv "TERM") "")))
   (not (or (equal (uiop:getenv "TERM") "dumb")
            (search "nvlime" (or (uiop:getenv "EDITOR") ""))))
   #-(or sbcl ccl ecl)
   nil))

(defmacro cformat (color &rest body)
  `(progn
    (if (supports-ansi-p)
        (format t "~a" (get-color-code ,color)))
    ,@body
    (if (supports-ansi-p)
        (format t "~a" (get-color-code :reset)))))


(defvar *before-all-hooks* nil)
(defvar *after-all-hooks* nil)
(defvar *before-each-hooks* nil)
(defvar *after-each-hooks* nil)

(defmacro before-all (&body body)
  `(push (lambda () ,@body) *before-all-hooks*))

(defmacro after-all (&body body)
  `(push (lambda () ,@body) *after-all-hooks*))

(defmacro before-each (&body body)
  `(push (lambda () ,@body) *before-each-hooks*))

(defmacro after-each (&body body)
  `(push (lambda () ,@body) *after-each-hooks*))

(defmacro describe-group (group-name &body body)
  (let ((group-name-gensym (gensym))
         (tests-gensym (gensym)))
    `(let ((,group-name-gensym ,group-name)
           (*before-all-hooks* nil)
           (*after-all-hooks* nil)
           (*before-each-hooks* nil)
           (*after-each-hooks* nil)
           (,tests-gensym nil))

       (flet
           ((it (name &rest body)
              (push (list name body) ,tests-gensym))
            (skip-it (name &rest body)
              (push (list :skip name) ,tests-gensym)))
            ,@body)

       (format t "~&~A:~%" ,group-name-gensym)
       (loop for i from 0 to (length (string ,group-name-gensym)) do
             (format t "="))
       (format t "~%")
       ;; Execute all hooks and body, return body result
       (mapc #'funcall (reverse *before-all-hooks*))
       (dolist (test (reverse ,tests-gensym))
         (if (eq (first test) :skip)
             (skip-it (second test) nil)
             (it (first test) 
               (eval (second test))))
       (mapc #'funcall (reverse *after-all-hooks*))))))


(defmacro it (test-name &body body)
  (let ((test-name-gensym (gensym)))
    `(let ((,test-name-gensym ,test-name))
       (format t "~&~A |> " ,test-name-gensym)
       (handler-case 
           (progn
             (mapc #'funcall *before-each-hooks*)
             ,@body 
             (cformat :green (format t "PASSED~%")))
         (error (e)
           (cformat :red (format t "FAILED: ~A~%" e))))
       (mapc #'funcall *after-each-hooks*))))

(defmacro skip-it (test-name &body body)
  (let ((test-name-gensym (gensym)))
    `(let ((,test-name-gensym ,test-name))
       (format t "~&~A |> " ,test-name-gensym)
       (cformat :yellow (format t "SKIPPED~%")))))

(describe-group "A group of mathematics tests" 

  (before-all 
    (format t "Before all tests~%"))

  (before-each 
    (format t "Before each test~%"))

  (after-each 
    (format t "After each test~%"))

  (after-all 
    (format t "After all tests~%"))

  (it "should add two numbers" 
    (assert (= (+ 3 4) 6)))

  (it "should subtract two numbers"
    (assert (= (- 3 4) -1)))

  (skip-it "should multiply two numbers"
    (assert (= (* 3 4) 12)))
  )


;; DONE 

;; describe - You already have this with describe-group
;; test/it - You already have this

;; LEFT TO DO 

;; skip-it - I could figure out how to do this ez
;; only-it - I have no idea how to do this
;; beforeAll/afterAll - Runs once before/after all tests in a describe block
;; beforeEach/afterEach - Runs before/after each test case
;; afterAll.unordered() - For cleanup that can run in any order

;; expect - For assertions (you're using CL's assert) ;; I don't quite understand all the to be stuff
