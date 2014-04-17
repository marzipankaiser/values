;;;;; VALUES library ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  by marzipankaiser (marzipankaiser@gmail.com)
;  Dependencies: 
;  - closer-mop (http://common-lisp.net/project/closer/closer-mop.html)
;  Allows to make variables and slots that change behavior
;  depending on their values (so, using special values you can
;  e.g. make lazy values)
; License: see LICENSE file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:values)

(defun mk-string (&rest args)
  (with-output-to-string (*standard-output*)
    (dolist (arg args)
      (princ arg))))
(defun sym (&rest args)
  (intern (apply 'mk-string args)))
(defmacro in ((value &key (test 'eq)) &rest values)
  (let ((gs (gensym)))
    `(let ((,gs ,value)) 
       (or ,@(mapcar (lambda (x) `(,test ,gs ,x))
		     values)))))
