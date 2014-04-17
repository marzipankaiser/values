;;;;; VALUES library ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  by marzipankaiser (marzipankaiser@gmail.com)
;  Dependencies: 
;  - closer-mop (http://common-lisp.net/project/closer/closer-mop.html)
;  Allows to make variables and slots that change behavior
;  depending on their values (so, using special values you can
;  e.g. make lazy values)
; License: see LICENSE file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage #:values 
  (:use :cl :closer-mop)
  (:shadowing-import-from :closer-mop 
			  #:defmethod
			  #:slot-value-using-class
			  #:defgeneric
			  #:standard-generic-function)
  (:export #:get-value #:set-value ; value protocol

	   #:defvar-ext #:defparameter-ext ; variable definition
	   #:let-ext #:let*-ext            ;  / binding

	   #:value-quote #:value-unquote ; suppress expansion
	   #:setf-ext ; value-quote'd assignment
	   
	   #:metaclass-ext-mixin ; metaclasses for values in slots
	   #:standard-class-ext

	   ;;;; Default value types
	   #:lazy
	   #:traced #:traced-value-op #:traced-value-new))
