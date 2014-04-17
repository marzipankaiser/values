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

;;; Called when value of variable should be gotten
(defgeneric get-value (var)
  (:method (var) var)) ; default: return value unchanged

;;; Called when value of variable should be set
;;;  calling setter-fun sets the variable (as if it where normal)
;;;  var is the old value of the variable
;;;  new-values is a list of the new values
;;;   (usually only one. =values of the variables that are
;;;                       the third return value of get-setf-expansion)
(defgeneric set-value (var setter-fun new-values)
  (:method (var setter-fun new-values) (declare (ignore var new-values))
    (funcall setter-fun))) ; default: set variable

;;; (setf (get-value X) ...) expands to something like
;;;   (set-value X (lambda () (setf X ...)))
(define-setf-expander get-value (var &environment env)
  (multiple-value-bind (dummies vals newval setter getter) 
      (get-setf-expansion var env)
    (values dummies 
	    vals 
	    newval 
	    `(set-value ,var 
			(lambda (&optional
				   ,@(mapcar (lambda (x)
					       `(,x ,x)) 
					     newval)) 
			  ,setter)
			(list ,@newval)) 
	    `(get-value ,getter))))

;;; special variable. if set, get-value will ALWAYS be equivalent to
;;;  the identity function
;;;  (unless you specified an :around method for get-value)
(defvar *value-quote-activated* NIL)
(defmethod get-value :around (val)
  (if *value-quote-activated* 
      val
      (call-next-method)))

;;; Sets *value-quote-activated* (see above)
;;;  [efficiency addition: 
;;;   (value-quote (get-value X)) = X ]
(defmacro value-quote (val &environment env)
  (let ((res (macroexpand-1 val env)))
    (if (and (consp res) 
	     (eq (car res) 'get-value)
	     (symbolp (cadr res)))
	(cadr res)
	`(let ((*value-quote-activated* T)) ,res))))

;;; Reverse of value-quote
(defmacro value-unquote (val)
  `(let ((*value-quote-activated* NIL)) (get-value ,val)))

;;; setf with value-quote'd values
(defmacro setf-ext (&rest name-value-pairs)
  `(setf ,@(do* ((name-d name-value-pairs (cddr name-d))
		 (value-d (cdr name-value-pairs) (cdr name-d))
		 (res NIL))
		((null name-d) (nreverse res))
		(push (car name-d) res)
		(push `(value-quote ,(car value-d)) res))))

;;;; Macro definition helpers
;;; Returns required variable and symbol-macro definition
;;;  [as 2 return values:
;;;    - variable definition (VAR-NAME VALUE)
;;;    - symbol-macro def. (SYMBOL-MACRO-NAME SYMBOL-MACRO-VALUE) ]
(defun ext-expansion (name value)
  (let ((gs (gensym)))
    (values `(,gs (value-quote ,value))
	    `(,name (get-value ,gs)))))

;;; like ext-expansion but with input and output
;;;  for [symbol-macro]let 
(defun ext-expansions (bindings)
  (let ((res-1 NIL)
	(res-2 NIL)) 
    (dolist (binding bindings)
      (multiple-value-bind (var- symbolmacro-) 
	  (if (consp binding) 
	      (ext-expansion (car binding) (cadr binding))
	      (ext-expansion binding NIL))
	(push var- res-1)
	(push symbolmacro- res-2)))
    (values (nreverse res-1)
	    (nreverse res-2))))

;;;; global variable definitions
;;; Helper macro
(defmacro def-global-definer (name)
 `(defmacro ,(sym name '-ext) (name &optional val doc)
    (multiple-value-bind (defvar-args symbolmacro-args) 
	(ext-expansion name val)
      `(progn
	 (,',name ,@defvar-args ,doc)
	 (define-symbol-macro ,@symbolmacro-args)))))
;;; defvar-ext
(def-global-definer defvar)
;;; defparameter-ext
(def-global-definer defparameter)

;;;; let and variants
(defmacro let-ext (bindings &body body)
  (multiple-value-bind (let-bindings symbol-macrolet-bindings)
      (ext-expansions bindings)
    `(let ,let-bindings
       (symbol-macrolet ,symbol-macrolet-bindings
	 ,@body))))
(defmacro let*-ext (bindings &body body)
  (multiple-value-bind (let-bindings symbol-macrolet-bindings)
      (ext-expansions bindings)
    (labels ((expand-let*-ext (let-bs sm-bs)
	       (if (null let-bs) 
		   `(progn ,@body)
		   `(let (,(car let-bs))
		      (symbol-macrolet (,(car sm-bs))
			,(expand-let*-ext (cdr let-bs) (cdr sm-bs)))))))
      (expand-let*-ext let-bindings symbol-macrolet-bindings))))

;;;; Class Slots (metaclass-mixin & standard metaclass)
;;; Metaclass (MOP) mixin class
;;;  to make slots be able to use values, use a 
;;;  metaclass that inherits from this
(defclass metaclass-ext-mixin () ())
;;; Getter for values in slots
(defmethod slot-value-using-class :around
    ((class metaclass-ext-mixin) object slot)
  (if *value-quote-activated* 
      (call-next-method)
      (get-value (call-next-method))))
(defmethod (setf slot-value-using-class) :around
    (new-val (class metaclass-ext-mixin) obj slot)
  (if (slot-boundp-using-class class obj slot) 
      (set-value (value-quote (slot-value-using-class class obj slot))
		 (lambda (&optional (new-val new-val)) 
		   (call-next-method new-val class obj slot))
		 (list new-val))
      (call-next-method)))

;;; Predefined combination of standard-class & metaclass-ext-mixin
(defclass standard-class-ext (metaclass-ext-mixin standard-class) ())
(defmethod validate-superclass ((class standard-class-ext)
				(superclass standard-class))
  T)

;;;;; Default value classes
;;;;;-----------------------

;;;; Lazy values
(defclass lazy-value () 
  ((function :initarg :function :accessor lazy-value-function)))
(defmethod get-value ((var lazy-value))
  (get-value (funcall (lazy-value-function var))))

;;; Usage: (lazy ...)
;;;  Evaluates ... if get-value is called on it / it is read from
;;;   a variable bound using a *-ext form
;;;   and it is not value-quote'd (see above)
(defmacro lazy (val)
  `(make-instance 'lazy-value 
		  :function (lambda () ,val)))


;;;; Traced values
(defclass traced-value ()
  ((trace-function :initarg :trace-function
		   :accessor traced-value-trace-function)
   (value :initarg :value :accessor traced-value-value)))
(defmethod get-value ((val traced-value))
  (funcall (traced-value-trace-function val) :get)
  (get-value (traced-value-value val)))
(defmethod set-value ((val traced-value) setter new-vals)
  (funcall (traced-value-trace-function val) :set (car new-vals))
  (setf (traced-value-value val) (car new-vals)))

;;; Makes a value for that
;;;  trace-forms are evaluated before it is read or written
;;;  with:
;;;   - traced-value-op set to
;;;        * :set if it is written
;;;        * :get if it is read
;;;   - traced-value-new set to
;;;        * the new value if it is written
;;;        * NIL if it is read
(defmacro traced (value &body trace-forms)
  `(make-instance 'traced-value
		  :value ,value
		  :trace-function
		  (lambda (traced-value-op 
			   &optional (traced-value-new NIL))
		    ,@trace-forms)))
