values
======

Common Lisp library that allows to make variables and slots that change behavior depending on their values (so, using special values you can e.g make lazy values)

[Please notice that I'm currently kind of a Common Lisp newb, so the code probably isn't exactly good...]

Basic usage
===========

	(defvar-ext *a* (let ((x 0)) (lazy (incf x))))
	*a* => 1
	*a* => 2
	...

When you use a variable defined using one of `defvar-ext`, `defparameter-ext`, `let-ext` or `let*-ext`, those will actually be symbol-macrolets (_Notice:_ They will not be `boundp`) and the get-value generic function will be called on the value (this defaults to identity) and set-value will be called when setting them (for the arguments of set-value see values.lisp). Thus, you can have something like variables that can be lazily evaluated or like above counters.

values can also be used to trace variables, like this:

    (defvar-ext *b* (traced 2	; initial value
			   (format T "*b*: ~A ~:[to ~A~;~]" ;on set/get
				   traced-value-op
				   (eq traced-value-op :get)
				   traced-value-new)))
    
    *b*
    -> *b*: GET
    => 2
    
    (setf *b* 3)
    -> *b*: SET to 3
    => 3
    
    *b*
    -> *b*: GET
    => 3

Value quotation
===============

If you want to get at the *real* value of the variable, i.e. the value object, you can use `value-quote` like this:
    
    (defvar-ext *b* (lazy 2))
    *b* => 2
    (value-quote *b*) => #<VALUES::LAZY-VALUE ...>

All `*-ext` forms implicitly `value-quote` their arguments (the initial values). This includes `setf-ext` which is meant for this purpose (You can use `setf` instead if you don't need `value-quote`'d values). To undo `value-quote` you can simply use `value-unquote`.


Defining own value classes
==========================

If you want your own class to affect the behavior of the variable defined using a `*-ext` form, you can specialize on the following methods:

<pre><b>get-value</b> value</pre>
 is called when the value of the variable is read. value is the current value of the variable (i.e. an instance of your own class).


<pre><b>set-value</b> value setter-function new-values</pre> 
 is called when the value of the variable is written (using `setf` or `setf-ext`). It takes these arguments:

 * `value` - this is the current value (*before* setting) of the variable (i.e. an instance of your own class)
 * `setter-function` - this is a function that, if called, sets the variable to the new value. It takes as many optional arguments as there are elements in `new-values` that default to the values in `new-values` (and can be used to change the value that is to be set)
 * `new-values` - This is a list of the new values of the variable. (This will usually be only one. This is the list of the values of the variables returned as the third return value of `get-setf-expansion`)

Further explanation
===================
Further explanation can be found in `values.lisp` (as comments) and may be added here somewhen...
