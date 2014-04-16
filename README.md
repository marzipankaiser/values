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

