;;;;; VALUES library ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  by marzipankaiser (marzipankaiser@gmail.com)
;  Dependencies: 
;  - closer-mop (http://common-lisp.net/project/closer/closer-mop.html)
;  Allows to make variables and slots that change behavior
;  depending on their values (so, using special values you can
;  e.g. make lazy values)
; License: see LICENSE file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(asdf:defsystem #:values
  :depends-on (:closer-mop)
  :description "Make variables change get/set behavior depending on value."
  :version "0.1"
  :author "marzipankaiser <marzipankaiser@gmail.com>"
  :licence "MIT License"
  :serial T
  :components ((:file "package")
	       (:file "utils")
	       (:file "values")))
