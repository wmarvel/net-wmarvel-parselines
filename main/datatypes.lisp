;;;; datatypes.lisp

;; Copyright (c) 1999-2016 Wendall A. Marvel 

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
(in-package #:net-wmarvel-parselines)

(defclass binding-node ()
  ((binding-names :accessor binding-names
	 :initarg :names
	 :initform nil)
   (binding-values :accessor binding-values
	   :initarg :values
	   :initform nil)))

(defgeneric make-binding-node (names values)
  (:documentation "Create a new binding node"))

(defgeneric get-binding (name bindings)
  (:documentation "Return the value bound to name in bindings"))

(defclass string-scanner ()
  ((pattern :accessor pattern
	    :initarg :pattern
	    :initform nil)
   (scanner :accessor scanner
	    :initarg :scanner
	    :initform nil)
   (registers :accessor registers
	      :initarg :registers
	      :initform nil)))

(defclass parser ()
  ((rootclass
    :accessor rootclass
    :initarg  :rootclass
    :initform nil)
   (rootvariable
    :accessor rootvariable
    :initarg :rootvariable
    :initform 'original-line)
   (parsecount
    :accessor parsecount
    :initarg :parsecount
    :initform 0)
   (reordercount
    :accessor reordercount
    :initarg :reordercount
    :initform 1000))) 

(defgeneric parse-instance (parser object)
  (:documentation
   "Parse an object into a new instance. The object may be a stream or string"))
 

(defclass parser-class
    #+(or :allegro :lispworks) (standard-class) 
    #+cmu (pcl::standard-class)
    #+sbcl (sb-pcl::standard-class)
    ((parsespec ;; list of (slotname scanner ...)
      :accessor parsespec
      :initarg  :parsespec
      :initform nil)
     (formatspec
      :accessor formatspec
      :initarg :formatspec
      :initform nil)
     (hitcount
      :accessor hitcount
      :initarg :hitcount
      :initform 0)
     (ordered-subclasses
      :accessor ordered-subclasses
      :initarg  :ordered-subclasses
      :initform nil)))

;; Skip over this class in any metaclass recursion
(defclass skip-parser-class (parser-class) ())

;; Needed because cmucl has what
;; http://www.cons.org/cmucl/doc/pcl-mop-hints.html calls 'Class Schizophrenia'
;; Note that this package may not work on CMUCL depending on whether or not
;; they've fixed define-method-combination
#+cmu
(defmethod pcl:validate-superclass 
    ((class parser-class) (superclass pcl::standard-class))
  T)

;; And sbcl has it's own package
#+sbcl
(defmethod sb-pcl:validate-superclass 
    ((class parser-class) (superclass sb-pcl::standard-class))
  T)

;; And Lispworks has it's own package, and wants the args reversed...
#+lispworks
(defmethod clos:validate-superclass
   ((class standard-class) (superclass parser-class))
  T)

