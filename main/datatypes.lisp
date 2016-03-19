;;;; datatypes.lisp

;;  Copyright (C) 1999-2016 Wendall A. Marvel

;;  This library is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU Lesser General Public
;;  License as published by the Free Software Foundation; either
;;  version 2.1 of the License, or (at your option) any later version.

;;  This library is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  Lesser General Public License for more details.

;;  You should have received a copy of the GNU Lesser General Public
;;  License along with this library; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

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

