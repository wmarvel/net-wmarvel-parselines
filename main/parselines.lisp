;;;; net-wmarvel-parselines.lisp

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

(defvar *format-specification-scanner* (create-scanner "\\?<\\S+>"))

;;; BINDINGS - represented as either a single binding node, or a list of them
(defmethod make-binding-node ((names symbol) (values string))
  (make-instance 'binding-node
		 :names (vector names)
		 :values (vector values)))

(defmethod make-binding-node ((names vector) (values vector))
  (make-instance 'binding-node
		 :names names
		 :values values))

(defmethod get-binding ((name symbol) (bindings binding-node))
  (with-slots (binding-names binding-values) bindings
    (map 'nil
	 #'(lambda (var value)
	     (when (eq name var) (return-from get-binding value)))
	 binding-names
	 binding-values)))

(defmethod get-binding ((name symbol) (bindings list))
  (dolist (binding bindings (error "Unable to find ~A in bindings" name))
    (let ((value (get-binding name binding)))
      (when value (return-from get-binding value)))))

(defun strings->symbolvector (list package)
  (map 'vector
       #'(lambda (string)
	   (intern (string-upcase string) package))
       list))

(defun formatspec->controlstring (spec)
  (regex-replace-all *format-specification-scanner* spec "~A"))

(defun namedregister->symbol (register)
  (intern (string-upcase (subseq register 2 (- (length register) 1)))))

(defun formatspec->slotnames (spec)
  (mapcar #'namedregister->symbol 
	  (all-matches-as-strings *format-specification-scanner* spec)))

(defun formatspec->formatdata (spec)
  `(,(formatspec->controlstring spec) ,@(formatspec->slotnames spec)))

(defmethod shared-initialize :after 
    ((object string-scanner) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignorable slot-names initargs))
  (let ((*allow-named-registers* T))
    (with-slots (scanner registers pattern) object
      (multiple-value-bind (regexp regstrings) (create-scanner pattern)
	(setf scanner regexp
	      registers (strings->symbolvector regstrings *package*)))))
  object)

(defmethod make-load-form ((object string-scanner) &optional environment)
  (declare (ignorable environment))
  `(make-instance ',(class-of object)
		  :pattern ,(pattern object)))

(defun make-string-scanner (regex &optional (package *package*))
  (make-instance 'string-scanner :pattern regex :package package))

(defun repackage-symbol (symbol)
  (intern (symbol-name symbol) *package*))
  
(defun make-parser (rootclass &key
		      (class 'parser)
		      (rootvariable (repackage-symbol 'original-line))
		      (reordercount 100))
  (let ((rootclass (etypecase rootclass
		     (symbol (find-class rootclass))
		     (T rootclass))))
    (make-instance class 
		   :rootclass rootclass
		   :rootvariable rootvariable
		   :reordercount reordercount)))

(defmethod scan-string ((scanner string-scanner) (string string))
  "Scan a strings. Returns nil if we fail to match, and either T
   or any bindings if we succeed."
  (with-slots (scanner registers) scanner
    (if (= 0 (length registers))
	(when (scan scanner string) T)
	(multiple-value-bind (match values) (scan-to-strings scanner string)
	  (when match
	    (make-binding-node registers values))))))

(defun newinstance (class bindings)
  (loop
     with instance = (make-instance (class-name class))
     for binding in bindings
     do (with-slots (binding-names binding-values) binding
	  (map 'nil
	       #'(lambda (name value)
		   (when (slot-exists-p instance name)
		     (setf (slot-value instance name) value)))
	       binding-names
	       binding-values))
     finally (return instance)))

(defmethod hitcount+ ((class parser-class))
  (with-slots (hitcount) class
    (setf hitcount (1+ hitcount))))

(defmethod class-matches ((class parser-class) (bindings list))
  "class-matches (class bindings) - Semipredicate to determine if a
   class matches using the given bindings. Returns either the bindings
   required to recusively match subclasses, or nil if the class does
   not match"
  (loop
     for (name . scanners) in (parsespec class)
     with newbinds = nil
     do (loop named pick-one-scanner
	   for scanner in scanners
	   with checkbinds = (or newbinds bindings)
	   do (let ((matchbinds
		     (scan-string scanner (get-binding name checkbinds))))
		(when matchbinds
		  (if (eq matchbinds T)
		      (when (null newbinds) (setf newbinds checkbinds))
		      (setf newbinds (add-bindings matchbinds checkbinds)))
		  (return-from pick-one-scanner t))))
       finally (return newbinds)))

(defmethod make-best-instance (class (bindings binding-node))
  (make-best-instance class (list bindings)))

(defmethod make-best-instance ((class skip-parser-class) (bindings list))
  ;;; Skip matching on this class, just match the children.
  (dolist (subclass (ordered-subclasses class))
    (let ((best (make-best-instance subclass bindings)))
      (when best (hitcount+ class) (return best)))))

(defmethod make-best-instance ((class parser-class) (bindings list))
  (let ((matchbinds (class-matches class bindings)))
    (when matchbinds
      (hitcount+ class)
      (dolist (subclass (ordered-subclasses class) (newinstance class matchbinds))
	(let ((best (make-best-instance subclass matchbinds)))
	  (when best (return best)))))))

(defmethod parse-instance ((parser parser) (line string))
  (with-slots (rootclass rootvariable parsecount reordercount) parser
    (when (= 0 (mod parsecount reordercount))
      (update-class-tree-ordering rootclass))
    (setf parsecount (1+ parsecount))
    (make-best-instance rootclass (make-binding-node rootvariable line))))

(defmethod parse-instance ((parser parser) (stream stream))
  (let ((line (read-line stream nil)))
    (and line (parse-instance parser line))))


(defmethod sorted-subclasses ((class parser-class))
  (stable-sort (copy-seq (class-direct-subclasses class))
	       #'> :key #'hitcount))

(defmethod update-class-ordering ((class standard-class)))

(defmethod update-class-ordering ((class parser-class))
  (let ((sorted (sorted-subclasses class)))
    (setf (ordered-subclasses class) sorted)))

(defmethod update-parent-class-ordering ((class parser-class))
  "Update the ordered-subclasses of our parent class"
  (dolist (parent (class-direct-superclasses class))
    (update-class-ordering parent)))

(defmethod update-class-tree-ordering ((class T))
  "Do-nothing method to terminate the recursion")

(defmethod update-class-tree-order ((class parser-class))
  "Update the ordered-subclasses for a class and all subclasses"
  (dolist (child (update-class-ordering class))
    (update-class-tree-ordering child)))
  
  
    
  


				     
			     
  
  
  
  


