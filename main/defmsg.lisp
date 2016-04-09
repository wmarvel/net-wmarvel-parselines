;;;; defmsg.lisp

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

;;; Giving defmsg it's own file so some small utility functions can
;;; be written to make it a little bit cleaner.

;;; A defmsg form should look something like this
;;;(defmsg foo (msg)
;;;  ;;; Slots that are uninvolved in the parsing are specified exactly
;;;  ;;; the same way they would be with defclass
;;;  ((unparsed-slot :accessor unparsed-slot
;;;                  :initform :unparsed-slot
;;;		     :initform (make-hash-table)))
;;;  ;;; The parser specification here will cause the class to have 
;;;  ;;; four additional slots -> parsed-slot, digit1, digit2, digit3
;;;  ;;; We are using cl-ppcre regular expressions, with the variable
;;;  ;;; *allow-named-registers* set to t so that we can pull the 
;;;  ;;; named registers out as slot names
;;;  ;;; If there is no :parser-specification, then the class is
;;;  ;;; matches during parsing if, and only if, some child class
;;;  ;;; matches.
;;;  (:parse-specification
;;;   (parsed-slot "^(?<digit1>\\d)(?<digit2>\\d)(?<digit3>\\d)$"))
;;;   (another "^(?<foonumber>FOO\\d+)$"))
;;;  ;;; The formatting portion of the code has not yet been written,
;;;  ;;; but should basically be a slot name, a format string, and then
;;;  ;;; the slot names used to fill that format string
;;;  (:format-specification
;;;   (parsed-slot "?<digit1>?<digit2>?<digit3>")))

(defvar *defmsg-options*
  '(:parse-specification :format-specification :metaclass))

(defvar *metaclass-warning*
  "Removing the :metaclass class option from  (defmsg ~A ....)"
  "The warning message to give when we find a :metaclass class option in a
   defmsg form")

(defun parsespec->slotnames (parsespec)
  "Converts the a parsespec form, which must have already been compiled
   with compile-parsespec, into the slot names specified (either directly
   or as part of the pattern) by the parsespec"
  (labels ((slotnames-node (node)
	     (etypecase node
	       (symbol node)
	       (list (map 'list #'slotnames-node node))
	       (string-scanner (map 'list #'identity (registers node))))))
    (remove-duplicates (flatten (mapcar #'slotnames-node parsespec)))))


(defun compile-parsespec (parsespec)
  "Converts the strings inside the parsespec of a parser-class metaclass into
   string-scanner objects"
  (labels ((compile-node (node)
	     (etypecase node
	       (symbol node)
	       (list (mapcar #'compile-node node))
	       (string (make-string-scanner node)))))
    (mapcar #'compile-node parsespec)))

;;;   (parsed-slot "?<digit1>?<digit2>?<digit3>" ...)))
(defun compile-formatspec (formatspec)
  (labels ((compile-node (node)
	     (etypecase node
	       (symbol node)
	       (list (mapcar #'compile-node node))
	       (string (formatspec->formatdata node)))))
    (mapcar #'compile-node formatspec)))

(defun slotnames->slotspecs (slotnames)
  "Converts the slot names (as returned by parsespec->slotnames) into
   a list of slot specifiers suitable for defclass"
  (mapcar #'(lambda (slotname)
	      `(,slotname
		:accessor ,slotname
		:initarg ,(intern (symbol-name slotname) 'keyword)
		:initform nil))
	  slotnames))

(defun options->defclassoptions (name options)
  "Return the class options for defclass, removing the defmsg options"
  (remove-if
   #'(lambda (option) 
       (let ((optionkey (first option)))
	 (when (eq :metaclass optionkey)
	   (warn *metaclass-warning* name))
	 (find optionkey *defmsg-options*)))
   options))

(defun options->parsespec (options)
  (rest (assoc :parse-specification options)))

(defun options->formatspec (options)
  (rest (assoc :format-specification options)))

;;; defmsg is exactly like defclass except it has additional syntax
;;; so that the specification of how to parse is included, as well
;;; as exporting the parsed slots.
(defmacro defmsg (name extends slot-specifiers &rest class-options)
  (let* ((parsespec 
	  (compile-parsespec (options->parsespec class-options)))
	 (formatspec
	  (compile-formatspec (options->formatspec class-options)))
	 (slotnames (parsespec->slotnames parsespec)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name ,extends
	 ,(append (slotnames->slotspecs slotnames) slot-specifiers)
	 (:metaclass ,(if (null parsespec) 'skip-parser-class 'parser-class))
	 ,@(options->defclassoptions name class-options))
       (let ((class (find-class (quote ,name))))
	 (setf (parsespec class) (quote ,parsespec))
	 (setf (formatspec class) (quote ,formatspec))
	 ;;; We don't want to do full tree updates while defining classes
	 ;;; so just do it for the level above and the class we just defined.
	 (update-class-ordering class)
	 (update-parent-class-ordering class))
       (dolist (sym (quote ,(cons name slotnames)))
	 (export sym))
       (find-class ',name))))


;;(defmsg msg () () (:parse-specification (original-line "^.*$")))
;;;(defmsg testmsg (msg) () (:parse-specification (original-line "^(?<date>(?<day>\\d+)/(?<month>)/(?<year>))$")))
