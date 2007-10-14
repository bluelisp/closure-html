;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Package definitions for the html parser packages
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :cl-user)

(defpackage :html-glisp
  (:use :cl)
  (:export "ALWAYS"
           "CL-BYTE-STREAM"
           "CL-CHAR-STREAM"
           "CL-STREAM"
           "COMPOSE"
           "CURRY"
           "G/CLOSE"
           "G/FINISH-OUTPUT"
           "G/PEEK-CHAR"
           "G/READ-BYTE"
           "G/READ-BYTE-SEQUENCE"
           "G/READ-CHAR"
           "G/READ-CHAR-SEQUENCE"
           "G/READ-LINE"
           "G/READ-LINE*"
           "G/UNREAD-BYTE"
           "G/UNREAD-CHAR"
           "G/WRITE-BYTE"
           "G/WRITE-BYTE-SEQUENCE"
           "G/WRITE-CHAR"
           "G/WRITE-STRING"
           "GSTREAM"
           "MULTIPLE-VALUE-OR"
           "RCURRY"
           "UNTIL"
           "USE-BYTE-FOR-CHAR-STREAM-FLAVOUR"
           "USE-CHAR-FOR-BYTE-STREAM-FLAVOUR"
           "WHILE"
           "WHITE-SPACE-P"

           "CL-BYTE-STREAM->GSTREAM"
           "CL-CHAR-STREAM->GSTREAM"

           "MAYBE-PARSE-INTEGER"))

(defpackage :closure-mime-types
  (:use :cl :html-glisp ;; white-space-p
        )
  (:export #:parse-mime-content-type    ;### yet to be defined
	   #:find-mime-type-from-extension
	   #:mime-type-name
           #:find-mime-type
	   #:mime-type-equal
	   #:mime-type-extensions))

(defpackage :sgml
  (:use :cl :html-glisp :runes)
  (:export #:SGML-PARSE 
	   #:PT
           #:PPT 
           #:SGML-UNPARSE 
           #:PARSE-DTD
           #:*OPTIONS/PARSER-SILENT-P*
           #:PT-NAME 
           #:PT-CHILDREN 
           #:PT-PARENT 
           #:PT-ATTRS 
           #:SLURP-CATALOG
           ;; in pt-utils:
           #:map-pt
           #:pt-cdata
           #:pt-attr
           #:pt-root
           #:pt-root-property
           #:gi
           #:flat-find-element
           #:flat-find-elements
           #:pt-full-name-path
           #:lhtml->pt
           ;;
           #:html-parse-file
           ))

(defpackage :closure-html
  (:use :cl :runes)
  (:nicknames :chtml)
  (:use :sgml)
  (:export #:*html-dtd*
	   #:parse

	   #:make-octet-vector-sink
	   #:make-octet-steam-sink
	   #:make-rod-sink
	   #+rune-is-character #:make-character-stream-sink
	   #-rune-is-character #:make-string-sink/utf8
	   #-rune-is-character #:make-character-stream-sink/utf8
	   #+rune-is-character #:make-string-sink

	   #:with-html-output
	   #:with-output-sink
	   #:with-element
	   #:attribute
	   #:text
	   #:comment

	   #:make-pt-builder
	   #:pt
	   #:pt-name
	   #:pt-children
	   #:pt-parent
	   #:pt-attrs

	   #:make-lhtml-builder))
