;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SGML; Readtable: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Documentation strings for the Closure HTML
;;;   Created: 2007-10-20
;;;    Author: David Lichteblau
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2007 David Lichteblau

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

(in-package :closure-html)

(setf (documentation 'parse 'function)
      "@arg[input]{a pathname, stream, string, or octet array}
       @arg[handler]{nil, or a HAX/SAX handler}
       @return{The return value of this function is determined by the
         @var{handler} argument; see below.}
       @short{Parse the HTML document given as an argument, or referred to
       using a pathname.}

       @var{input} can have one of the following types:
       @begin{itemize}
       @item{pathname -- a Common Lisp pathname. Opens the file specified by
         the pathname parses it as an HTML document.}
       @item{stream -- a Common Lisp stream that has already been opened.}
       @item{array -- an @code{(unsigned-byte 8)} array.  The array is parsed
         directly, and interpreted according to the encoding it specifies.}
       @item{string/rod -- a rod (or string on unicode-capable
         implementations). Parses an XML document from the input string that
         has already undergone external-format decoding.}
       @end{itemize}
       
       If @var{handler} is @code{nil}, the parser's internal representation
       of the document is returned.  The result is equivalent to that
       returned using a PT builder as returned by @fun{make-pt-builder}, but
       avoids creating the same representation twice.
       
       Alternatively, @var{handler} can be a HAX handler
       (see @class{hax:abstract-handler}) or a SAX handler (see the
       @a[http://common-lisp.net/project/cxml/sax.html#sax]{SAX protocol in
       cxml}). In this case, the document will be serialized to the specified
       handler, and the result of @fun{hax:end-document} will be returned
       from this function. Note that the parser will currently always process
       the entire document before sending the first HAX event.")

(setf (documentation 'make-lhtml-builder 'function)
      "@return{The @class{lhtml-builder}, a HAX handler.}
       @short{Create a HAX handler which builds LHTML list structures.}

       Example:
       @begin{pre}
 (chtml:parse \"<p>nada</p>\" (chtml:make-lhtml-builder))
       @end{pre}
       @begin{code}
 => (:HTML NIL (:HEAD NIL) (:BODY NIL (:P NIL \"nada\")))
       @end{code}

       @see{parse}
       @see{serialize-lhtml}")

(setf (documentation 'lhtml-builder 'type)
      "@short{A HAX handler which builds LHTML list structures.}

       LHTML represents each HTML element as a list of the form

  @code{(}@em{name}@code{ (}@em{attributes...}@code{) }@em{children...}@code{)}

       and each attribute as a list of the form

  @code{(}@em{name value}@code{)}
       
       Element and attribute names are symbols in the keyword package
       with uppercase names.  Attribute values are rods or strings.

       @see{make-lhtml-builder}
       @see{serialize-lhtml}")

(setf (documentation 'serialize-lhtml 'function)
      "@arg[document]{an LHTML list}
       @arg[handler]{a HAX/SAX handler}
       @arg[name]{root element name, a rod/string}
       @arg[public-id]{nil or the Public ID, a rod/string}
       @arg[system-id]{nil or the System ID/URI, a rod/string}
       @return{The return value of this function is determined by the
         @var{handler} argument; see below.}
       @short{Serialize the LHTML document into HAX events, sent to the 
         specified HAX handler.}

       @var{handler} can be a HAX handler
       (see @class{hax:abstract-handler}) or a SAX handler (see the
       @a[http://common-lisp.net/project/cxml/sax.html#sax]{SAX protocol in
       cxml}).

       The result of calling @fun{hax:end-document} on the handler will be
       returned from this function.

       If @var{system-id} is specified, a doctype will be written
       according to the arguments @var{name}, @var{public-id}, and
       @var{system-id}.

       Use this function with a serialization sink to get a string or file
       with a serialized HTML document, or with a HAX/SAX builder to
       convert LHTML into a different representation, like DOM, PT, or
       STP.

       Example:
       @begin{pre}
 (let ((x '(:HTML NIL (:HEAD NIL) (:BODY NIL (:P NIL \"nada\"))))))
   (chtml:serialize-lhtml x (chtml:make-string-sink))
       @end{pre}
       @begin{code}
 => \"<HTML><HEAD></HEAD><BODY><P>nada</P></BODY></HTML>\"
       @end{code}

       @see{parse}
       @see{make-lhtml-builder}")

(setf (documentation 'make-pt-builder 'function)
      "@return{The @class{pt-builder}, a HAX handler.}
       @short{Create a HAX handler which builds @class{pt} structures.}

       Example:
       @begin{pre}
 (chtml:parse \"<p>nada</p>\" (chtml:make-pt-builder))
       @end{pre}
       @begin{code}
 => #<SGML:PT HTML ..>
       @end{code}

       @see{parse}
       @see{serialize-pt}")

(setf (documentation 'pt-builder 'type)
      "@short{A HAX handler which builds PT structures.}

       PT represents each HTML element as a structure instance of type
       @class{pt}.

       @see{make-pt-builder}
       @see{serialize-pt}")

(setf (documentation 'serialize-pt 'function)
      "@arg[document]{an @class{pt} instance}
       @arg[handler]{a HAX/SAX handler}
       @arg[name]{root element name, a rod/string}
       @arg[public-id]{nil or the Public ID, a rod/string}
       @arg[system-id]{nil or the System ID/URI, a rod/string}
       @return{The return value of this function is determined by the
         @var{handler} argument; see below.}
       @short{Serialize the PT node into HAX events, sent to the 
         specified HAX handler.}

       @var{handler} can be a HAX handler
       (see @class{hax:abstract-handler}) or a SAX handler (see the
       @a[http://common-lisp.net/project/cxml/sax.html#sax]{SAX protocol in
       cxml}).

       The result of calling @fun{hax:end-document} on the handler will be
       returned from this function.

       If @var{system-id} is specified, a doctype will be written
       according to the arguments @var{name}, @var{public-id}, and
       @var{system-id}.

       Use this function with a serialization sink to get a string or file
       with a serialized HTML document, or with a HAX/SAX builder to
       convert PT into a different representation, like DOM, LHTML, or
       STP.

       Example:
       @begin{pre}
 (let ((x (chtml:parse \"<p>nada</p>\" (chtml:make-pt-builder)))))
   (chtml:serialize-pt x (chtml:make-string-sink))
       @end{pre}
       @begin{code}
 => \"<HTML><HEAD></HEAD><BODY><P>nada</P></BODY></HTML>\"
       @end{code}

       @see{parse}
       @see{make-pt-builder}")

(setf (documentation 'pt 'type)
      "@short{Represents an HTML element.}

       PT represents each HTML element as a structure instance, named by
       a keyword symbol.  The children of a PT node are strings (rods)
       for text nodes, or other PT nodes for elements.

       @see{make-pt-builder}
       @see{serialize-pt}
       @see-slot{pt-name}
       @see-slot{pt-children}
       @see-slot{pt-parent}
       @see-slot{pt-attrs}")

(setf (documentation 'pt-name 'function)
      "@arg[instance]{a @class{pt} node}
       @return{a keyword symbol}
       @short{Returns the element's name.}

       HTML element names are symbols in the keyword package with uppercase
       names.")

(setf (documentation 'pt-children 'function)
      "@arg[instance]{a @class{pt} node}
       @return{a list}
       @short{Returns the element's children.}

       The children of a PT node are strings (rods)
       for text nodes, or other PT nodes for elements.

       @see{pt-parent}")

(setf (documentation 'pt-parent 'function)
      "@arg[instance]{a @class{pt} node}
       @return{nil, or a @class{pt} node}
       @short{Returns the element's parent node.}

       This slot should refer to the node's parent, if it is included
       in the list of that node's children.

       @see{pt-children}")

(setf (documentation 'pt-attrs 'function)
      "@arg[instance]{a @class{pt} node}
       @return{a plist}
       @short{Returns the element's attributes as a plist.}

       This plist maps attribute names to their values. 

       Attribute names are symbols in the keyword package with uppercase
       names.  Attribute values are strings/rods.")
