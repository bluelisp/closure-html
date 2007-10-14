(in-package :closure-html)

;;; FIXME: I liked the old SLURP-CATALOG code better than the LOOP below.
;;; (Except for the use of NETLIB and URI, which we don't have here.)

#||

(defun slurp-catalog (catalog-url)
  ;; Really dirty implementation
  (setf *simple-catalog* nil)
  (multiple-value-bind (io header) (netlib::open-document-2 catalog-url)
    (declare (ignore header))
    (unwind-protect
        (let ((str (glisp::gstream-as-string io)))
          (with-input-from-string (input str)
            (do ((x (read input nil nil) (read input nil nil)))
                ((null x))
              (assert (equal (symbol-name x) "PUBLIC"))
              (let ((name (read input))
                    (file (read input)))
                (assert (stringp name))
                (assert (stringp file))
                (push (cons name (url:merge-url (url:parse-url file) catalog-url))
                      *simple-catalog*)))))
      (g/close io))))

(format T "~&;; Parsing DTD~% ")
(sgml:slurp-catalog (url:parse-url "file://closure/resources/dtd/catalog"))
(setf cl-user::*html-dtd* (sgml:parse-dtd '(:public "-//W3C//DTD HTML 4.0 Frameset//EN")))
(format T "~&;; done~%")

||#

(defparameter sgml::*simple-catalog*
  (let ((base
	 (merge-pathnames
	  "resources/"
	  (asdf:component-relative-pathname
	   (asdf:find-system :closure-html)))))
    (loop
       :for (name . filename)
       :in '(("-//W3O//DTD W3 HTML 3.0//EN" . "dtd/HTML-3.0")
	     ("NETSCAPE-Bookmark-file-1" . "dtd/NETSCAPE-Bookmark-file-1")
	     ("-//W3C//ENTITIES Special//EN//HTML" . "dtd/Entities-Special")
	     ("-//W3C//ENTITIES Symbols//EN//HTML" . "dtd/Entities-Symbols")
	     ("-//W3C//ENTITIES Latin1//EN//HTML" . "dtd/Entities-Latin1")
	     ("-//W3C//DTD HTML 4.0 Frameset//EN" . "dtd/DTD-HTML-4.0-Frameset")
	     ("-//W3C//DTD HTML 4.0//EN" . "dtd/DTD-HTML-4.0")
	     ("-//W3C//DTD HTML 4.0 Transitional//EN" . "dtd/DTD-HTML-4.0-Transitional"))
       :collect (cons name (merge-pathnames filename base)))))

(defparameter *html-dtd*
  (sgml:parse-dtd '(:public "-//W3C//DTD HTML 4.0 Frameset//EN")))

(defun parse-xstream (input handler)
  (setf (sgml::a-stream-scratch input)
	(make-array #.(* 2 4096) :element-type 'runes:rune))
  (sgml::setup-code-vector input :utf-8)
  (let* ((dtd *html-dtd*)
	 (r (sgml:sgml-parse dtd input))
	 (pt (sgml::post-mortem-heuristic dtd r)))
    (if handler
	(serialize-pt pt handler)
	pt)))

(defun parse (input handler &key pathname)
  (etypecase input
    (xstream
     (parse-xstream input handler))
    (rod
     (let ((xstream (make-rod-xstream (string-rod input))))
;;;        (setf (xstream-name xstream)
;;; 	     (make-stream-name
;;; 	      :entity-name "main document"
;;; 	      :entity-kind :main
;;; 	      :uri nil))
       (parse-xstream xstream handler)))
    (array
     (parse (make-octet-input-stream input) handler))
    (pathname
     (with-open-file (s input :element-type '(unsigned-byte 8))
       (parse s handler :pathname input)))
    (stream
     (let ((xstream (make-xstream input :speed 8192)))
;;;        (setf (xstream-name xstream)
;;; 	     (make-stream-name
;;; 	      :entity-name "main document"
;;; 	      :entity-kind :main
;;; 	      :uri (pathname-to-uri
;;; 		    (merge-pathnames (or pathname (pathname input))))))
       (parse-xstream xstream handler)))))

(defun serialize-pt (document handler &key (name "HTML") public-id system-id)
  (hax:start-document handler name public-id system-id)
  (labels ((recurse (pt)
	     (cond
	       ((eq (gi pt) :pcdata)
		(hax:characters handler (pt-attrs pt)))
	       (t
		(let ((name (coerce (symbol-name (pt-name pt)) 'rod)))
		  (hax:start-element handler name (pt-attrs pt))
		  (mapc #'recurse (pt-children pt))
		  (hax:end-element handler name))))))
    (recurse document))
  (hax:end-document handler))
