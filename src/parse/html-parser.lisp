(in-package :closure-html)

(defparameter *this-file*
  (load-time-value
   (or #.*compile-file-pathname* *load-pathname*)))

(defparameter *this-directory*
  (make-pathname :directory (pathname-directory *this-file*)))

(defparameter sgml::*simple-catalog*
  (loop :for (name . filename)
     :in '(("-//W3O//DTD W3 HTML 3.0//EN" . "dtd/HTML-3.0")
           ("NETSCAPE-Bookmark-file-1" . "dtd/NETSCAPE-Bookmark-file-1")
           ("-//W3C//ENTITIES Special//EN//HTML" . "dtd/Entities-Special")
           ("-//W3C//ENTITIES Symbols//EN//HTML" . "dtd/Entities-Symbols")
           ("-//W3C//ENTITIES Latin1//EN//HTML" . "dtd/Entities-Latin1")
           ("-//W3C//DTD HTML 4.0 Frameset//EN" . "dtd/DTD-HTML-4.0-Frameset")
           ("-//W3C//DTD HTML 4.0//EN" . "dtd/DTD-HTML-4.0")
           ("-//W3C//DTD HTML 4.0 Transitional//EN" . "dtd/DTD-HTML-4.0-Transitional"))
     :collect (cons name (merge-pathnames filename *this-directory*))))

(defparameter *html-dtd* (sgml:parse-dtd '(:public "-//W3C//DTD HTML 4.0 Frameset//EN")))

(defun parse (inputstr)
  "given a string, produce a sgml:pt, which would be your toplevel parse tree node"
  (let ((dtd *html-dtd*))
    (let ((input (runes:make-xstream
                  (flexi-streams:make-in-memory-input-stream
                   (flexi-streams:string-to-octets
                    inputstr :external-format (flexi-streams:make-external-format :utf-8))))))
      (setf (sgml::a-stream-scratch input)
            (make-array #.(* 2 4096) :element-type 'runes:rune))
      (sgml::setup-code-vector input :utf-8)
      (let ((r (sgml:sgml-parse dtd input)))
        (sgml::post-mortem-heuristic dtd r)))))
