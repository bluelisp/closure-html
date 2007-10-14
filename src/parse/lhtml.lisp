(in-package :closure-html)


;;;; Parsing into LHTML

(defclass lhtml-builder (hax:abstract-handler)
  ((stack :initform nil :accessor stack)
   (root :initform nil :accessor root)))

(defun make-lhtml-builder ()
  (make-instance 'lhtml-builder))

(defmethod hax:start-document ((handler lhtml-builder) name pubid sysid)
  (declare (ignore name pubid sysid))
  nil)

(defun pt-attributes-to-lhtml (attrs)
  (mapcar (lambda (a)
	    (list (intern (string-upcase (hax:attribute-name a)) :keyword)
		  (hax:attribute-value a)))
	  attrs))

(defmethod hax:start-element ((handler lhtml-builder) name attrs)
  (let* ((parent (car (stack handler)))
	 (this (list (intern (string-upcase name) :keyword)
		     (pt-attributes-to-lhtml attrs))))
    (push this (stack handler))
    (if parent
	(push this (cddr parent))
	(setf (root handler) this))))

(defmethod hax:characters ((handler lhtml-builder) data)
  (push data (cddar (stack handler))))

(defmethod hax:comment ((handler lhtml-builder) data)
  ;; zzz haven't found out what the representation of comments is...
  data)

(defmethod hax:end-element ((handler lhtml-builder) name)
  (let ((current (pop (stack handler))))
    (setf (cddr current) (nreverse (cddr current)))))

(defmethod hax:end-document ((handler lhtml-builder))
  (root handler))



;;;; old stuff

#|
;;; brauchen wir hier alles das noch?

(defun unbreak-utf8 (arr &key (start 0))
  "given an utf-8 string, fix a common trouble with it:
   namely broken non-breaking-space sequences not being prefixed by 194"
  (when (> (length arr) start)
    (let* ((pos (position 160 arr :start start))
           (rest-fixed (when pos (unbreak-utf8 arr :start (1+ pos)))))
      (if pos
          (concatenate 'vector (subseq arr start pos) #(194 160) rest-fixed)
          (subseq arr start)))))

(defun cxml-pt-to-lhtml (pt)
  "given a sgml:pt, produce a lispified parse tree composed of lists of form:
   (tag property-list children)"
  (labels ((f (x)
             (cond
               ((null x) nil)
               ((stringp x) x)
               ((> (length x) 0)
                (let ((r (flexi-streams:octets-to-string x :external-format (flexi-streams:make-external-format :utf-8 :little-endian t))))
                  (unless r
                    (f (unbreak-utf8 x)))
                  r))
               (t (format t "impossible happened: ~S~%" x))))
           (iterate (pt)
             (let* ((attrs (if (listp (sgml:pt-attrs pt))
                               (loop :for (name val) :on (sgml:pt-attrs pt) :by #'cddr
                                  :collect (list name (f val)))
                               (f (sgml:pt-attrs pt)))))
               (if (eq (sgml:pt-name pt) :pcdata)
                   (f (sgml:pt-cdata pt))
                   (cons
                    (sgml:pt-name pt)
                    (cons
                     attrs
                     (loop :for n :in (sgml:pt-children pt)
                        :when n :do (if (arrayp n) (f n))
                        :nconc (if (arrayp n) 
                                   (list (f n))
                                   (list (iterate n))))))))))
    (iterate pt)))

(defun parse-html-to-lhtml (html)
  (cxml-pt-to-lhtml (parse html nil)))
|#

(defun walk-lhtml (lhtml tag-callback text-callback)
  (if (stringp lhtml)
      (funcall text-callback lhtml)
      (destructuring-bind (tag &rest body)
          (if (consp lhtml) lhtml (list lhtml))
        (destructuring-bind (tag-name &rest attributes)
            (if (consp tag) tag (list tag))
          (funcall tag-callback tag-name attributes body)))))


;;;; Old reader stuff:

(defun lhtml->pt (lhtml)
  (walk-lhtml lhtml
              ;; tag callback
              (lambda (tag-name attributes body)
                (sgml::make-pt :name tag-name
                         :attrs (loop :for (key value) :on attributes :by #'cddr
                                      :collect key
                                      :collect (etypecase value
                                                 (string (runes:string-rod value))
                                                 (sgml::rod value)))
                         :children (mapcar #'lhtml->pt body)))
              ;; text callback
              (lambda (string)
                (assert (stringp string))
                (sgml::make-pt :name :pcdata :attrs (runes:string-rod string)))))

(defun lhtml-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(lhtml->pt
    ,(funcall (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character #\# #\T 'lhtml-reader)
