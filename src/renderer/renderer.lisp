;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; Encoding: utf-8; Readtable: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: The Renderer's Heart
;;;   Created: long ago
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;;       $Id: renderer.lisp,v 1.7 2002-07-30 13:40:28 gilbert Exp $
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-2002 by Gilbert Baumann

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(in-package :R2)

;;;; TODO

;; . somehow when given
;;
;;   <div><div float:left width:100%>FFFF</div>RRRR</div> we wind up with:
;;
;;   RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
;;   FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
;;
;;   instead of
;;
;;   FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
;;   RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
;;

;;   "as high as possible"!

;; . somehow we missed case-insensitivity for css property names.

;; . See: http://www.w3.org/TR/css3-selectors/#first-line
;;   They changed rules!

;; . multiple STYLE nodes in HEAD?

;; . test clear on tables.

;; . setting borders on list-item results in funny effects.

;; . we still fail on percentage values on line-height (due to
;;   css-properties not defining the new infra-structure). Bad!

;; . we still fail on :lighter and :bolder.

;; . although we have the TeX mode, justify does not work.

;; . when creating the first line pseudo element we probably should
;;   not copy its HTML style attr style, should we?

;; . the first line of PRE element looks too small; looks like an
;;   inhertance bug wrt the first-line pseudo element.

;; . <tt>a</tt>, <tt>b</tt> is rendered as "a,b" instead of "a, b"
;;   correct? might be some HTML property.

;; . (oops) it looks like we have a new CSS bug: when the style says
;;   'margin: 20px; margin-left: auto' margin-left winds up as being
;;   But this only happens with style from the HTML style attribute.

;; . we should rethink spilling lines since currently we have drawing
;;   order problem as we draw the background and border after the
;;   content, in fact we misuse drawing the content to also keep track
;;   of the horizontal layout.

;; . support for letter spacing and word spacing is currently missing.

;; . letter spacing and word spacing should _not_ been done by KERN
;;   chunks but by appropriate white space chunks; that is support
;;   white space chunks which do not have the default width. And
;;   support NBSP chunks. Prefered would be to have new chunk types
;;   altogether and later extend them to actual \hfil chunks for the
;;   TeX-mode.

;; . we need to rework our »walking interface«. On current word are
;;   kind of chunks, but when reissueing these to need to completly
;;   reexamine style, which means that even stuff like 'display' might
;;   be different. Currently this reexamination breaks on replaced
;;   elements.

;; . tex mode breaks bitterly when we have replaced elements in the
;;   line -- why?

;; . on replaced elements: maybe also copy over width and height into
;;   the actual style.

;; . test height on block-level elements.

;; . vertical align on inline replaced elements does not seem to work.

;; . and to boot we still need to implement 'top' and 'bottom'
;;   vertical alignment.

;; . we need modify the replaced element protocol to also contain a
;;   predicate has-baseline-p since the glorious CSS1 spec seems to
;;   assume that images have no baseline and them move it to the outer
;;   box bottom (which is in IMHO very bad).

;; . floating »inline« replaced elements seems to work modulo correct
;;   border.

;; . display on replaced elements

;; . line height calculation for replaced elements is kind of wrong.

;; . IMG.width and IMG.height should probably map to css attributes.

;; . width/height of inline replaced elements must be taken into
;;   account.

;; . choose a representation for inline replaced elements

;; . implement the weird rules about text decorating replaced inline
;;   elements.

;; . padding top/bottom on oc chunks

;; . text-decoration is currently faked because we cannot easily
;;   access the font metrics when text decoration is due.

;; . disallow negative padding

;; . [maybe] use special element for the border-left/right half of
;;   inline elements?

;; . add-oc/add-cc should not peek at old style style.

;; . inline box decoration: same as with normal boxen, we need to draw
;;   it before the content! The interesting question is now how to
;;   accomplish this? we can either do another output record trick or
;;   we can peek ahead in the chunk vector to find the right x
;;   cooridnate for the decoration. Also: look into the specification
;;   if it specifies an exact drawing order.

;; . while we hack output records, we also should have a look at
;;   z-index. Is it global? If yes, we can probably implement it by
;;   twiddleing with the medium argument having it point to different
;;   »top-level« output records.
;;
;;   That is we think as if we had initially:
;;   (loop for i in ... do (setf (aref zplanes i) (with-new-output-record ...))
;;   and then
;;   (»with-output-to-record« (aref zplanes i) ...)

;; . background images on inline elements; also: does
;;   background-position apply to those?

;; . really somehow unify the decoration drawing between inline
;;   elements and block elements. And let the unified interface peek
;;   at computed style, so that when we implement the fancier options
;;   from CSS3 we get that automatically right for both.

;; . also: look at 'clip'.

;;;; NOTES

;; - First line elements (generated by either run-in boxen or marker
;;   boxen) are not yet correctly handled, currently they just push
;;   closures to rc-first-line-tasks, while we do not want closures
;;   there but fully rendered list of chunks. Which may not know their
;;   x-position (for after markers) but are taken into account for
;;   vertical align and are aligned vertically by the line spilling
;;   further down.
;;
;;   we might even get away with an output record? how knows.
;;

;; - We copy far too much. First the layout for lines should be changed into
;;   something like:
;;
;;      <x> <rune-1> <rune-2> <rune-3>
;;
;;   That is keep the runes together. Further we do not want runes there, but
;;   directly indexes into the relevant _font_. When that is given, we can
;;   directly give part of this line buffer to the xlib:draw-glyphs function. 
;;   This also implies that we need font changes in the line buffer and that
;;   we need to keep the line buffer around after rendering.
;;
;;   Implications:
;;
;;    a. The rune adder needs the rune -> glyph mapping (it does need it
;;       already for the width, so that this information comes for free).
;;
;;    b. We need special chunks for font changes.
;;
;;    c. We need special chunks for width indication.
;;
;;    d. It is still open how to efficiently cope with kerning.
;;
;;    e. When we count the copying, we see that we copy just once from the
;;       parse tree to the relevant output records. [And from the input stream
;;       into the parse tree -- we might even think about to spare these (at
;;       least part of it).
;;
;;   More aggressive would even be, if we generate the X protocol octets
;;   directly in the line buffer, but that would not longer be portable.
;;
;; - Further why not just keep the dangling open chunks at the start of a
;;   line? That would simplify scaning it quite a bit.
;;
;; - Also: We should generally first convert the paragraph to a vector of
;;   chunks and then operate on these. And remember the chunk vector for
;;   faster reflow and for not doing duplicate work in case we compute the
;;   minimum/maximum width within tables. Problem: first-line pseudo class. 
;;   For that we probably need to simultanous chunk list one for in the
;;   first-line style and the other in normal style. And: optimize the case
;;   that we do not have a first line pseudo-element at all.
;;
;; - can't we directly align lines?
;;

;;;; BUGS
 
;; - The borders are not yet correct 
;; - background images are still kind of fragile 
;; - from time to time we have broken images probably due to bogus mime type 
;; - the points at list items are missing 
;; - does clear work? 
;;  

;; - I am not sure about :capitalize 
;;                              --GB 2000-11-29

;; Q: What exactly is the percentage base for 'text-indent'?

;;;; XXX Early package definition

(defpackage :closure/clim-device
    (:use :clim :clim-lisp :clim-sys)
  (:import-from #:R2
                ;;
                #:device-dpi
                #:device-font-ascent
                #:device-font-descent
                #:device-font-underline-position
                #:device-font-underline-thickness
                #:device-font-has-glyph-p
                #:device-font-glyph-width
                #:device-realize-font-desc
                #:device-font-database

                ;; font-desc
                #:make-font-desc
                #:font-desc
                #:font-desc-family
                #:font-desc-weight
                #:font-desc-style
                #:font-desc-size
                #:font-desc-ddp
                #:font-desc-charset
                #:font-desc-widthen
                ;;
                #:make-font-database
                #:font-database-relate
                ;;
                #:scale-font-desc
                ;;
                ;;
                ;;
                #:bbox-p
                #:ibox-p
                #:ibox-x
                #:ibox-height
                #:ibox-y-oben
                #:abox-bx0 #:abox-by0 #:abox-bx1 #:abox-by1
                ))

;;; ---- Notes & TODO ---------------------------------------------------------

;;; Implementation status:

;; Noch nicht implementiert
;;   text-align ::= justify
;;
;; Intentionally not implemented
;; ¹ text-decoration ::= blink
;;   background-attachment
;;
;; Implementiert:
;;                clear ::= none | left | right | both
;;               height ::= <length> | auto
;;   list-style-image
;;   list-style-type
;;  list-style-position ::= inside | outside
;;                float ::= left | right | none
;;          white-space ::= normal | pre | nowrap 
;;         word-spacing ::= normal | <length>
;;       letter-spacing ::= normal | <length>
;;                width ::= <length> | <percentage> | auto
;;          text-indent ::= <length> | <percentage>
;;          line-height ::= normal | <number> | <length> | <percentage>
;;       vertical-align ::= baseline | sub | super | top | text-top | 
;;                          middle | bottom | text-bottom | <percentage>
;;      text-decoration ::= none | [ underline || overline || line-through ]
;;               margin ::= [ <length> | <percentage> | auto ]{1,4}
;;              padding ::= [ <length> | <percentage> ]{1,4}
;;                 font ::= [ <font-style> || <font-variant> || <font-weight> ]? 
;;                          <font-size> [/ <line-height> ]? <font-family>
;;              display ::= block | inline | list-item | none
;;           text-align ::= left | right | center
;;                color ::= <color>
;;               border ::= <border-width> || <border-style> || <color>
;;           background ::= ...

;;; Kind of boxen

;;  abox - proper box
;;    ibox - inline box
;;    bbox - block box
;;    rbox - replaced element box
;;  gbox - glyph box (only as child of ibox)
;;  fbox - floating box (warper around bbox and only immediate)
;;

;;; ---- Parameters & Constants -----------------------------------------------

;; Some handy unicode characters:
(defconstant u/bullet             #x2022)
(defconstant u/rightwards-arrow   #x2192)
(defconstant u/black-spade-suit   #x2660)
(defconstant u/black-club-suit    #x2663)
(defconstant u/black-heart-suit   #x2665)
(defconstant u/black-diamond-suit #x2666)
(defconstant u/black-square       #x25a0)
(defconstant u/white-square       #x25a0)
(defconstant u/white-circle       #x25cb)
(defconstant u/black-circle       #x25cf)
(defconstant u/white-bullet       #x25E6)

(defconstant u/newline            #x000A)
(defconstant u/space              #x0020)
(defconstant u/hyphen             #x002D)
(defconstant u/soft-hyphen        #x00AD)
(defconstant u/nbsp               #x00A0)

;; List item bullets 
;; Each such list is a list of preferences, there should be a
;; character, which is likely to exist in all fonts.
#||
(defconstant +list-style-type-glyphs/disc+
    (list u/black-circle u/bullet u/white-bullet u/white-circle
          (char-code #\o)))

(defconstant +list-style-type-glyphs/circle+
    (list u/white-circle u/white-bullet u/bullet u/black-circle
          (char-code #\*)))

(defconstant +list-style-type-glyphs/square+
    (list u/black-square u/white-square u/white-bullet u/bullet
          (char-code #\-)))
||#

(defconstant +list-style-type-glyphs/disc+
    (list ;;u/black-circle u/bullet u/white-bullet u/white-circle
          (char-code #\o)))

(defconstant +list-style-type-glyphs/circle+
    (list ;;u/white-circle u/white-bullet u/bullet u/black-circle
          (char-code #\*)))

(defconstant +list-style-type-glyphs/square+
    (list ;;u/black-square u/white-square u/white-bullet u/bullet
          (char-code #\-)))


;;;;

(defvar *default-style-sheet*)

(defparameter *tex-mode-p* nil
  "Whether we use the TeX-like algorithm.")

;;;;

#+CMU
(eval-when (compile)
  (setq ext:*efficiency-note-cost-threshold* (1- most-positive-fixnum)))

(defun iterate-over-runes/normal/generic (fun runes start end text-style)
  ;; Wird fast nicht mehr gebraucht.
  (declare (type rod runes)
           (type text-style text-style))
  (let ((letter-spacing (text-style-letter-spacing text-style))
        (word-spacing (text-style-word-spacing text-style))
        (font (text-style-font text-style)))
    (if (eql letter-spacing :normal) (setf letter-spacing 0))
    (if (eql word-spacing :normal) (setf word-spacing 0))
    (let ((x 0))
      (loop for i from start to (1- end) do
            (let* ((rune (aref runes i))
                   (next-rune (if (< (+ i 1) end)
                                  (aref runes (1+ i))
                                #xFFFF)))
              (if (white-space-rune-p rune) (setf rune 32))
              (unless (and (= rune 32) (white-space-rune-p next-rune))
                (let ((cw (+ (if (white-space-rune-p rune)
                                 (+ (rune-width font rune) word-spacing)
                               (rune-width font rune))
                             letter-spacing)))
                  (funcall fun rune i x cw)
                  (incf x cw)))))
      x)))

(defun iterate-over-runes/pre/generic (fun runes start end text-style)
  (declare (type rod runes)
           (type text-style text-style))
  (let ((letter-spacing (text-style-letter-spacing text-style))
        (word-spacing (text-style-word-spacing text-style))
        (font (text-style-font text-style)))
    (if (eql letter-spacing :normal) (setf letter-spacing 0))
    (if (eql word-spacing :normal) (setf word-spacing 0))
    (let ((x 0))
      (loop for i from start to (1- end) do
            (let* ((rune (aref runes i)))
              (if (white-space-rune-p rune) (setf rune 32))
              (progn
                (let ((cw (+ (if (white-space-rune-p rune)
                                 (+ (rune-width font rune) word-spacing)
                               (rune-width font rune))
                             letter-spacing)))
                  (funcall fun rune i x cw)
                  (incf x cw)))))
      x)))

(eval-when (compile eval load)
  (defparameter *fetch-rune-width-code*
      '(progn
        (setf $cw (svref (the (simple-array t (256))
                           (svref (the (simple-array t (256)) $fwt)
                                  (the (unsigned-byte 8) 
                                    (ldb (byte 8 8) (the fixnum $rune)))))
                   (the (unsigned-byte 8) 
                     (ldb (byte 8 0) (the fixnum $rune)))))
        (when (= $cw -1)
          (css-font-desc-ensure-glyph-info $font $rune)
          (setf $cw (svref (the (simple-array t (256))
                             (svref (the (simple-array t (256)) $fwt)
                                    (the (unsigned-byte 8)
                                      (ldb (byte 8 8) (the fixnum $rune)))))
                     (the (unsigned-byte 8) (ldb (byte 8 0) (the fixnum $rune)))))))))

(defmacro iterate-over-runes/normal* (fun runes start end text-style)
  `(locally
       (declare (type rod ,runes)
                (type text-style ,text-style)
                (type fixnum ,start ,end)
                #.cl-user:+optimize-very-fast+)
     (let* (($font (text-style-font ,text-style))
            ($fwt  (css-font-desc-width-table $font)))
       (declare (type (simple-array (simple-array t (256)) (256)) $fwt)
                (type css-font-desc $font))
       (let (($x 0)
             ($prev-rune #xFFFF)
             ($rune #xFFFF))
         (declare (type rune $prev-rune $rune))
         (declare (type fixnum $x))
         (let (($i ,start))
           (declare (type fixnum $i))
           (loop
             (when (= (the fixnum $i) (the fixnum ,end)) (return))
             (setq $prev-rune $rune
                   $rune (aref (the rod ,runes) $i))
             (if (white-space-rune-p* $rune)
                 (setf $rune 32))
             (unless (and (= $rune 32) (= $prev-rune 32))
               (let* (($cw 0))
                 (declare (type fixnum $cw))
                 ,*fetch-rune-width-code*
                 (progn
                   (,fun $rune (the fixnum $i) $x $cw)
                   (setf $x (the fixnum (+ $x (the fixnum $cw))) ))))
             (setq $i (the fixnum (+ (the fixnum $i) 1))) ))
         $x))))

(defmacro iterate-over-runes/pre* (fun runes start end text-style)
  `(locally
       (declare (type rod ,runes)
                (type text-style ,text-style)
                (type fixnum ,start ,end)
                #.cl-user:+optimize-very-fast+)
     (let* (($font (text-style-font ,text-style))
            ($fwt  (css-font-desc-width-table $font)))
       (declare (type (simple-array (simple-array t (256)) (256)) $fwt)
                (type css-font-desc $font))
       (let ((x 0)
             ($rune 0))
         (declare (type rune $rune))
         (declare (type fixnum x))
         (loop for i #-GCL of-type #-GCL fixnum from ,start to (the fixnum (1- ,end)) do
               (locally
                   (declare (fixnum i))
                 (setq $rune (aref (the rod ,runes) i))
                 (if (white-space-rune-p*/no-nl $rune)
                     (setf $rune 32))
                 (let (($cw 0))
                   (declare (type fixnum $cw))
                   ,*fetch-rune-width-code*
                   (,fun $rune i x $cw)
                   (incf x (the fixnum $cw)) ) ))
         x))))

(defmacro iterate-over-runes/pre** (fun runes start end text-style)
  `(locally
       (declare (type rod ,runes)
                (type text-style ,text-style)
                (type fixnum ,start ,end)
                #.cl-user:+optimize-very-fast+)
     (let* (($font (text-style-font ,text-style))
            ($fwt  (css-font-desc-width-table $font)))
       (declare (type (simple-array (simple-array t (256)) (256)) $fwt)
                (type css-font-desc $font))
       (let ((x 0)
             ($rune 0))
         (declare (type rune $rune))
         (declare (type fixnum x))
         (loop for i #-GCL of-type #-GCL fixnum from ,start to (the fixnum (1- ,end)) do
               (locally
                   (declare (fixnum i))
                 (setq $rune (aref (the rod ,runes) i))
                 (if (white-space-rune-p* $rune)
                     (setf $rune 32))
                 (let (($cw 0))
                   (declare (type fixnum $cw))
                   ,*fetch-rune-width-code*
                   (,fun $rune i x $cw)
                   (incf x (the fixnum $cw)) ) ))
         x))))
;;;; ==========================================================================

(defmacro iterate-over-runes (fun runes start end text-style white-space)
  (assert (eq (car fun) 'lambda))
  `((lambda (runes start end text-style white-space)
      (cond ((and ;; nil
                  (eq (text-style-letter-spacing text-style) 0)
                  (eq (text-style-word-spacing text-style) :normal))
             (ecase white-space
               ((:normal :nowrap)
                (iterate-over-runes/normal* ,fun runes start end text-style))
               ((:pre)
                (iterate-over-runes/pre* ,fun runes start end text-style)) ))
            (t
             (ecase white-space
               ((:normal :nowrap)
                (iterate-over-runes/normal/generic ,fun runes start end text-style))
               ((:pre)
                (iterate-over-runes/pre/generic ,fun runes start end text-style)) )) ))
    ,runes ,start ,end ,text-style ,white-space))

;;; --------------------------------------------------------------------------

;;; ---- Renderer Driver ------------------------------------------------------

(defun add-content (bbox child)
  (assert (bbox-p bbox))
  (unless (member child (bbox-contents bbox))
    (setf (bbox-contents bbox) (nconc (bbox-contents bbox) (list child))))
  (values))

(defun absolute-positioned-p (pt)
  (member (css:style-attr pt 'css:@position) '(:absolute :fixed)))

(defun next-sibling (pt)
  "Return the next sibling of 'pt'; return NIL if no such sibling exists."
  (let ((parent (sgml:pt-parent pt)))
    (and parent
         (cadr (member pt (sgml:pt-children parent))))))

(defun next-pt (pt)
  "Find the next pt node following pt. Climbs up the the tree if neccassary."
  (or (next-sibling pt)
      ;; no next sibling: climb up find next sibling of parent
      (and (sgml:pt-parent pt)
           (next-pt (sgml:pt-parent pt)))))

(defun block-box-follows-p (pt)
  "Does a block box (that does not float and is not absolutely positioned) follow?
   Should do what ever the correct interpretation of this CSS-2 phrase will be."
  ;; find the following box, additional magic thrown in to ignore
  ;; empty CDATA nodes.
  (do ((next (next-pt pt) (next-pt next)))
      ((or (null next)
           (not (or
                 ;; ignore empty CDATA nodes
                 (and (eq (sgml:gi next) :pcdata)
                      (every #'white-space-rune-p* (sgml:pt-cdata next)))
                 ;; also ignore display = none nodes
                 (eq (css:display next) :none))))
       (and next                                        ;a box follows at all
            (eq (css:style-attr pt 'css:@float) :none)  ;does not float
            (not (absolute-positioned-p next))            ;not absolutely positioned
            ;; is a block box
            (case (css:display next)
              ((:block) T)
              ((:run-in)
               (eq (run-in-role next) :block))
              (otherwise
               nil))))))

(defun run-in-role (pt)
  "Decide, which role this run-in box should play; returns either :inline or :block."
  (if (block-box-follows-p pt)
      :inline
    :block))

(defun brender (rc pt parent-box &optional res)
  (ecase (css:display pt)
    ((:none)
     nil)
    ((:block :run-in)
     (cond #||
           ((absolute-positioned-p pt) 
            (render-abs-box rc pt parent-box))
           ||#
           ((eq (sgml:gi pt) :table)
            (render-table rc pt parent-box));XXX same 'res' has to apply here.
           (t
            (render-normal-block rc pt parent-box res))))
    ((:list-item)
     (assert (null res))
     (render-list-item rc pt parent-box))
    ((:inline :pcdata)
     (error "In ~S: This situation shouldn't occur." 'brender)) ))

;;; Absoultely Positioned Boxen

;; Absolutely positioned boxen (boxen with 'position' set to
;; 'absolute' or 'fixed') should not generate content in the normal
;; flow. Thus viewed from the block boxen or paragraph renderer these
;; boxen act like 'display' were set to 'none'. 

;; When we encounter an absoultely positioned box, while rendering, we
;; insert an empty BBOX into the content of the parent box to preserve
;; drawing order. We generate an ABS-TASK and push it onto
;; RC-ABS-TASKS. Later when the parent block box (in CSS-2
;; terminology: containing block) is fully rendered otherwise, we
;; proceed with rendering these ABS-TAKSs and thus filling the empty
;; BBOXen generated before with content. This is also needed, since
;; the height of the containing block wouldn't be known otherwise.

;; Note: This is the first time we encounter a situtation, where
;; rendering order is different from drawing order.

(defstruct abs-task
  x0
  x1
  y
  box
  parent-box)

(defun render-abs-box (rc pt parent-box)
  ;; for now we simply insert a totally faked bbox.
  (let ((new (make-bbox :pt pt)))
    (add-content parent-box new)
    (push (make-abs-task :x0 (rc-x0 rc)  ;this is wrong, when we deal with inline boxen,
                         :x1 (rc-x1 rc)  ; which mutated to absoultely positioned boxen!
                         :y (rc-y rc)
                         :box new
                         :parent-box parent-box)
          (rc-abspos rc))))

(defun render-abs-boxen (rc)
  ;; care for the delayed absoultely positioned boxen in 'rc'
  (dolist (k (rc-abspos rc))
    (render-abs-task rc k)))

;; containing block ist einfach die papa block level box ...

(defun render-abs-task (rc task)
  (let* ((x0 (abs-task-x0 task))
         (x1 (abs-task-x1 task))
         (y0 (abs-task-y task))
         (box (abs-task-box task))
         (pt  (abox-pt box))
         (pwd (- x1 x0))
         (pheight (bbox-iheight (abs-task-parent-box task)))
         (py0     (bbox-iy (abs-task-parent-box task)))
         (container (abs-task-parent-box task))
         (position (css:style-attr pt 'css:@position))
         cix ciy )
    py0
    (cond ((eq position :fixed)
           (setf pwd     (device-canvas-width (rc-device rc))
                 pheight (device-canvas-height (rc-device rc))
                 cix     0
                 ciy     0))
          (t
           (setf cix (bbox-ix container)
                 ciy (bbox-iy container))))
    ;; (+ (bbox-iy container) x0) = task-x0 
    (setf x0 (- (abs-task-x0 task) cix)
          y0 (- (abs-task-y  task) ciy))
    ;; fetch and calcuate horizontal position
    (let ((left          (maybe-resolve-percentage (css:style-attr pt 'css:@left) pwd))
          (margin-left   (maybe-resolve-percentage (css:style-attr pt 'css:@margin-left) pwd))
          (padding-left  (maybe-resolve-percentage (css:style-attr pt 'css:@padding-left) pwd))
          (width         (maybe-resolve-percentage (css:style-attr pt 'css:@width) pwd))
          (padding-right (maybe-resolve-percentage (css:style-attr pt 'css:@padding-left) pwd))
          (margin-right  (maybe-resolve-percentage (css:style-attr pt 'css:@margin-right) pwd))
          (right         (maybe-resolve-percentage (css:style-attr pt 'css:@right) pwd))
          (border        (pt-border pt))
          (top           (maybe-resolve-percentage (css:style-attr pt 'css:@top) pheight))
          (bottom        (maybe-resolve-percentage (css:style-attr pt 'css:@bottom) pheight))
          (height        (css:style-attr pt 'css:@height))
          (padding-top   (css:style-attr pt 'css:@padding-top))
          (padding-bottom (css:style-attr pt 'css:@padding-bottom))
          (margin-top     (css:style-attr pt 'css:@margin-top))
          (margin-bottom  (css:style-attr pt 'css:@margin-bottom)))
      (let ((border-left-width  (if border (border-left-width border) 0))
            (border-right-width (if border (border-right-width border) 0))
            (border-top-width  (if border (border-top-width border) 0))
            (border-bottom-width (if border (border-bottom-width border) 0)))
        (multiple-value-bind (left margin-left width margin-right right)
            (calc-width-for-abs-pos 
             x0 pwd
             left margin-left border-left-width padding-left
             width 
             padding-right border-right-width margin-right right)
          (multiple-value-bind (top margin-top height margin-bottom bottom)
              (calc-height-for-abs-pos 
               y0 pheight
               top
               margin-top border-top-width padding-top
               height
               padding-bottom border-bottom-width margin-bottom
               bottom)
            bottom
            margin-bottom 
            right
            height
          ;;
          (setf (abox-margin-top box)     margin-top
                (abox-margin-right box)   margin-right
                (abox-margin-bottom box)  margin-bottom
                (abox-margin-left box)    margin-left
                (abox-padding box)        (make-padding padding-top padding-right  padding-bottom padding-left)
                ;;(abox-padding-top box)    padding-top
                ;;(abox-padding-right box)  padding-right
                ;;(abox-padding-bottom box) padding-bottom
                ;;(abox-padding-left box)   padding-left
                (abox-white-space box)    (css:style-attr pt 'css:@white-space)
                (abox-border box)         border
                (abox-background box)     (pt-backgroud pt)
                (abox-color box)          (css:style-attr pt 'css:@color)
                (bbox-width box)          width
                (bbox-text-align box)     (css:style-attr pt 'css:@text-align)
                )
          (let ((new-rc (copy-rc rc)))
            (setf (rc-y new-rc)  (+ ciy top)
                  (rc-x0 new-rc) (+ cix left)
                  (rc-x1 new-rc) 17    ;eigentlich scheißegal -- wird eh' überschrieben
                  (rc-vertical-margins new-rc) nil
                  (rc-vertical-margin-callbacks new-rc) nil
                  (rc-first-line-tasks new-rc) nil
                  (rc-left-floating-boxen new-rc) nil
                  (rc-right-floating-boxen new-rc) nil
                  (rc-anchors new-rc) nil
                  (rc-abspos new-rc) nil)
            (let ((*rcontext* new-rc))
              (render-normal-block new-rc pt container box) ;hmmm 'parent-box' wird noch für text-indent gebraucht.
              ;; hoffe das reicht -- nur noch alles rekursive
              (render-abs-boxen new-rc)) )))))))
                
;;; ---- Block Boxen ----------------------------------------------------------

(defmacro with-new-margins ((rc x0 x1) &body body)
  `(let ((.old-x0. (rc-x0 ,rc))
         (.old-x1. (rc-x1 ,rc)))
     (psetf (rc-x0 ,rc) ,x0
            (rc-x1 ,rc) ,x1)
     (prog1
         (locally
           ,@body)
       (psetf (rc-x0 ,rc) .old-x0.
              (rc-x1 ,rc) .old-x1.))))

(defparameter *height-ist-aussen-p* t)

(defvar *run-ins* nil)

(defun add-vertical-margin (rc amount &optional (callback nil))
  (when (eq amount :auto) (setq amount 0))
  (setf (rc-vertical-margins rc) (cons amount (rc-vertical-margins rc)))
  (if callback (push callback (rc-vertical-margin-callbacks rc))))


;; Note the specification is highly obfuscated here:

;; | In the case of negative margins, the absolute maximum of the negative 
;; | adjoining margins is deducted from the maximum of the positive 
;; | adjoining margins. If there are no positive margins, the absolute 
;; | maximum of the negative adjoining margins is deducted from zero. 

;; To deduct the absolute maximum of negative values == add the
;; minimum. So we get:
;;
;; effective-margin <- min (negative margins) + max (positive margins).
;;
;; Warum nicht gleich so?!

(defun flush-vertical-margin (rc)
  (when (rc-vertical-margins rc) 
    (let ((neg 0)
          (pos 0))
      (dolist (k (rc-vertical-margins rc))
        (setf neg (min neg k))
        (setf pos (max pos k)))
      (incf (rc-y rc) (+ pos neg)) ))
  (dolist (x (rc-vertical-margin-callbacks rc))
    (funcall x (rc-y rc)))
  (setf (rc-vertical-margins rc) nil
        (rc-vertical-margin-callbacks rc) nil))

;;; ---- Inline and #PCDATA Boxen ---------------------------------------------

(defun maybe-pt-imap (rc pt)
  "When the parse tree at hand should have an imap associated with it, return it;
   returns NIL otherwise."
  (cond ((and (eq (sgml:gi pt) :A)
              (pt-attr* pt :href nil))
         (make-instance 'imap-everywhere
           :link (make-hyper-link 
                  :url (pt-effective-url-attr (rc-document rc) pt :href)
                  :alt (pt-attr* pt :title nil)
                  :target (pt-effective-target-attr pt :target))))
        (t
         nil)))

(defun nuke-white-space (pt first? last?)
  ;; Only used in rminmax.lisp
  "Handles white space nukeing for PCDATA elements; 'first-p' and 'last-p'
   should indicate, wheter this element is the first/last child of its 
   parent; Returns two values: 
   'start' and 'end', the subseq of content, which should be rendered."
  ;; This is also used by the min/max finder.
  (let* ((str (sgml:pt-cdata pt))
         (start 0)
         (end (length str))
         ;;(i (position pt (sgml:pt-children (sgml:pt-parent pt))))
         (ws (css:style-attr (sgml:pt-parent pt) 'css:@white-space)))
    (declare (type fixnum start end)
             (type rod str))

    (unless (eq ws :pre)
      ;; Nuke leading white space
      (when (or first? ;;(eql i 0)               ;directly after start tag
                #||
                ;; directly after a block level elements end tag
                (and (elt (sgml:pt-children (sgml:pt-parent pt)) (- i 1))
                     (css:block-element-p
                      (elt (sgml:pt-children (sgml:pt-parent pt)) (- i 1))))
                ||#
                )
        (loop
          (when (= start end) (return))
          (unless (white-space-rune-p* (aref str start)) ;xxx NL and :pre
            (return))
          (setf start (the fixnum (+ start 1)))))
    
      ;; Nuking trailing white space is orthogonal to that
      (when (or last?
                ;;(eql i (1- (length (sgml:pt-children (sgml:pt-parent pt)))))
                                        ;directly before end tag
                ;; directly before a block level elements start tag (Woher?)
                #||
                #+(OR)
                (and (elt (sgml:pt-children (sgml:pt-parent pt))
                           (+ i 1))
                     (css:block-element-p
                      (elt (sgml:pt-children (sgml:pt-parent pt))
                           (+ i 1))) )
                ||#)
        (loop
          (when (= start end) (return))
          (unless (white-space-rune-p* (aref str (- end 1)))
            (return))
          (setf end (the fixnum (- end 1))) ) ))
    (values start end)))

;;; ---- Vertical Geometry of Lines -------------------------------------------

(defvar *tops* nil)
(defvar *btms* nil)

(defun ibox-text-height+depth (box)
  (let* ((ts (text-style-font (ibox-text-style box)))
         (as (font-desc-ascent ts))
         (ds (font-desc-descent ts))
         (lh (ibox-line-height box))
         (hl-1 (floor   (- lh (+ as ds)) 2)))                   ;DEVRND
    (let* ((ds2 (max 0 (+ ds hl-1)))
           (as2 (- lh ds2)))
      (assert (= lh (+ as2 ds2)))
      ;; wir müssen hier leider negative werte vermeiden, deswegen
      ;; dieser hack mit 'max' oben.
      (values as2 ds2) )))

;;; ---- Floating Boxen -----------------------------------------------------------------------

(defun render-floating-box (rc pt)
  (let (w b)
    ;; wir stellen uns mal ganz dumm...
    (cond ((eq (sgml:gi pt) :table)
           (setf w (table-pt-aw rc pt)))   ;###
          (t
           (setf b (make-skeleton-bbox-for-pt rc pt (- (rc-x1 rc) (rc-x0 rc))))
           (setf w (+ (abox-left-leading b)
                      (bbox-width b)
                      (abox-right-leading b)))))
    (make-fbox-desc :pt pt 
                    :bbox b
                    :side (css:style-attr pt 'css:@float)
                    :w (max 0 w))))

(defun find-floating-box-position (kind lfboxen rfboxen parent-x0 parent-x1 yo w) 
  ;; -> x; y
  (let ((ys (sort (remove-duplicates (mapcan (lambda (b)
                                               (list (fbox-desc-y b)
                                                     (+ (fbox-desc-y b) (fbox-desc-h b))))
                                             (append lfboxen rfboxen))
                                     :test #'=)
                  #'<)))
    ;; This is for rule 5 part 1 in section 4.1.4:
    ;;
    ;; |  A floating element's top may not be higher than the top of any
    ;; |  earlier floating [..] element.
    ;;
    (dolist (b (append lfboxen rfboxen))
      (setf yo (max yo (fbox-desc-y b))))

    (labels ((find-x0 (y)
               (let ((x parent-x0))
                 (dolist (k lfboxen)
                   (when (and (<= (fbox-desc-y k) y)
                              (< y (+ (fbox-desc-y k) (fbox-desc-h k))))
                     (setf x (max x (+ (fbox-desc-x k) (fbox-desc-w k))))))
                 x))
             (find-x1 (y)
               (let ((x parent-x1))
                 (dolist (k rfboxen)
                   (when (and (<= (fbox-desc-y k) y) (< y (+ (fbox-desc-y k) (fbox-desc-h k))))
                     (setf x (min x (fbox-desc-x k)))))
                 x))
             (try (y)
               (let ((x1 (find-x1 y))
                     (x0 (find-x0 y)))
                 (cond ((>= (- x1 x0) w)
                        (ecase kind
                          (:left
                           (return-from find-floating-box-position
                             (values x0 y)))
                          (:right
                           (return-from find-floating-box-position
                             (values (- x1 w) y)))) ))) ))
      ;; Note: This has some resemblance to the sweep line algorithm.
      (try yo)
      (dolist (y ys)
        (and (> y yo) (try y)))

      ;; If we came here, the parent box is probably too narrow for
      ;; the floating box, move past the lowest floating box.
      ;; Q: is this right?!
      (when ys
        (setf yo (max yo (car (last ys)))))
      (ecase kind
        (:left
         (values parent-x0 yo))
        (:right
         (values (- parent-x1 w) yo))) )))

(defun add-floating-box (rc yoben fb)
  "Add the floating box given by the fbox-desc 'fb' to render context 'rc'.
   'yoben' is top of the current line."
  ;; xxx this all is still not 100% correct, due to the famous 4.1.4
  ;; item 6 design flaw and due to drawing order in general.
  (let ((new-rc (copy-rc rc)))
    (let ((y000 (rc-y rc))
          (*rcontext* new-rc))

      (setf (rc-y new-rc) yoben
            (rc-vertical-margin-callbacks new-rc) nil
            (rc-first-line-tasks new-rc) nil )
    
      (flush-vertical-margin new-rc)
      (handle-clear new-rc
                    (fbox-desc-pt fb) (css:style-attr (fbox-desc-pt fb) 'css:@clear))

      (print (list 'add-floating-box
                   '(fbox-desc-w fb) '= (fbox-desc-w fb)
                   (rc-x0 new-rc)
                   (rc-x1 new-rc)
                   (- (rc-x0 new-rc) (rc-x1 new-rc) )))
      
      (multiple-value-bind (px py)
          (find-floating-box-position (fbox-desc-side fb)
                                      (rc-left-floating-boxen new-rc) 
                                      (rc-right-floating-boxen new-rc)
                                      (rc-x0 new-rc)
                                      (rc-x1 new-rc)
                                      (rc-y new-rc)
                                      (fbox-desc-w fb))
        (setf (rc-x0 new-rc) px
              (rc-x1 new-rc) (+ px (fbox-desc-w fb))
              (rc-y new-rc) py))        ;??!

      ;; what if margin-left is negative?
      ;; and margin-top?!
      (let* ((fake-parent (make-bbox)))
        (let ((bbox (brender new-rc (fbox-desc-pt fb) fake-parent (fbox-desc-bbox fb))))
          (flush-vertical-margin new-rc)
          (setf (fbox-desc-h fb) (- (rc-y new-rc) y000)
                (fbox-desc-y fb) y000
                (fbox-desc-x fb) (rc-x0 new-rc)
                (fbox-desc-bbox fb) bbox) )))
    
    (dolist (k (rc-left-floating-boxen new-rc))
      (pushnew k (rc-left-floating-boxen rc)))
    (dolist (k (rc-right-floating-boxen new-rc))
      (pushnew k (rc-right-floating-boxen rc))))
  
  (ecase (fbox-desc-side fb)
    (:left
     (push fb (rc-left-floating-boxen rc)))
    (:right
     (push fb (rc-right-floating-boxen rc))) ))



;;; ---- List Items ---------------------------------------------------------------------------

(defun render-list-item (rc pt parent-box)
  (let ((res-box (make-skeleton-bbox-for-pt rc pt (- (rc-x1 rc) (rc-x0 rc)))))
    (render-normal-block rc pt parent-box res-box)))

(defun list-item-index (pt)
  (let ((parent (sgml:pt-parent pt)))
    (let ((index 0))
      (let ((start (pt-attr/integer parent :start nil)))
        (when start
          (if (>= start 1)
              (setf index (1- (pt-attr/integer parent :start)))
            (warn "Value, ~S, of 'START' is insane." start))))
      (dolist (k (sgml:pt-children parent) (assert nil))
        (let ((value (pt-attr/integer k :value nil)))
          (cond (value
                 (if (>= value 1)
                     (setf index (1- value))
                   (warn "Value, ~S, of 'VALUE' is insane." value))) ))
        (when (eq k pt)
          (return index))
        (incf index)))))

;;;

(defclass replacement-object ()
  ())

(defun process-head-child (doc pt)
  (cond ((eq (sgml:gi pt) :title)
         (setf (document-title doc) (pt-all-data pt)))
        ((eq :link (sgml:gi pt))
         (setf (document-links doc)
           (append (document-links doc)
                   (list (make-instance 'link
                           :title  (pt-attr/latin1 pt :title)
                           :rel    (pt-attr/link-types pt :rel)
                           :rev    (pt-attr/link-types pt :rev)
                           :media  (pt-attr/latin1 pt :media) ;; xxx (comma-separated-list k :media)
                           :target (pt-attr/latin1 pt :target)
                           :href   (pt-effective-url-attr doc pt :href))))))
        (t
         )))

(defun process-head (rc pt)
  (let ((doc (rc-document rc)))
    ;;
    (dolist (k (sgml:pt-children pt))
      (process-head-child doc k))))

;;;

;; * RFC-2068:
;; |
;; | 19.6.2.4 Link
;; | 
;; |    The Link entity-header field provides a means for describing a
;; |    relationship between two resources, generally between the requested
;; |    resource and some other resource. An entity MAY include multiple Link
;; |    values. Links at the metainformation level typically indicate
;; |    relationships like hierarchical structure and navigation paths. The
;; |    Link field is semantically equivalent to the <LINK> element in
;; |    HTML.[5]
;; | 
;; |           Link           = "Link" ":" #("<" URI ">" *( ";" link-param )
;; | 
;; |           link-param     = ( ( "rel" "=" relationship )
;; |                              | ( "rev" "=" relationship )
;; |                              | ( "title" "=" quoted-string )
;; |                              | ( "anchor" "=" <"> URI <"> )
;; |                              | ( link-extension ) )
;; | 
;; |           link-extension = token [ "=" ( token | quoted-string ) ]
;; | 
;; |           relationship   = sgml-name
;; |                          | ( <"> sgml-name *( SP sgml-name) <"> )
;; | 
;; |           sgml-name      = ALPHA *( ALPHA | DIGIT | "." | "-" )
;; | 
;; |    Relationship values are case-insensitive and MAY be extended within
;; |    the constraints of the sgml-name syntax. The title parameter MAY be
;; |    used to label the destination of a link such that it can be used as
;; |    identification within a human-readable menu. The anchor parameter MAY
;; |    be used to indicate a source anchor other than the entire current
;; |    resource, such as a fragment of this resource or a third resource.
;; | 
;; |    Examples of usage include:
;; | 
;; |        Link: <http://www.cern.ch/TheBook/chapter2>; rel="Previous"
;; | 
;; |        Link: <mailto:timbl@w3.org>; rev="Made"; title="Tim Berners-Lee"
;; | 
;; |    The first example indicates that chapter2 is previous to this
;; |    resource in a logical navigation path. The second indicates that the
;; |    person responsible for making the resource available is identified by
;; |    the given e-mail address.
;; | 


(defun render-pt (device document pt w 
                  &optional (flag t) (h 0)
                  &key (selected-style :default))
  flag
  (let* ((rc (make-rc :device device :y 0 :x0 0 :x1 w
                      :vertical-margins nil
                      :vertical-margin-callbacks nil
                      :first-line-tasks nil
                      :left-floating-boxen nil
                      :right-floating-boxen nil
                      :document document ))
         (*rcontext* rc))

    ;;
    (setf (document-title document) nil
          (document-links document) nil)
    ;;
    (dolist (k (document-http-header document))
      (cond ((string-equal :link (car k))
             (let ((links (netlib:parse-http-link-field (cdr k))))
               (cond ((null links)
                      (warn "HTTP ?ink?header field has illegal syntax: ~S." (cdr k)))
                     (t
                      (dolist (link links)
                        (process-head-child document (sgml:lhtml->pt link)))))))))
    ;;
    ;; First of all process the head
    (dolist (k (sgml:pt-children pt))
      (when (eq (sgml:gi k) :head)
        (process-head rc k)))
    ;;
    (unless (document-title document)
      (setf (document-title document) "no title")
      (warn "Each document should have a TITLE element."))
    ;;
    ;;
    (let ((stsh (document-style-sheet (rc-document rc)
                                      :selected-style selected-style)))
      (setq css::*device* device
            css::*style-sheet* stsh)
      (let ((css::*device* device)
            (css::*style-sheet* stsh))
        (dolist (k (sgml:pt-children pt))
          (when (eq (sgml:gi k) :body)
            ;; hmm?!
            (css:setup-style (rc-device rc) stsh k) ))
        (let* ((fake-parent)
               (box ))
          (setf fake-parent 
            (make-bbox :pt "fake parent"))
          (setf box
            (brender rc pt fake-parent))
          (render-abs-boxen rc)
          (let ((dl (gui:make-display-list
                     :document document
                     :items (bbox->display-list rc box h))))
            (setf (document-display-list document) dl))
          ;;
          (values (abox-bx0 box)
                  (abox-by0 box)
                  (abox-bx1 box)
                  (abox-by1 box))))) ))

(defun bbox->display-list (rc bbox h)
  (let ((res nil))
    (copy-floating-boxen-into-bbox rc bbox)
    (setf (bbox-iheight bbox) (max (bbox-iheight bbox) h))
    (position-hack bbox)
    (calc-bounding-box bbox)
    (push bbox res)
    (nreverse res)))

(defun position-hack (bbox)
  (cond ((abox-p bbox)
         (let* ((pt      (abox-pt bbox))
                (pos     (and pt (css:style-attr (abox-pt bbox) 'css::@position)))
                (left    (and pt (css:style-attr (abox-pt bbox) 'css::@left)))
                (top     (and pt (css:style-attr (abox-pt bbox) 'css::@top)))
                (right   (and pt (css:style-attr (abox-pt bbox) 'css::@right)))
                (bottom  (and pt (css:style-attr (abox-pt bbox) 'css::@bottom))))
           bottom right
           (unless (or (null pos) (eq pos :static))
             )
           (case pos
             (:relative
              (nmove-box bbox 
                         (subst 0 :auto left)
                         (subst 0 :auto top)))))
         (mapc #'position-hack (abox-contents bbox)))
        (t
         nil)))

;; Wärend wir diese Berechnungen vornehmen setzen wir gleich noch die
;; Koordinaten der iboxen entsprechend auf. Die gboxen sollten
;; eigentlich auch Koordinaten und Bounding Boxen bekommen. Gerade die
;; Breitenberechnung von Strings ist ja relativ teuer. Wenn wir das
;; dann getan haben sollte es auch mit schnellem Redraw klappen.

;; Diese ganze Berechnung sollte schon wärend des Renderns geschehen, da
;; wir fuer die Berechnung der bounding boxen schätzungweise noch mal
;; so viel Zeit, wie zum Rendern benoetigen. 

(defun calc-bounding-box (box) ;; -> x0, y0, x1, y1
  (ignore-errors ;;xxx
   (etypecase box
     (bbox
      (let ((x0 (left-border-edge box))
            (x1 (right-border-edge box))
            (y0 (top-border-edge box))
            (y1 (bottom-border-edge box)))
        (dolist (k (abox-contents box))
          (multiple-value-bind (u0 v0 u1 v1) (calc-bounding-box k)
            (setf x0 (min x0 u0)
                  y0 (min y0 v0)
                  x1 (max x1 u1)
                  y1 (max y1 v1))))
        (setf (abox-bx0 box) x0
              (abox-by0 box) y0
              (abox-bx1 box) x1
              (abox-by1 box) y1)
        (values x0 y0 x1 y1)))
     (ibox 
      (calc-ibox-bounding-box box (ibox-x box) (+ (ibox-height box) (ibox-y-oben box)))) )))

(defun calc-ibox-bounding-box (box xo yo) ;; -> x0 y0 x1 y1 w
  (cond ((ibox-p box)
         (let ((x0 xo)
               (y0 (+ yo
                      ;;(+ (ibox-margin-top box))
                      (- (ibox-padding-top box))
                      (- (abox-border-top-width box))
                      (- (ibox-height box))
                      (- (ibox-valign box))))
               (y1 (+ yo
                      ;;(- (ibox-margin-bottom box)) was ist das?
                      (+ (ibox-padding-bottom box))
                      (+ (abox-border-bottom-width box))
                      (ibox-depth box)
                      (- (ibox-valign box))))
               (x1 xo)
               (x 0)
               (y (- yo (ibox-valign box))))
           (setf (ibox-x box) x0)
           (setf (ibox-y-oben box) (- yo (ibox-height box)))    ;Hmm was ist mit valign?!
           (incf x (ibox-margin-left box))
           (incf x (ibox-padding-left box))
           (incf x (abox-border-left-width box))
           (dolist (k (abox-contents box))
             (multiple-value-bind (u0 v0 u1 v1 w) (calc-ibox-bounding-box k (+ x xo) y)
               (incf x w)
               (setf x0 (min x0 u0)
                     y0 (min y0 v0)
                     x1 (max x1 u1)
                     y1 (max y1 v1))))
           (incf x (ibox-padding-right box))
           (incf x (abox-border-right-width box))
           (incf x (ibox-margin-right box))
           (setf x1 (- (max x1 (+ x0 x)) (ibox-margin-right box)))
           (setf (abox-bx0 box) (+ x0 (ibox-margin-left box))
                 (abox-by0 box) y0
                 (abox-bx1 box) x1
                 (abox-by1 box) y1)
           (values x0 y0 x1 y1 x)))
        
        ((gbox-p box)
         (let ((w (gbox-twidth box))) 
           (values xo 
                   (- yo 0
                      (font-desc-ascent (text-style-font
                              (ibox-text-style (gbox-parent box)))))
                   (+ xo w)
                   (+ yo 0
                      (font-desc-descent (text-style-font
                              (ibox-text-style (gbox-parent box)))))
                   w)))
        
        ((rbox-p box)
         (values xo
                 (- yo (rbox-height box))
                 (+ xo (rbox-width box))
                 (+ yo (rbox-depth box))
                 (rbox-width box)))
        (t
         (error "~S" (list 'calc-ibox-bounding-box box))) ))

;; how about a generic boxen mapping function?!

;; glisp material!
(defun mapc/reverse (fun list)
  (cond ((consp list)
         (mapc/reverse fun (cdr list))
         (funcall fun (car list))
         list)))

;;; ---- Believed to be correct -----------------------------------------------

(defsubst rune-width (font rune)
  (css-font-desc-glyph-width font rune))

(defun pt-depth (pt)
  (cond ((null pt) -1)
        ((+ 1 (pt-depth (sgml:pt-parent pt))))))

(defun parse-url* (url)
  (cond ((url:url-p url) url)
        ((url:parse-url url))))

(defun abox-twidth (box)
  (cond ((bbox-p box) (bbox-twidth box))
        ((ibox-p box) (ibox-twidth box))
        ((gbox-p box) (gbox-twidth box))
        ((rbox-p box) (rbox-width box))        
        (t
         (error "~S" (list 'abox-twidth box)))))

(defun ibox-twidth (box)
  (+ (abox-left-leading box)
     (lreduce* #'+ (abox-contents box) :key #'abox-twidth)
     (abox-right-leading box)))

(defun gbox-twidth (box)
  (or (gbox-%twidth box)
      (setf (gbox-%twidth box)
        (iterate-over-runes (lambda (a b c d) (declare (ignore a b c d)))
                            (gbox-runes box) (gbox-start box) (gbox-end box)
                            (ibox-text-style (gbox-parent box))
                            (ibox-white-space (gbox-parent box)))) ))
  

(defun rbox-width (rbox)
  (nth-value 0 (ro/size (rbox-obj rbox))))

(defun rbox-height (rbox)
  (nth-value 1 (ro/size (rbox-obj rbox))))

(defun rbox-depth (rbox)
  (nth-value 2 (ro/size (rbox-obj rbox))))

(defun make-text-style (device 
                        &key (font-family '("times"))
                             (font-weight 400)
                             (font-size 14)
                             (font-style :normal)
                             (font-variant :normal)
                             (letter-spacing :normal)
                             (word-spacing :normal))
  (check-type letter-spacing (or (member :normal) real))
  (check-type word-spacing (or (member :normal) real))
  (make-text-style-prim 
   (find-css-font-desc (device-font-database device)
                       font-family font-weight
                       font-style font-size font-variant)
   font-family font-weight
   font-size font-style font-variant
   letter-spacing word-spacing))

(defun ibox-iwidth (box)
  (- (abox-twidth box)
     (abox-left-leading box)
     (abox-right-leading box)))

(defun find-x0-at (rc y x) 
  (dolist (k (rc-left-floating-boxen rc))
    (when (and (<= (fbox-desc-y k) y)
               (< y (+ (fbox-desc-y k) (fbox-desc-h k))))
      (setf x (max x (+ (fbox-desc-x k) (fbox-desc-w k))))))
  x)

(defun find-x1-at (rc y x) 
  (dolist (k (rc-right-floating-boxen rc))
    (when (and (<= (fbox-desc-y k) y)
               (< y (+ (fbox-desc-y k) (fbox-desc-h k))))
      (setf x (min x (fbox-desc-x k)))))
  x)

(defun handle-clear (rc pt q)
  ;; pt ist der pt der das clear will, damit wir nicht ueber parents hinweg
  ;; clearen.
  ;; pt kann aber auch NIL sein.
  (ecase q
    (:none)
    (:left
     (flush-vertical-margin rc)
     (setf (rc-y rc) (find-clear-left-y rc pt)))
    (:right
     (flush-vertical-margin rc)
     (setf (rc-y rc) (find-clear-right-y rc pt)))
    (:both
     (flush-vertical-margin rc)
     (setf (rc-y rc) (max (find-clear-left-y rc pt) 
                          (find-clear-right-y rc pt)))) ) )

(defun find-clear-left-y (rc pt)
  (lreduce* #'max (rc-left-floating-boxen rc)
            :key (lambda (x)
                   (if (and pt (pt-is-a-parent-of-p (fbox-desc-pt x) pt))
                       (progn  (rc-y rc))
                     (+ (fbox-desc-y x) (fbox-desc-h x))))
            :initial-value (rc-y rc)))

(defun find-clear-right-y (rc pt)
  (lreduce* #'max (rc-right-floating-boxen rc)
            :key (lambda (x)
                   (if (and pt (pt-is-a-parent-of-p (fbox-desc-pt x) pt))
                       (rc-y rc)
                     (+ (fbox-desc-y x) (fbox-desc-h x))))
            :initial-value (rc-y rc)))

(defun pt-is-a-parent-of-p (possible-parent possible-child)
  (cond ((eq possible-parent (sgml:pt-parent possible-child)))
        ((null (sgml:pt-parent possible-child)) nil)
        ((pt-is-a-parent-of-p possible-parent
                              (sgml:pt-parent possible-child)))))

(defun copy-floating-boxen-into-bbox (rc bbox)
  (let ((yl 0))
    (dolist (k (append (rc-left-floating-boxen rc)
                       (rc-right-floating-boxen rc)))
      (setf yl (max yl (+ (fbox-desc-y k) (fbox-desc-h k))))
      (when (fbox-desc-bbox k)
        (add-content bbox (fbox-desc-bbox k))))
    (setf (bbox-iheight bbox)
      (max (bbox-iheight bbox) (- yl (bbox-iy bbox))))))

(defun pt-attr-warn (pt fmt &rest args)
  (declare (ignore pt))
  (apply #'warn fmt args))

;;; ---- Aux ------------------------------------------------------------------

(defun make-skeleton-bbox-for-pt (rc pt width)
  (declare (type sgml::pt pt))
  (let ((res (make-bbox 
              :pt pt
              :white-space (css:style-attr pt 'css:@white-space)
              :background  (pt-backgroud pt)
              :border      (pt-border pt)
              :color       (css:style-attr pt 'css:@color)
              :text-align (css:style-attr pt 'css:@text-align))))
    (declare (type bbox res))
    (setf (abox-padding res)
      (make-padding
       (maybe-resolve-percentage (css:style-attr pt 'css:@padding-top) width)
       (maybe-resolve-percentage (css:style-attr pt 'css:@padding-right) width)
       (maybe-resolve-percentage (css:style-attr pt 'css:@padding-bottom) width)
       (maybe-resolve-percentage (css:style-attr pt 'css:@padding-left) width)))
    (setf (abox-margin-top res)
      (cond ((eq (css:style-attr pt 'css:@margin-top) :auto) 
             0)
            (t 
             (maybe-resolve-percentage (css:style-attr pt 'css:@margin-top)
                                       width))))
    (setf (abox-margin-bottom res)
      (cond ((eq (css:style-attr pt 'css:@margin-bottom) :auto) 
             0)
            (t 
             (maybe-resolve-percentage (css:style-attr pt 'css:@margin-bottom)
                                       width))))
    (multiple-value-bind (ml wd mr) 
        (resolve-width/1 (rc-document rc)
                         (rc-device rc)
                         pt
                         (maybe-resolve-percentage 
                          (css:style-attr pt 'css:@margin-left) width)
                         (maybe-resolve-percentage
                          (css:style-attr pt 'css:@width) width)
                         (maybe-resolve-percentage
                          (css:style-attr pt 'css:@margin-right) width)
                         (- width
                            (abox-padding-left res)
                            (abox-border-left-width res)
                            (abox-border-right-width res)
                            (abox-padding-right res)))
      (setf (abox-margin-left res) (floor ml) ;DEVRND
            (bbox-width res) (ceiling wd) ;DEVRND
            (abox-margin-right res) (floor mr)) ;DEVRND
      res)))

;;; Prinzipielles vorgehen beim tabellen rendern soll nun sein:

;;  1. alles parsen etc.
;;  2. zunaechst horizontale ausrichtung der tabelle bestimmen
;;     [unterscheidung zwischen fester breite und :auto noetig].
;;  3. alle Zellen zunaechst "trocken" rendern
;;  4. Zeilen-hoehen und verticales alignment der Zellen
;;  5. alles zusammenfuegen


;;; CSS-2: 10.3.7 Absolutely positioned, non-replaced elements

;; | The constraint that determines the computed values for these elements is:
;; | 
;; |      'left' + 'margin-left' + 'border-left-width' + 'padding-left' +
;; |      'width' + 'padding-right' + 'border-right-width' + 'margin-right'
;; |      + 'right' = width of containing block
;; | 
;; | 
;; |   1. If 'left' has the value 'auto' while 'direction' is 'ltr', replace
;; |      'auto' with the distance from the left edge of the containing block to
;; |      the left margin edge of a hypothetical box that would have been the
;; |      first box of the element if its 'position' property had been 'static'.
;; |      (But rather than actually computing that box, user agents are free to
;; |      make a guess at its probable position.) The value is negative if the
;; |      hypothetical box is to the left of the containing block.

;;; Wir müssen bei absolute positionierten Boxen noch auf die
;;; Drawing-Order achten!

(defun calc-width-for-abs-pos (x0 container-width
                               left margin-left border-left-width padding-left
                               width 
                               padding-right border-right-width margin-right right)
  ;; [We assume direction is 'ltr'].

  ;; | If 'left' has the value 'auto' [..], replace
  ;; | 'auto' with the distance from the left edge of the containing block to
  ;; | the left margin edge of a hypothetical box that would have been the
  ;; | first box of the element if its 'position' property had been 'static'.
  ;; [As always this is highly obfuscated: 
  ;;  "left margin edge of a hyp. box whould have been the first box of the element"
  ;;  = right-hand-side of left padding of this box.]

  (cond ((eq left :auto)
         ;; "left edge" ist außen oder innen?!
         ;; Wie spielen hier floating boxen rein?!
         (setf left 
           (- x0 0))))
  ;; | 3. If 'width' is 'auto', replace any remaining 'auto' for 'left' or
  ;; |    'right' with '0'.
  (cond ((eq width :auto)
         (setf left  (subst 0 :auto left)
               right (subst 0 :auto right))))
  ;; | 4. If 'left', 'right' or 'width' are (still) 'auto', replace any 'auto' on
  ;; |    'margin-left' or 'margin-right' with '0'.
  (cond ((or (eq left :auto) (eq right :auto) (eq width :auto))
         (setf margin-left  (subst 0 :auto margin-left)
               margin-right (subst 0 :auto margin-right))))
  ;;; It is easily verifiable, that these cases are exhaustive.
  (cond
   ;; |  5. If at this point both 'margin-left' and 'margin-right' are still
   ;; |    'auto', solve the equation under the extra constraint that the two
   ;; |    margins must get equal values.
   ((and (eq margin-left :auto)
         (eq margin-right :auto))
    (let ((lack (- container-width
                   (+ left 
                      border-left-width padding-left 
                      width 
                      border-right-width padding-right
                      right))))
      ;; Well, "equal" for all practical purposes.
      (setf margin-left (floor lack 2)) ;DEVRND
      (setf margin-right (- lack margin-left))))
   ;; | 6. If at this point there is only one 'auto' left, solve the equation for
   ;; |    that value.
   ((eq left :auto)
    (setf left (- container-width
                  (+ margin-left border-left-width padding-left 
                     width 
                     right margin-right border-right-width padding-right))))
   ((eq margin-left :auto)
    (setf margin-left (- container-width
                         (+ left border-left-width padding-left 
                            width 
                            right margin-right border-right-width padding-right))))
   ((eq width :auto)
    (setf width (- container-width
                   (+ left margin-left border-left-width padding-left 
                      right margin-right border-right-width padding-right))))
   ((eq margin-right :auto)
    (setf margin-right (- container-width
                          (+ left margin-left border-left-width padding-left 
                             width 
                             right border-right-width padding-right))))
   ((eq right :auto)
    (setf right (- container-width
                   (+ left margin-left border-left-width padding-left 
                      width 
                      margin-right border-right-width padding-right))))
   (t
    ;; | 7. If at this point the values are over-constrained, ignore the value for
    ;; |    either 'left' (in case 'direction' is 'rtl') or 'right' (in case
    ;; |    'direction' is 'ltr') and solve for that value.
    (setf right 
      (- container-width
         (+ left margin-left border-left-width padding-left 
            width 
            margin-right border-right-width padding-right)))) )

  (values left margin-left width margin-right right) )

(defun calc-height-for-abs-pos (y0 cheight
                                top
                                margin-top border-top-width padding-top
                                height
                                padding-bottom border-bottom-width margin-bottom
                                bottom)
  ;; Um der Schwachsinningkeit der Regel 1 zu entkommen:
  (cond ((and (eq top :auto)
              (zerop (count :auto (list margin-top height margin-bottom bottom))))
         (warn "'top' hack is active.")
         (setf top
           (- cheight
              margin-top border-top-width padding-top
              height
              padding-bottom border-bottom-width margin-bottom bottom))
         (return-from calc-height-for-abs-pos
           (values top margin-top height margin-bottom bottom))))

  (cond ((eq top :auto)
         (setf top y0)))
  ;;  2. If both 'height' and 'bottom' are 'auto', replace 'bottom' with 0.
  (cond ((and (eq height :auto) (eq bottom :auto))
         (setf bottom 0)))
  ;;  3. If 'bottom' or 'height' are (still) 'auto', replace any 'auto' on
  ;;     'margin-top' or 'margin-bottom' with '0'.
  (cond ((or (eq bottom :auto) (eq height :auto))
         (setf margin-top    (subst 0 :auto margin-top)
               margin-bottom (subst 0 :auto margin-bottom))))
  ;;  4. If at this point both 'margin-top' and 'margin-bottom' are still
  ;;     'auto', solve the equation under the extra constraint that the two
  ;;     margins must get equal values.
  (cond ((and (eq margin-top :auto) (eq margin-bottom :auto))
         (let ((lack (- cheight
                        top border-top-width padding-top
                        height
                        padding-bottom border-bottom-width bottom)))
           (setf margin-top (floor lack 2)
                 margin-bottom (- lack margin-top))))
  ;;  5. If at this point there is only one 'auto' left, solve the equation for
  ;;     that value.
        ((eq margin-top :auto)
         (setf margin-top 
           (- cheight
              top border-top-width padding-top
              height
              padding-bottom border-bottom-width margin-bottom bottom)))
        ((eq height :auto)
         (setf height 
           (- cheight
              top margin-top border-top-width padding-top
              padding-bottom border-bottom-width margin-bottom bottom)))
        ((eq margin-bottom :auto)
         (setf margin-bottom 
           (- cheight
              top margin-top border-top-width padding-top
              height
              padding-bottom border-bottom-width bottom)))
        ((eq bottom :auto)
         (setf bottom
           (- cheight
              top margin-top border-top-width padding-top
              height
              padding-bottom border-bottom-width margin-bottom)))
        ;;  6. If at this point the values are over-constrained, ignore the value for
        ;;     'bottom' and solve for that value.
        (t
         (setf bottom
           (- cheight
              top margin-top border-top-width padding-top
              height
              padding-bottom border-bottom-width margin-bottom))))
  (values top margin-top height margin-bottom bottom) )
        
      
#|
10.6.4 Absolutely positioned, non-replaced elements

For absolutely positioned elements, the vertical dimensions must satisfy
this constraint:

     'top' + 'margin-top' + 'border-top-width' + 'padding-top' +
     'height' + 'padding-bottom' + 'border-bottom-width' +
     'margin-bottom' + 'bottom' = height of containing block

(If the border style is 'none', use '0' as the border width.) The solution
to this constraint is reached through a number of substitutions in the
following order:

  1. If 'top' has the value 'auto' replace it with the distance from the top
     edge of the containing block to the top margin edge of a hypothetical
     box that would have been the first box of the element if its 'position'
     property had been 'static'. (But rather than actually computing that
     box, user agents are free to make a guess at its probable position.)
     The value is negative if the hypothetical box is above the containing
     block.

[Was wieder totaler Schwachsinn ist: Wie soll ich dann eine Box am unteren Rand
festmachen?]

  2. If both 'height' and 'bottom' are 'auto', replace 'bottom' with 0.
  3. If 'bottom' or 'height' are (still) 'auto', replace any 'auto' on
     'margin-top' or 'margin-bottom' with '0'.
  4. If at this point both 'margin-top' and 'margin-bottom' are still
     'auto', solve the equation under the extra constraint that the two
     margins must get equal values.
  5. If at this point there is only one 'auto' left, solve the equation for
     that value.
  6. If at this point the values are over-constrained, ignore the value for
     'bottom' and solve for that value.

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Style
;;;

(defstruct computed-style
  (font-size         0        :type real)
  (font-family       nil)
  (font-variant      :normal  :type (member :normal :small-caps))
  (font-style        :normal  :type (member :normal :italic :oblique))
  (font-weight       400      :type (member 100 200 300 400 500 600 700 800 900))
  (letter-spacing    0        :type real)
  (word-spacing      :normal  :type (or (member :normal) real))
  (color             nil)
  (text-style)
  (text-transform    :none    :type (member :none :uppercase :lowercase :capitalize))
  (text-decoration   :none    :type t)
  (background        nil)
  (line-height       nil)
  (orig-line-height  nil)
  (content           nil)
  (vertical-align    :baseline
                     :type (or (member :baseline :sub :super :top :text-top :middle
                                       :bottom :text-bottom  :img-middle)
                               t))) ; percentage

(defun interpret-length (value dpi percentage-base one-em one-ex canvas-width canvas-height)
  (cond ((consp value)
	 (let ((unit (car value))
	       (a    (cdr value)))
	   (case unit
	     (:% (cond ((not (null percentage-base))
			(ceiling (* percentage-base a) 100));DEVRND
		       (t
			value))) ;;;(error "Percentage value not allowed."))))
	     (:px (* 1 a))
	     (:em (* a one-em))
	     (:ex (* a one-ex))
	     (:in (* dpi a))
	     (:cm (* (round (* a dpi) 2.54)));DEVRND
	     (:mm (* (round (* a dpi) 25.4)));DEVRND
	     (:pt (* (round (* a dpi) 72))) ;DEVRND
	     (:pc (* (round (* a dpi) 6))) ;DEVRND
             (:canvas-h-percentage
              ;; DEVRND
              (round (* a canvas-width) 100))
             (:canvas-v-percentage
              ;; DEVRND
              (round (* a canvas-height) 100))
	     (otherwise
	      (error "~S is not a proper css length value." value)) )))
	((eql value 0) 0)
	(t
	 (error "~S is not a proper css length value." value)) ))

(defun style-font-size (style)
  (or (aref (style-att-vector style) css:@font-size) :inherit))
(defun style-font-weight (style)
  (or (aref (style-att-vector style) css:@font-weight) :inherit))
(defun style-font-style (style)
  (or (aref (style-att-vector style) css:@font-style) :inherit))
(defun style-font-variant (style)
  (or (aref (style-att-vector style) css:@font-variant) :inherit))
(defun style-font-family (style)
  (or (aref (style-att-vector style) css:@font-family) :inherit))
(defun style-letter-spacing (style)
  (or (aref (style-att-vector style) css:@letter-spacing) :inherit))
(defun style-word-spacing (style)
  (or (aref (style-att-vector style) css:@word-spacing) :inherit))
(defun style-text-transform (style)
  (or (aref (style-att-vector style) css:@text-transform) :inherit))
(defun style-text-decoration (style)
  (or (aref (style-att-vector style) css:@text-decoration) :none))
(defun style-background-color (style)
  (or (aref (style-att-vector style) css:@background-color) :transparent ))
(defun style-background-image (style)
  (or (aref (style-att-vector style) css:@background-image) :none))
(defun style-background-position (style)
  (or (aref (style-att-vector style) css:@background-position) '((:% . 0) . (:% . 0))))
(defun style-background-attachment (style)
  (or (aref (style-att-vector style) css:@background-attachment) :scroll))
(defun style-background-repeat (style)
  (or (aref (style-att-vector style) css:@background-repeat) :repeat))
(defun style-line-height (style)
  (or (aref (style-att-vector style) css:@line-height) :inherit))
(defun style-content (style)
  (or (aref (style-att-vector style) css:@content) nil))
(defun style-counter-reset (style)
  (or (aref (style-att-vector style) css:@counter-reset) nil))
(defun style-counter-increment (style)
  (or (aref (style-att-vector style) css:@counter-increment) nil))

(defun style-vertical-align (style)
  (or (aref (style-att-vector style) css:@vertical-align) :baseline))

(defconstant +null-background+
    (make-background
     :color      :transparent
     :image      :none
     :repeat     :repeat
     :attachment :scroll
     :position   '((:% . 0) . (:% . 0))))

(defun compute-style (raw-style parent-computed-style device)
  ;; first we care about font-size
  device
  (let ((dpi 72)                        ;xxx
        (cw  800)                       ;xxx
        (ch  800))                      ;xxx
    cw ch
    (labels ((inherited (value parent-value interpreter)
               (if (eq value :inherit)
                   parent-value
                 (funcall interpreter value))))
      (let* ((font-size
              (inherited (style-font-size raw-style) (computed-style-font-size parent-computed-style)
                         (lambda (x)
                           (case x
                             (:xx-small
                              (* (expt css::*font-scaling-factor* -3)
                                 (interpret-length css::*medium-font-size* dpi nil 0 0 0 0)))
                             (:x-small
                              (* (expt css::*font-scaling-factor* -2)
                                 (interpret-length css::*medium-font-size* dpi nil 0 0 0 0)))
                             (:small
                              (* (expt css::*font-scaling-factor* -1)
                                 (interpret-length css::*medium-font-size* dpi nil 0 0 0 0)))
                             (:medium
                              (* (expt css::*font-scaling-factor*  0)
                                 (interpret-length css::*medium-font-size* dpi nil 0 0 0 0)))
                             (:large
                              (* (expt css::*font-scaling-factor*  1)
                                 (interpret-length css::*medium-font-size* dpi nil 0 0 0 0)))
                             (:x-large
                              (* (expt css::*font-scaling-factor*  2)
                                 (interpret-length css::*medium-font-size* dpi nil 0 0 0 0)))
                             (:xx-large
                              (* (expt css::*font-scaling-factor*  3)
                                 (interpret-length css::*medium-font-size* dpi nil 0 0 0 0)))
                             (:larger  (round (* (computed-style-font-size parent-computed-style)
                                                 css::*font-scaling-factor* ))) ;DEVRND
                             (:smaller (round (computed-style-font-size parent-computed-style)
                                              css::*font-scaling-factor*)) ;DEVRND
                             (t
                              (interpret-length x dpi (computed-style-font-size parent-computed-style)
                                                (computed-style-font-size parent-computed-style);;em
                                                (* 1/2 (computed-style-font-size parent-computed-style));;ex xxx
                                                0 0))))))
             (font-variant 
              (inherited (style-font-variant raw-style) (computed-style-font-variant parent-computed-style)
                         #'identity))

             (font-style 
              (inherited (style-font-style raw-style) (computed-style-font-style parent-computed-style)
                         #'identity))

             (font-family 
              (inherited (style-font-family raw-style) (computed-style-font-family parent-computed-style)
                         #'identity))

             (font-weight
              (inherited (style-font-weight raw-style) (computed-style-font-weight parent-computed-style)
                         (lambda (x)
                           (ecase x
                             (:normal 400)
                             (:bold   700)
                             (:bolder
                              (min 900 (+ (computed-style-font-weight parent-computed-style) 300)))
                             ((eq value :lighter)
                              (max 100 (- (computed-style-font-weight parent-computed-style) 300)))
                             ((100 200 300 400 500 600 700 800 900)
                              x)))))
           
             (one-em font-size)
             (one-ex (* 1/2 font-size)) ;xxx

             (word-spacing
              (inherited (style-word-spacing raw-style)
                         (computed-style-word-spacing parent-computed-style)
                         (lambda (x)
                           (case x
                             (:normal x)
                             (otherwise
                              (interpret-length x dpi nil one-em one-ex cw ch))))))

             (letter-spacing
              (inherited (style-letter-spacing raw-style)
                         (computed-style-letter-spacing parent-computed-style)
                         (lambda (x)
                           (case x
                             (:normal 0)
                             (otherwise
                              (interpret-length x dpi nil one-em one-ex cw ch))))))

             (text-transform
              (inherited (style-text-transform raw-style)
                         (computed-style-text-transform parent-computed-style)
                         #'identity))
             (text-decoration
              (inherited (style-text-decoration raw-style)
                         (computed-style-text-decoration parent-computed-style)
                         #'identity))
             (background-color (or (style-background-color raw-style)
                                   :transparent)
               #+NIL (inherited 
                                          (background-color (computed-style-background parent-computed-style))
                                          #'identity))
             (background-image (inherited (style-background-image raw-style)
                                          (background-image (computed-style-background parent-computed-style))
                                          #'interpret-image))
             (background-repeat (inherited (style-background-repeat raw-style)
                                           (background-repeat (computed-style-background parent-computed-style))
                                           #'identity))
             (background-attachment (inherited (style-background-attachment raw-style)
                                               (background-attachment (computed-style-background parent-computed-style))
                                               #'identity))
             (background-position (inherited (style-background-position raw-style)
                                             (background-position (computed-style-background parent-computed-style))
                                             (lambda (x)
                                               (cons (interpret-length (car x) dpi nil one-em one-ex cw ch)
                                                     (interpret-length (cdr x) dpi nil one-em one-ex cw ch)))))
             (background
              (cond ((and (eq background-color :transparent)
                          (eq background-image :none))
                     +null-background+)
                    (t
                     (make-background
                      :color      background-color
                      :image      background-image
                      :repeat     background-repeat
                      :attachment background-attachment
                      :position   background-position))))
             (orig-line-height
              (inherited (style-line-height raw-style)
                         (computed-style-orig-line-height parent-computed-style)
                         (lambda (x)
                           (cond ((eq x :normal)
                                  `(* . 1.2)) ;xxx
                                 ((numberp x)
                                  (cons '* x))
                                 (t
                                  (interpret-length x dpi font-size one-em one-ex cw ch))))))
             (line-height
              (round
               (if (and (consp orig-line-height)
                        (eq (car orig-line-height) '*))
                   (* (cdr orig-line-height)
                      font-size)
                 orig-line-height)))

             (vertical-align
              ;; Hmm what to do about percentage values?
              (style-vertical-align raw-style))
             
             (content
              (inherited (style-content raw-style)
                         (computed-style-content parent-computed-style)
                         #'identity)) )
        (make-computed-style
         :font-size font-size
         :font-family font-family
         :font-variant font-variant
         :font-style font-style
         :font-weight font-weight
         :word-spacing word-spacing
         :letter-spacing letter-spacing
         :text-style
         (make-text-style device 
                          :font-family     font-family
                          :font-weight     font-weight
                          :font-size       font-size
                          :font-style      font-style
                          :font-variant    font-variant
                          :letter-spacing  letter-spacing
                          :word-spacing    word-spacing)
         :text-decoration text-decoration
         :text-transform text-transform
         :background background
         :line-height line-height
         :orig-line-height orig-line-height
         :content       content
         :vertical-align vertical-align) ))))

(defun old-style-vector-to-computed-style (device vec)
  (make-computed-style
   :font-size           (aref vec css:@font-size)
   :font-family         (aref vec css:@font-family)
   :font-variant        (aref vec css:@font-variant)
   :font-style          (aref vec css:@font-style)
   :font-weight         (aref vec css:@font-weight)
   :word-spacing        (aref vec css:@word-spacing)
   :letter-spacing      (aref vec css:@letter-spacing)
   :color               (aref vec css:@color)
   :text-transform      (aref vec css:@text-transform)
   :background          +null-background+;xxx
   :orig-line-height    (aref vec css:@line-height)
   :text-style
   (make-text-style device 
                          :font-family     (aref vec css:@font-family)
                          :font-weight     (aref vec css:@font-weight)
                          :font-size       (aref vec css:@font-size)
                          :font-style      (aref vec css:@font-style)
                          :font-variant    (aref vec css:@font-variant)
                          :letter-spacing  (aref vec css:@letter-spacing)
                          :word-spacing    (aref vec css:@word-spacing))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; New line breaking algorithm

;; first-line pseudo element: 
;; - der re-drawing code in r-x11.html benutzt eigentlich gar kein
;;   CSS:STYLE-ATTR (nur bei position Hack).

;; Was fehlt? 
;;  - rboxen, fboxen, anchors (beide Richtungen).
;;  - list-items 
;;  - run-in, compact (sollten wir hier jetzt mal animplementieren).
;;  - before/after
;;  - positionierte boxen
;;  - letter-spacing
;;  - word-spacing
;;  - oc/cc breiten
;;  - first-char
;;  - first-line
;;  - white space nuking ist noch nicht ganz in Ordnung (was?)
;;  - SHY
;;  - ENSP, EMSP, THINSP, ZWNJ, ZWJ,
;;  - vertical padding
;;  - border around inline elements

;; Done
;;  - text-indent
;;  - <BR>


;;;;
;;;; Structures
;;;;

(defconstant +null-simple-array+ (vector))

(defstruct pc
  ;; a paragraph context, used while formating (breaking) a paragraph.
  (data +null-simple-array+
        :type (simple-array t (*)))     ;the lines data, a simple vector
                                        ; resourced and adjusted by copying
  
  (eline 0 :type fixnum)                ;pointer to end of current line
  (espc  0 :type fixnum)                ;pointer to end of current space
  (eword 0 :type fixnum)                ;pointer to end of current word
  
  line-width            ;the width of the current line
  spc-width             ;the width of the current white space
  word-width            ;the width of the current word
  
  word-fbs              ;reversed list of floating boxen on the current word
  avail                 ;available horizontal space
  
  dangling-ocs          ;dangling open chunks
  dangling-fboxen       ;reversed list of dangling floating boxen (fbox-desc objects)
  
  ts-stack              ;text style stack
  x0 x1                 ;ordinates of current margins
  
  last-was-space-p      ;last character seen was a white space?
  
  block-box             ;bbox we render this paragraph in.
  block-pt              ;pt of block element

  last-was-word-space-p ;last character seen was a white space or a NBSP?
  
  justify               ;How is the current paragraph to be justified

  parent-width          ;Width of "containing block"

  style-stack           ;style stack

  (current-word (make-array 10 :fill-pointer 0 :adjustable t :initial-element :unset)) )

;;   While we assemble lines, we stuff runes and open/close chunks into
;;   the pc-data buffer maintaining three pointers: eline, espace,
;;   eword.
;;  
;;   0 .. eline      The up until now fully assembled line.
;;  
;;   eline .. espc   [if there is any] the white space just before the
;;                   current word. 
;;  
;;   espc .. eword   is the current word, we keep adding stuff here.
;;  
;;   this may look like this:
;;  
;;    
;;                       |     |             |
;;     W h i l e SPC w e | SPC | a s s e m b |
;;                       ^     ^             ^
;;                       |     |             |
;;                     eline  espc         eword
;;                             
;;                             \-------------/
;;                              "current word"

;;; open chunk

;; an open chunk represents the start of an element

(defstruct oc
  pt 
  ts
  border-width                          ;vertical border width at this chunk
  padding-top                           ;padding top
  padding-bottom                        ;padding bottom
  border-style
  border-color
  border-top-width
  border-top-style
  border-top-color
  border-bottom-width
  border-bottom-style
  border-bottom-color
  color
  style-stack
  
  height        ;the height of this element
  depth         ;the depth of this element
  dy            ;absolute vertical align (offset from baseline)
  ro                                    ;possible replaced object -- kludge!
  margin-top                            ;only used for ro
  margin-bottom                         ;only used for ro
  )

;;; close chunk

;; a close chunk represents the end of an element

(defstruct cc
  pt
  border-width                          ;vertical border width at this chunk
  border-style                          ;
  border-color)

(defstruct roc
  ;; replaced object chunk
  )

(defstruct fbc
  ;; floating box chunk
  )
        
(defstruct kern-chunk
  ;; a kern chunk represents some horizontal adjustment
  amount)

(defstruct ro-chunk
  ro)

(defmethod print-object ((object oc) sink)
  (print-unreadable-object (object sink :identity nil :type t)
    (prin1 (sgml:gi (oc-pt object)) sink)))

(defmethod print-object ((object cc) sink)
  (print-unreadable-object (object sink :identity nil :type t)
    (prin1 (sgml:gi (cc-pt object)) sink)))

(defmethod print-object ((object kern-chunk) sink)
  (print-unreadable-object (object sink :identity nil :type t)
    (prin1 (kern-chunk-amount object) sink)))

;;;; some predicates

(defun current-word-fits-p (pc)
  "Does the current open word still fit on the line?"
  (<= (+ (pc-line-width pc) (pc-spc-width pc) (pc-word-width pc))
      (pc-avail pc)))

(defun current-word-is-only-word-p (pc)
  "Is the current open word the only word on the line?"
  ;; This is the case if espc = eline
  ;; xxx we want to replace #'<= by #'=
  (<= (pc-espc pc) (pc-eline pc)))

(defun current-word-empty-p (pc)
  "Is the current word empty?"
  ;; Used while placing floating boxen, should resemble the phrase "any content".
  ;; was: (= (pc-espc pc) (pc-eword pc))
  ;; now:
  (at-the-very-beginning-of-line-p pc)
  )

(defun at-the-very-beginning-of-line-p (pc)
  "Are we at the very beginning of the current line? That is there nothing on
   the current line but some open/close chunks."
  ;; xxx why not (= eword espc)
  (dotimes (i (pc-eword pc) t)
    (when (or (integerp (aref (pc-data pc) i))
              (ro-chunk-p (aref (pc-data pc) i)))
      (return nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Walker
;;;

(defvar *white-space*)

(defun walk (rc pc pt)
  "Within a paragraph, walks a single parse tree node"
  (multiple-value-bind (re re-map) (replaced-element-p (rc-document rc) (rc-device rc) pt)
    (cond
     ;; Check for floating box
     ((floating-box-p pt)
      (add-fbox rc pc (render-floating-box rc pt) nil))
     ;; Replaced Element?
     ((not (null re))
      (case (css:display pt)
        ((:INLINE)
         (walk/replaced-element rc pc pt re re-map))
        ((:BLOCK :LIST-ITEM)
         (walk/block-level-element rc pc pt))))
     ;;
     ;; Forced line break?
     ((eq (sgml:gi pt) :BR)
      (walk/br rc pc) )
     ;; 'normal' nodes
     (t
      ;; the main walker
      (case (css:display pt)
        ((:NONE)
         nil)                           ;nothing to do
        ((:INLINE)
         #+NIL
         (when (rc-first-line-tasks rc)
           (mapc #'(lambda (x) (funcall x rc pc))
                 (prog1 (rc-first-line-tasks rc)
                   (setf (rc-first-line-tasks rc) nil))))

         (walk/inline-element rc pc pt))
        ((:BLOCK :LIST-ITEM)
         (walk/block-level-element rc pc pt))
        ((:PCDATA)
         (when (and (rc-first-line-tasks rc)
                    (not (every #'white-space-rune-p* (sgml::pt-attrs pt))))
           (mapc #'(lambda (x) (funcall x rc pc))
                 (prog1 (rc-first-line-tasks rc)
                   (setf (rc-first-line-tasks rc) nil))))
         (ecase *white-space*
           (:normal (walk-pcdata/normal rc pc pt))
           (:pre    (walk-pcdata/pre rc pc pt))
           (:nowrap (walk-pcdata/nowrap rc pc pt)) )) )))))

(defun floating-box-p (pt)
  "Is the parse tree node 'pt' a floating box?"
  (and (eq (css:display pt) :block)
       (neq (css:style-attr pt 'css:@float) :none)))

(defvar *replaced-object* nil)          ;xxx kludge!

(defun resize-replaced-element-according-to-style (re pt)
  (let ((width (maybe-resolve-percentage (css:style-attr pt 'css:@width)
                                         600)) ;xxx
        (height (maybe-resolve-percentage (css:style-attr pt 'css:@height)
                                          600)))
    ;;
    (cond ((and (realp width) (realp height))
           (ro/resize re width height))
          ((and (realp width) (eq height :auto))
           (ro/resize re
                      ;;xxx because of round
                      (round width)
                      (round (if (zerop (nth-value 0 (ro/intrinsic-size re)))
                                 0
                                 (* width (/ (+ (nth-value 1 (ro/intrinsic-size re))
                                                (nth-value 2 (ro/intrinsic-size re)))
                                             (nth-value 0 (ro/intrinsic-size re))))))))
          ((and (eq width :auto) (realp height))
           (ro/resize re
                      (round            ;xxx
                       (if (zerop (+ (nth-value 1 (ro/intrinsic-size re))
                                     (nth-value 2 (ro/intrinsic-size re))))
                           0
                           (* height (/ (nth-value 0 (ro/intrinsic-size re))
                                        (+ (nth-value 1 (ro/intrinsic-size re))
                                           (nth-value 2 (ro/intrinsic-size re)))))))
                      (round
                       height)))
          ((and (eq width :auto) (eq height :auto))
           ;; nothing to do
           ) )))

(defun walk/replaced-element (rc pc pt re re-map)
  (declare (ignore re-map))
  ;; care about the elements geometry
  (resize-replaced-element-according-to-style re pt)
  ;;
  (cond ((sgml::pt-p re)
         (mapc (curry #'walk rc pc) (sgml:pt-children re)))
        (t
         (let ((*replaced-object* re))  ;kludge
           (walk/add-oc rc pc pt re))
         (walk/add-ro rc pc re)
         (walk/add-cc rc pc pt) )))

(defun walk/inline-element (rc pc pt)
  (walk/add-oc rc pc pt)
  ;; :before pseudo element?
  (unless (sgml::pt-attr pt :%pseudo-class)
    (let ((pe (make-before/after-pseudo-element rc :before pt)))
      (when pe
        (walk rc pc pe))))
  ;;process children
  (dolist (k (sgml:pt-children pt))
    (walk rc pc k))
  ;; :after pseudo element?
  (unless (sgml::pt-attr pt :%pseudo-class)
    (let ((pe (make-before/after-pseudo-element rc :after pt)))
      (when pe
        (walk rc pc pe))))
  ;;              
  (walk/add-cc rc pc pt))

(defun walk/block-level-element (rc pc pt)
  (walk/br rc pc)
  (brender rc pt (pc-block-box pc)))

(defun walk/br (rc pc)
  "Performes an unconditional line break."
  (end-current-word rc pc)
  (spill-line rc pc) )

(defun walk/add-oc (rc pc pt &optional ro)
  (let ((margin-left (maybe-resolve-percentage 
                      (css:style-attr pt 'css:@margin-left)
                      (pc-parent-width pc)))
        (padding-left (maybe-resolve-percentage
                       (css:style-attr pt 'css:@padding-left)
                       (pc-parent-width pc)))
        (border-left-width (css:style-attr pt 'css:@border-left-width))
        )
    (when (realp margin-left) (add-kern pc margin-left))
    (add-oc rc pc pt)                   ;open chunk dazu
    (unless (zerop border-left-width) (add-kern pc border-left-width))
    (when (realp padding-left) (add-kern pc padding-left)) ))

(defun walk/add-cc (rc pc pt)
  (let ((padding-right (maybe-resolve-percentage
                        (css:style-attr pt 'css:@padding-right)
                        (pc-parent-width pc)))
        (margin-right (maybe-resolve-percentage
                       (css:style-attr pt 'css:@margin-right)
                       (pc-parent-width pc)))
        (border-right-width (css:style-attr pt 'css:@border-right-width)))
    (when (realp padding-right) (add-kern pc padding-right))
    (unless (zerop border-right-width) (add-kern pc border-right-width))
    (add-cc rc pc pt)                   ;close chunk dazu
    (when (realp margin-right) (add-kern pc margin-right)) ))

(declaim (inline add-rune* walk-pcdata/normal))

(defun pc-first-letter-p (pc)
  (at-the-very-beginning-of-line-p pc)
  )

(defun add-rune* (rc pc rune white-space
                  &aux first-letter-pseudo-element)
  (declare (type rc rc)
           (type pc pc)
           (type fixnum rune))
  ;; In theory this is correct, in praxis we now keep abuting
  ;; white-space (which winds up non-existing) with first-letter
  ;; pseudo elements.
  #+NIL
  (cond ((and (not (member rune '(32 8 10 13 12)))
              (pc-first-letter-p pc))
         (setf first-letter-pseudo-element
               (let ((e
                      (make-pseudo-element :first-letter (pc-block-pt pc))))
                 #+NIL (setf (sgml::pt-parent e) ?)
                 e))
         (walk/add-oc rc pc first-letter-pseudo-element)))
  (let ((text-transform (computed-style-text-transform 
                           (style-computed (car (pc-style-stack pc))))))
    (case rune
      (160                              ;non breakable space
       (vector-push-extend rune (pc-current-word pc))
       (add-rune pc #x20)
       (setf (pc-last-was-word-space-p pc) t))
      ((32 9)
       (case white-space
         ((:pre :nowrap)
          (vector-push-extend rune (pc-current-word pc))
          (add-rune pc #x20))
         (:normal 
          (add-white-space rc pc)))
       (setf (pc-last-was-word-space-p pc) t)
       (setf (pc-last-was-space-p pc) t))
      ((10)
       (case white-space
         (:pre    (walk/br rc pc))
         (:nowrap
          (vector-push-extend rune (pc-current-word pc))
          (add-rune pc #x20))
         (:normal 
          (add-white-space rc pc)))
       (setf (pc-last-was-word-space-p pc) t)
       (setf (pc-last-was-space-p pc) t))
      (t
       (vector-push-extend rune (pc-current-word pc))
       (ecase text-transform
         (:uppercase
          ;; special hack for ??
          (cond ((= rune #o337)
                 (add-rune pc #/S)
                 (add-rune pc #/S))
                (t
                 (add-rune pc (rune-upcase rune)))))
         (:lowercase (add-rune pc (rune-downcase rune)))
         (:capitalize
          (if (pc-last-was-word-space-p pc)
              (add-rune pc (rune-upcase rune))
            (add-rune pc rune)))
         (:none
          (add-rune pc rune)))
       (setf (pc-last-was-word-space-p pc) nil)
       (setf (pc-last-was-space-p pc) nil) )))
  (when first-letter-pseudo-element
    (walk/add-cc rc pc first-letter-pseudo-element))
  )

(defun walk-pcdata/normal (rc pc pt)
  (declare (type pc pc))
  (loop 
      for ch across (the (simple-array (unsigned-byte 16) (*)) (sgml:pt-attrs (the sgml::pt pt)))
      do (add-rune* rc pc ch :normal)))

(defun walk-pcdata/pre (rc pc pt)
  (declare (type pc pc))
  (loop
      for ch across (sgml:pt-attrs (the sgml::pt pt))
      do (add-rune* rc pc ch :pre)))

(defun walk-pcdata/nowrap (rc pc pt)
  (declare (type pc pc))
  (loop
      for ch across (sgml:pt-attrs (the sgml::pt pt))
      do (add-rune* rc pc ch :nowrap)))

;;;;

(defvar *defunt-level* 1)

(defmacro defunt (name args &body body)
  `(defun ,name ,args
     (format T "~&* ~vT<< ~S" (* 3 *defunt-level*) ',name)
     (debug/show-current-line pc)
     (let ((.res      (let ((*defunt-level* (+ 1 *defunt-level*))) (block ,name ,@body))))
       (debug/show-current-line pc)
       (format T "~&* ~vT>>" (* 3 *defunt-level*))
       .res)))

(defun add-white-space (rc pc)
  (cond
   ;; If last character already was a space character, forget this one.
   ((pc-last-was-space-p pc)
    nil)
   
   ;; Do not add spaces to otherwise empty lines.
   ((at-the-very-beginning-of-line-p pc)
    nil)

   ;; Otherwise add a new white space, update eline, espace, eword
   ;; accordingly
   (t
    (end-current-word rc pc)            ;the current word has ended
    ;; ending the current word might have resulted into a line break,
    ;; so we need to have another look, if we really should add a
    ;; space.
    (unless (at-the-very-beginning-of-line-p pc)
      (let ((width (rune-width (text-style-font (car (pc-ts-stack pc))) #x20)))
        (setf (aref (pc-data pc) (pc-eword pc)) width
              (aref (pc-data pc) (+ 1 (pc-eword pc))) #x20
              (pc-eline pc) (pc-eword pc)
              (pc-espc pc) (+ (pc-eword pc) 2)
              (pc-eword pc) (+ (pc-eword pc) 2)
              (pc-spc-width pc) width))
      (setf (fill-pointer (pc-current-word pc)) 0))))) ;not needed any more

(defun end-current-word (rc pc)
  (cond ((or (current-word-fits-p pc)
             *tex-mode-p*)
         (end-current-word/nobreak rc pc))
        ;;
        ;; Word does not fit anymore. However, if it is the only one, we
        ;; need to add it never the less and spill the line immediatly
        ;; after that.
        ;;
        ;; Wait a minute: we have a last change: if the containing
        ;; block is so narrow because of floating boxen, we might get
        ;; around by clearing; try that first.
        ;;
        ((current-word-is-only-word-p pc)
         (handle-clear rc (pc-block-pt pc) :both) ;(pc-block-pt ..) right?
         (find-margins rc pc)
         (cond ((or (current-word-fits-p pc)
                    *tex-mode-p*)
                (end-current-word/nobreak rc pc))
               ((current-word-is-only-word-p pc)      
                (warn "Overful line box.")
                (end-current-word/nobreak rc pc)
                (spill-line rc pc))
               (t
                (spill-line rc pc))))
        ;;
        ;; Otherwise spill line and start over
        ;;
        (t
         (spill-line rc pc) )))

(defun end-current-word/nobreak (rc pc)
  (setf (fill-pointer (pc-current-word pc)) 0) ;not needed any more
  (setf (pc-eline pc) (pc-eword pc)
        (pc-espc  pc) (pc-eword pc) )
  (incf (pc-line-width pc) (+ (pc-spc-width pc) (pc-word-width pc)))
  (setf (pc-word-width pc) 0
        (pc-spc-width pc) 0)
  ;; We need to render all floating boxen, which still fit
  (setf (pc-word-fbs pc) (reverse (pc-word-fbs pc)))
  (dolist (fb (pc-word-fbs pc))
    (add-fbox rc pc fb nil))
  (setf (pc-word-fbs pc) nil))

(defun debug/show-current-line (pc)
  (format T "~&* ~vT   line: " (* 3 *defunt-level*))
  (labels ((maybe-show-marker (i)
             (when (= i (pc-eline pc))
               (princ "[ELINE]"))
             (when (= i(pc-eword pc))
               (princ "[EWORD]"))
             (when (= i (pc-espc pc))
               (princ "[ESPC]"))))
    (do ((i 0 (+ i 1)))
        ((>= i (max (pc-eline pc) (pc-eword pc) (pc-espc pc)))
         (maybe-show-marker i))
      (maybe-show-marker i)
      (let ((x (aref (pc-data pc) i)))
        (cond ((integerp x)
               (incf i)
               (setq x (aref (pc-data pc) i))
               (if (<= 32 x 127)
                   (format t "~A" (or (code-char x) x))
                   (format t "(~S)" (or (code-char x) x))))
              ((oc-p x)
               (format t "<~A>" (sgml:pt-name (oc-pt x))))
              ((cc-p x)
               (format t "</~A>" (sgml:pt-name (cc-pt x))))
              ((kern-chunk-p x)
               (format t "<KERN ~D>" (kern-chunk-amount x)))
              (t
               (format t "<??>" x)))))
    (terpri)
    (format t "~&* ~vT   current-word: ~S~%" (* 3 *defunt-level*) (pc-current-word pc))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lower level adders
;;;

(defsubst add-to-word (pc x)
  "Add the chunk 'x' to the current word."
  (setf (svref (pc-data pc) (the fixnum (pc-eword pc))) x)
  (incf (the fixnum (pc-eword pc))))

(defsubst add-rune (pc rune)
  "Add the rune 'rune' to the current word." 
  (let ((w (rune-width (text-style-font (car (pc-ts-stack pc)))
                       rune)))
    (add-to-word pc w)
    (add-to-word pc rune)
    (incf (the fixnum (pc-word-width pc)) w) ))

(defun add-kern (pc amount)
  (add-to-word pc (make-kern-chunk :amount amount))
  (incf (the fixnum (pc-word-width pc)) amount))

(defun walk/add-ro (rc pc ro)
  (let ((c (make-ro-chunk :ro ro)))
    (add-to-word pc c)
    (vector-push-extend c (pc-current-word pc)))
  (setf (pc-last-was-word-space-p pc) nil)
  (setf (pc-last-was-space-p pc) nil)
  (incf (the fixnum (pc-word-width pc)) (ro/size ro)))

(defun add-fbox (rc pc fd flag)
  "Add the box as a floating box to the current word or what ever."
  (let ((fbox-width (fbox-desc-w fd)))
    (let ((*package* (find-package :r2)))
      (print (list 'add-fbox
                   '(current-word-empty-p pc) '= (current-word-empty-p pc)
                   'flag '= flag)))
    (cond ((or flag (current-word-empty-p pc))
           ;; the current word is empty, so we place this
           ;; floating box now.
           ;; Does it fit?
           (cond ((or (<= (+ (pc-line-width pc)
                             ;;(pc-word-width pc) -- xxx why was this in?
                             ;;(pc-spc-width pc) -- xxx why was this in?
                             fbox-width)
                          (pc-avail pc))
                      flag)
                  ;; it fits
                  (add-floating-box rc (rc-y rc) fd)
                  (find-margins rc pc) )
                 (t
                  ;; it does not fit, so place it on
                  ;; the list of dangling floating
                  ;; boxen
                  (push fd (pc-dangling-fboxen pc))) ))
          (t
           (debug/show-current-line pc)
           ;; the word is not empty, so add this
           ;; floating box to current word's dangling
           ;; floating boxen.
           (push fd (pc-word-fbs pc)) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - 'clear' scheint überhaupt nicht zu funktionieren
;; - Wir sollten keine Wörten setzen, wo keine hinpassen
;; - Ebenso für floating boxen.

(defun find-margins (rc pc)
  "Reflect new, possible narrower margins."
  (setf (pc-x0 pc)    (find-x0-at rc (rc-y rc) (rc-x0 rc))
        (pc-x1 pc)    (find-x1-at rc (rc-y rc) (rc-x1 rc))
        (pc-avail pc) (floor (- (pc-x1 pc) (pc-x0 pc)))))

;; ------------------------------------------------------------

(defun make-pseudo-element (class pt)
  (let ((r (sgml::copy-pt pt)))
    (setf (sgml::pt-attrs r) 
          (list* :%pseudo-class class (sgml::pt-attrs r)))
    (setf (sgml::pt-cache r) nil)
    ;; kludge
    (when (and (eq (css:display pt) :list-item) (eql class :before))
      (setf (sgml::pt-name r) :%PSEUDO-ELEMENT)
      (setf (sgml::pt-attrs r)
            (list* :style (concatenate 'rod
                                       (rod (format nil "display: ~A; content: '"
                                                    (case (css:style-attr pt 'css:@list-style-position)
                                                      ((:inside)  "inline")
                                                      ((:outside) "marker"))))
                                       ;; xxx we should use LIST-ITEM-STRING instead.
                                       (list-item-string* (css:style-attr pt 'css:@list-style-type)
                                                          (1+ (list-item-index pt)))
                                       (vector #o240)
                                       (rod "';"))
                   (sgml::pt-attrs r)) ))
    r))

(defparameter *before-pe-hash*
    (make-hash-table :test #'eq
                     #+(AND EXCL (version>= 5)) :weak-keys #+(AND EXCL (version>= 5)) t))
(defparameter *after-pe-hash*
    (make-hash-table :test #'eq #+(AND EXCL (version>= 5)) :weak-keys #+(AND EXCL (version>= 5)) t))

(defun pt-style-counter-reset (rc pt)
  (style-counter-reset (lookup-style rc pt)))

(defun pt-style-content (rc pt)
  (style-content (lookup-style rc pt)))

(defun make-before/after-pseudo-element (rc class pt)
  ;; hack for list-item
  (unless (getf (sgml::pt-attrs pt) :%pseudo-class)
    (let ((ht (ecase class (:before *before-pe-hash*) (:after *after-pe-hash*))))
      (or (gethash pt ht)
          (let* ((pe (make-pseudo-element class pt))
                 (content (pt-style-content rc pe)))
            (setf (gethash pt ht) pe)
            (cond ((null content)
                   (setf (gethash pt ht) nil)
                   nil)
                  (t
                   (setf (sgml::pt-children pe)
                         (mapcar
                          (lambda (x)
                            (cond ((and (consp x) (eq (car x) :string))
                                   (sgml::make-pt/low 
                                    :name :pcdata
                                    :parent pe
                                    :attrs (string-rod (cadr x))))
                                  ((and (consp x) (eq (car x) :url))
                                   ;; Hack
                                   (sgml::make-pt/low
                                    :name :img
                                    :parent pe
                                    :attrs (list :src (string-rod (cadr x)))))
                                  ((and (consp x) (eq (car x) :counter))
                                   (destructuring-bind (name &optional (style :decimal)) 
                                       (cdr x)
                                     (sgml::make-pt/low 
                                      :name :pcdata
                                      :parent pe
                                      :attrs
                                      (list-item-string* style (counter-value rc pe name)))))
                                  ((and (consp x) (eq (car x) :counters))
                                   (destructuring-bind (name inter &optional (style :decimal)) 
                                       (cdr x)
                                     (sgml::make-pt/low 
                                      :name :pcdata
                                      :parent pe
                                      :attrs
                                      (grind-counter-values (counter-values rc pe name)
                                                            inter
                                                            style))))
                                  (t
                                   (warn "Unhandled content: ~S: " x)
                                   (sgml::make-pt/low 
                                    :name :pcdata
                                    :parent pe
                                    :attrs (string-rod "")))))
                          content))
                   pe) ))) )))

(defun grind-counter-values (values inter style)
  (cond ((null values) (string-rod ""))
        ((null (cdr values))
         (list-item-string* style (car values)))
        (t
         (concatenate 'rod 
           (grind-counter-values (cdr values) inter style)
           (string-rod inter)
           (list-item-string* style (car values))))))

;;; Hack!

(defun counter-name-equal-p (x y) (string= x y))

(defun counter-value (rc pt name &optional (reset-this-p t))
  ;; functional description
  ;; first walk up until we find a counter reset
  (let ((value 0))
    (let ((scope pt) x)
      (loop
          (when (null scope)
            (return))
          (let ((resets (pt-style-counter-reset rc scope)))
            (when (and (not (and (eq pt scope) (not reset-this-p)))
                       (setq x (find name resets :key #'first :test #'counter-name-equal-p)))
              (setf value (cadr x))
              (return)))
        (setq scope (sgml:pt-parent scope)))
      ;; now walk down
      (block walking
        (labels ((walk (x)
                   (let* ((style (lookup-style rc x))
                          (resets (style-counter-reset style))
                          (incs   (style-counter-increment style)))
                     (unless (and (not (and (eq pt x) (not reset-this-p)))
                                  (find name resets :key #'first :test #'counter-name-equal-p))
                       (let ((y (find name incs :key #'first :test #'counter-name-equal-p)))
                         (when y
                           (incf value (cadr y))))
                       (cond ((eq x pt)
                              (return-from walking))
                             (t
                              (let ((b (gethash x *before-pe-hash*)))
                                (and b (walk b)))
                              (mapc #'walk (sgml:pt-children x))
                              (let ((a (gethash x *after-pe-hash*)))
                                (and a (walk a))))))) ))
          (mapc #'walk (sgml:pt-children (or scope (sgml::pt-root pt))))) )
      (values value scope))))

(defun counter-values (rc pt name &optional (reset-this-p t))
  (multiple-value-bind (value scope) (counter-value rc pt name reset-this-p)
    (if scope
        (list* value (counter-values rc scope name nil)))))

;;;;

;; How to handle text-transform?
;; Well, independently of css:@white-space, we need to indentify individual words.
;; we have
;;  :none       - nothing to do
;;  :uppercase  - just convert to upcase
;;  :lowercase  - just convert to downcase
;;  :capitalize - A char 'c' is upcase, if:
;;                  a. it is the beginning of a word
;;                  b. it is upcase in the source and immediately preceeded by an 
;;                     upcase source letter

;; ------------------------------------------------------------

;; small abstractions
(defstruct style
  att-vector
  papa
  computed)

(defun lookup-style (rc pt &optional papa)
  (let ((is (pt-implicit-style (rc-document rc) pt))
        (ss (pt-style-style (rc-document rc) pt)))
    (let ((res (make-array css::%%--NUMBER-- :initial-element nil)))
      (make-style :att-vector (css::lookup-all-style css::*style-sheet* pt is ss res)
                  :papa papa))))

(defun style-color (style)
  (cond ((null style) :inherit)
        ((or (aref (style-att-vector style) css:@color)
             (style-color (style-papa style))))))

(defun style-stack-color (stack)
  (let ((x (style-color (car stack))))
    (if (eq x :inherit)
        (style-stack-color (cdr stack))
      x)))

(defmacro define-style-accessors (attribute-name default-value interpreter)
  (labels ((symcat (&rest xs)
             (intern (apply #'concatenate 'string (mapcar #'string xs)))))
    `(progn
       (defun ,(symcat 'style- attribute-name) (style)
         (let ((x (aref (style-att-vector style)
                        ,(intern (format nil "@~A" (symbol-name attribute-name)) :css))))
           (if x
               (,interpreter x)
             ,default-value)))
       (defun ,(symcat 'style-stack- attribute-name) (stack)
         (if (null stack)
             :inherit
           (let ((x (,(symcat 'style- attribute-name) (car stack))))
             (if (eq x :inherit)
                 (,(symcat 'style-stack- attribute-name) (cdr stack))
               x)))) )))

(defun interpret-image (value)
  (cond ((eq value :none) :none)
        ((stringp value)
         (url:parse-url value))
        ((url:url-p value)
         value)
        (t
         (warn "Bad CSS image value: ~S." value)
         :none)))

(defun render-normal-block (rc pt parent-box &optional (res nil)
                                             &aux result)
  (declare (ignore parent-box))
  (or res (setf res (make-skeleton-bbox-for-pt rc pt (- (rc-x1 rc) (rc-x0 rc)))))
  (let ((bg-record
         (clim:with-new-output-record (clim-user::*medium*)
          )))
    (setf (bbox-decoration-output-record res) bg-record)
    (let (iy parent-width)
      ;;
      (setf parent-width (- (rc-x1 rc) (rc-x0 rc))) ;xxx right?
      
      ;; setup new margins
      (setf (bbox-ix res) (+ (rc-x0 rc) (bbox-margin-left res)))
      (with-new-margins (rc (+ (rc-x0 rc)
                               (abox-left-leading res))
                            (+ (rc-x0 rc)
                               (abox-left-leading res) (bbox-width res)))
        ;; care for clear
        (handle-clear rc pt (css:style-attr pt 'css:@clear)) ;why doesn't this work?!
        (when (member (sgml:gi pt) `(:hr :table))
          (handle-clear rc pt :both))
        ;; register margin
        (add-vertical-margin rc
                             (bbox-margin-top res) 
                             (lambda (y)
                               (setf (bbox-iy res) 
                                     (or (bbox-iy res) y)) ))
        ;; ???
        (setf iy (rc-y rc))
        ;; care for border or padding:
        (when (or (not (zerop (abox-border-top-width res)))
                  (not (zerop (abox-padding-top res))))
          (flush-vertical-margin rc)
          (setf iy (setf (bbox-iy res) (rc-y rc)))
          (incf (rc-y rc) (abox-border-top-width res))
          (incf (rc-y rc) (bbox-padding-top res)) )
        ;;
        (progn;;clim:with-output-as-presentation (clim-user::*medium* pt 'pt)
          (multiple-value-bind (re re-map) (replaced-element-p (rc-document rc) (rc-device rc) pt)
            (cond ((not (null re))
                       re-map
                   (flush-vertical-margin rc)
                   (resize-replaced-element-according-to-style re pt)
                   (incf (rc-y rc) (nth-value 1 (ro/size re)))
                   (closure/clim-device::medium-draw-ro*
                    clim-user::*medium*
                    re (rc-x0 rc) (rc-y rc))
                   (incf (rc-y rc) (nth-value 2 (ro/size re)))
                   )
                  (t
                   (setf result (render-normal-block-content rc pt res iy parent-width))))
            
            ;; bottom padding
            (when (or (not (zerop (bbox-padding-bottom res)))
                      (not (zerop (abox-border-bottom-width res))))
              (flush-vertical-margin rc))

            ;; iy
            (when (null (bbox-iy res))
              (setf (bbox-iy res) iy))  ;xxx

            ;; eventuelles height
            (let (y-unten)
              (incf (rc-y rc) (bbox-padding-bottom res))
              (incf (rc-y rc) (abox-border-bottom-width res))

              (cond ((not (eq :auto (css:style-attr pt 'css:@height)))
                     (let* ((aheight (- (- (rc-y rc) (bbox-iy res))
                                        (bbox-padding-top res)
                                        (abox-border-top-width res)
                                        (bbox-padding-bottom res)
                                        (abox-border-bottom-width res)))
                            (delta (- (css:style-attr pt 'css:@height) aheight)))
                       (cond ((< delta 0)
                              (warn "height delta is negative."))
                             (t
                              (incf (rc-y rc) delta))))) )
            
              (setf y-unten (rc-y rc))
            
              (setf (bbox-iheight res) 
                    (- (- y-unten (bbox-iy res))
                       (bbox-padding-top res)
                       (abox-border-top-width res)
                       (bbox-padding-bottom res)
                       (abox-border-bottom-width res))))
            ;; bottom margin
            (add-vertical-margin rc (bbox-margin-bottom res))
            (render-block-border-and-background rc res bg-record) )))))
  res)

(defun render-block-border-and-background (rc bbox &optional (bg-record (bbox-decoration-output-record bbox)))
  "Render the border and background of an block element, the border and background is taken form
the bbox argument."
  ;;
  ;; hack ...
  
  ;; Also it turns out, that this routine burns a lot of cycles, why?
  (clim:clear-output-record bg-record)
  (let* ((background (bbox-background bbox))
         (border     (bbox-border bbox))
         (background-color
          (let ((x (ignore-errors (clim-user::parse-x11-color (background-color background)))))
            (and x ;; (not (eq x clim:+white+))
                 x))))
    (when (or background-color
              background
              (and border
                   (or
                    (not (zerop (border-top-width border)))
                    (not (zerop (border-left-width border)))
                    (not (zerop (border-bottom-width border)))
                    (not (zerop (border-right-width border))))))
      (clim:with-output-recording-options (clim-user::*medium* :record t :draw nil)
       (multiple-value-bind (x y w h) (bbox-border-coordinates bbox)
         (let ((new-record 
                (clim:with-new-output-record (clim-user::*medium*)
                 (when background
                   (clim-draw-background clim-user::*medium*
                                         x y (+ x w) (+ y h)
                                         (background-color background)))
                 (when background
                   (closure/clim-device::x11-draw-background
                    (rc-document rc) clim-user::*medium* background
                    x y w h))
                 (when t #+NIL
                       (and (abox-border-top-width bbox)
                            (> (abox-border-top-width bbox) 0))
                   (clim-draw-border clim-user::*medium*
                                     x y (+ x w) (+ y h)
                                     (abox-border-top-width bbox)
                                     (abox-border-top-style bbox)
                                     (if (abox-border-top-width bbox) (abox-border-top-color bbox) "black")
                                     (abox-border-right-width bbox)
                                     (abox-border-right-style bbox)
                                     (if (abox-border-right-width bbox) (abox-border-right-color bbox) "black")
                                     (abox-border-bottom-width bbox)
                                     (abox-border-bottom-style bbox)
                                     (if (abox-border-bottom-width bbox) (abox-border-bottom-color bbox) "black")
                                     (abox-border-left-width bbox)
                                     (abox-border-left-style bbox)
                                     (if (abox-border-left-width bbox) (abox-border-left-color bbox) "black"))))))
           (clim:delete-output-record new-record (clim:output-record-parent new-record))
           (clim:add-output-record new-record bg-record) ))))))

(defun clim-draw-background (medium x1 y1 x2 y2
                             background-color)
  (when (and background-color (not (eql background-color :transparent)))
    (clim:draw-rectangle* medium
     x1 y1 x2 y2
     :ink (css-color-ink background-color))))

;;; Border

(defun css-color-ink (color)
  ;; xxx, we still sometimes wind up with bogus values here
  (if (stringp color)
      (clim-user::parse-x11-color color)
      clim:+black+))

(defun 3d-light-color (base-color)
  (multiple-value-bind (i h s) (clim:color-ihs base-color)
    (clim:make-ihs-color 1.5 h s)))

(defun 3d-dark-color (base-color)
  (multiple-value-bind (i h s) (clim:color-ihs base-color)
    (clim:make-ihs-color .8 h s)))

(defun clim-draw-border (medium x1 y1 x2 y2
                         border-top-width border-top-style border-top-color
                         border-right-width border-right-style border-right-color
                         border-bottom-width border-bottom-style border-bottom-color
                         border-left-width border-left-style border-left-color)
  (let* ((ix1 (+ x1 border-left-width))
         (iy1 (+ y1 border-top-width))
         (ix2 (- x2 border-right-width))
         (iy2 (- y2 border-bottom-width))
         (mx1 (/ (+ x1 ix1) 2))
         (my1 (/ (+ y1 iy1) 2))
         (mx2 (/ (+ x2 ix2) 2))
         (my2 (/ (+ x2 iy2) 2))
         )
    (labels ((m (x1 y1 x2 y2 x3 y3 x4 y4 style ink ink2 ink3 w)
               (case style
                 ((:solid)
                  (clim:draw-polygon* medium
                   (list x1 y1 x2 y2 x3 y3 x4 y4)
                   :filled t :ink ink))
                 ((:dotted)
                    (clim:draw-line* medium
                     (/ (+ x1 x2) 2)
                     (/ (+ y1 y2) 2)
                     (/ (+ x3 x4) 2)
                     (/ (+ y3 y4) 2)
                     :ink ink
                     :line-thickness w
                     :line-cap-shape :round
                     :line-dashes (vector w (* 3 w)))) 
                 ((:dashed)
                  ;; this triggers an CLX value-error
                  #+NIL
                  (clim:draw-line* medium
                     (/ (+ x1 x2) 2)
                     (/ (+ y1 y2) 2)
                     (/ (+ x3 x4) 2)
                     (/ (+ y3 y4) 2)
                     :ink ink
                     :line-thickness w
                     :line-cap-shape :square
                     :line-dashes (vector (* 3 w) (* 3 w)))) 
                 ((:double)
                  (clim:draw-polygon* medium
                   (list x1 y1
                    (/ (+ x1 x1 x2) 3)
                    (/ (+ y1 y1 y2) 3)
                    (/ (+ x3 x4 x4) 3)
                    (/ (+ y3 y4 y4) 3)
                    x4 y4)
                   :filled t
                   :ink ink)
                  (clim:draw-polygon* medium
                   (list 
                    (/ (+ x1 x2 x2) 3) (/ (+ y1 y2 y2) 3)
                    x2 y2
                    x3 y3
                    (/ (+ x3 x3 x4) 3) (/ (+ y3 y3 y4) 3))
                   :filled t
                   :ink ink))
                 ((:groove)
                  (clim:draw-polygon* medium
                   (list x1 y1
                    (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)
                    (/ (+ x3 x4) 2) (/ (+ y3 y4) 2)
                     x4 y4)
                   :filled t :ink ink2)
                  (clim:draw-polygon* medium
                   (list 
                    (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)
                    x2 y2
                    x3 y3
                    (/ (+ x3 x4) 2) (/ (+ y3 y4) 2))
                   :filled t :ink ink3)
                  )
                 ((:ridge)
                  (clim:draw-polygon* medium
                   (list x1 y1
                    (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)
                    (/ (+ x3 x4) 2) (/ (+ y3 y4) 2)
                     x4 y4)
                   :filled t :ink ink3)
                  (clim:draw-polygon* medium
                   (list 
                    (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)
                    x2 y2
                    x3 y3
                    (/ (+ x3 x4) 2) (/ (+ y3 y4) 2))
                   :filled t :ink ink2))
                 ((:inset)
                  (clim:draw-polygon* medium
                   (list x1 y1 x2 y2 x3 y3 x4 y4)
                   :filled t :ink ink2))
                 ((:outset)
                  (clim:draw-polygon* medium
                   (list x1 y1 x2 y2 x3 y3 x4 y4)
                   :filled t :ink ink3))
                 )))
      (m  x1 y1  ix1 iy1  ix2 iy1  x2 y1 border-top-style
          (css-color-ink border-top-color)
          (3d-dark-color (css-color-ink border-top-color)) (3d-light-color (css-color-ink border-top-color))
          border-top-width)
      (m  x2 y1  ix2 iy1  ix2 iy2  x2 y2 border-right-style
          (css-color-ink border-right-color)
          (3d-light-color (css-color-ink border-right-color)) (3d-dark-color (css-color-ink border-right-color))
          border-right-width)
      (m  x2 y2  ix2 iy2  ix1 iy2  x1 y2 border-bottom-style
          (css-color-ink border-bottom-color)
          (3d-light-color (css-color-ink border-bottom-color)) (3d-dark-color (css-color-ink border-bottom-color))
          border-bottom-width)
      (m  x1 y2  ix1 iy2  ix1 iy1  x1 y1 border-left-style
          (css-color-ink border-left-color)
          (3d-dark-color (css-color-ink border-left-color)) (3d-light-color (css-color-ink border-left-color))
          border-left-width) )))
    

(defun render-normal-block-content (rc pt res iy parent-width)
  ;; okay, now we are ready to render the contents
  ;; We have a state:
  (let* ((qqq (make-array (if *tex-mode-p* 20000 2000) :initial-element :unset)))
    (declare (dynamic-extent qqq))
    (let ((pc (make-pc
               :data qqq
               :eline 0
               :espc 0
               :eword 0
               :line-width 0
               :spc-width 0
               :word-width 0
               :word-fbs nil
               :avail 0
               :dangling-ocs nil
               :dangling-fboxen nil
               :ts-stack (list (pt-text-style (rc-device rc) pt))

               :style-stack ;;grff
               ;;(list (lookup-style rc pt))
               (list (make-style :att-vector
                                 (progn
                                    (css::ensure-style pt)
                                    (sgml:pt-cache pt))
                                 :computed
                                 (old-style-vector-to-computed-style
                                  (rc-device rc)
                                  (progn
                                    (css::ensure-style pt)
                                    (sgml:pt-cache pt)))))

               :x0 (find-x0-at rc (rc-y rc) (rc-x0 rc))
               :x1 (find-x1-at rc (rc-y rc) (rc-x1 rc))
               :last-was-space-p t
               :block-box res
               :block-pt pt
               :justify (css:style-attr pt 'css:@text-align)
               :parent-width parent-width)))
      ;;
      (declare (dynamic-extent pc))
      (setf (pc-avail pc) (floor (- (pc-x1 pc) (pc-x0 pc))))
      (let ((*white-space* (css:style-attr pt 'css:@white-space))
            (text-indent (maybe-resolve-percentage (css:style-attr pt 'css:@text-indent)
                                                   parent-width )))
        (let* ((first-line-pseudo-element (sgml::copy-pt pt))
               (css::*first-line-element* first-line-pseudo-element)
               )
          ;; kludge: nuke the STYLE attribute of the pseudo element
          (remf (sgml::pt-attrs first-line-pseudo-element) :style)
          ;;
          (let ((re (replaced-element-p (rc-document rc) (rc-device rc) pt)))
            (cond (re
                   (cond ((sgml::pt-p re)
                          (mapc (curry #'walk rc pc) re))
                         (t
                          (ro/resize re 
                                     (bbox-width res) 
                                     (if (realp (css:style-attr pt 'css:@height))
                                         (css:style-attr pt 'css:@height)
                                       nil))
                          ;; breite?!
                          (walk/replaced-element rc pc pt re nil))))
                  (t
                   ;; care for text-indent
                   (cond ((and (numberp text-indent) (/= text-indent 0))
                          (add-kern pc (round text-indent))))
                   (add-oc rc pc first-line-pseudo-element)
                   ;; :before pseudo element?
                   (unless (sgml::pt-attr pt :%pseudo-class)
                     (let ((pe (make-before/after-pseudo-element rc :before pt)))
                       (cond ((null pe)
                              nil)
                             ((eq (css:style-attr pe 'css:@display) :marker)
                              ;; Markers are now somewhat special, they render to the margin
                              ;; area, but otherwise vertically align with the line -- we go for
                              ;; a quick hack here and just add then to the first line.

                              ;; The correct way is to have a kind of absoulte kern, which will
                              ;; move the marker just to the right place -- and we might still
                              ;; have a problem with multiple before/marker elements.
                              #+NIL (dprint "marker-seen")
                              #+NIL (dprint "Its maximum width is ~S." (pt-maximum-width rc pe))
                              (push (lambda (rc pc)
                                      (let ((w (pt-maximum-width rc pe)))
                                        (add-kern pc (- w)))
                                      (walk/add-oc rc pc pe)
                                      (mapc (curry #'walk rc pc) (sgml:pt-children pe))
                                      (walk/add-cc rc pc pe))
                                    (rc-first-line-tasks rc)) )
                             (t
                              (walk rc pc pe)))))
              
                   ;; über alle Kinder
                   (mapc (curry #'walk rc pc) (sgml:pt-children pt))

                   ;; :after pseudo element?
                   (unless (sgml::pt-attr pt :%pseudo-class)
                     (let ((pe (make-before/after-pseudo-element rc :after pt)))
                       (when pe
                         (walk rc pc pe))))
              
                   )))
          ;;
          (cond (*tex-mode-p*
                 (end-current-word rc pc)
                 (tex-break-para rc pc)
                 )
                (t
                 (end-current-word rc pc)
                 (spill-line rc pc)
                 (unless (zerop (length (pc-current-word pc))) ;xxx condition right?
                   (end-current-word rc pc)
                   (spill-line rc pc)))) ))
      nil)))

(defun tex-break-para (rc pc)
  (let ((end (pc-eline pc))
        (line (make-array 100 :fill-pointer 0 :adjustable t)))
    (do ((i 0 (+ i 1)))
        ((= i end))
      (let ((x (aref (pc-data pc) i)))
        (etypecase x
          (integer
           (let ((c (aref (pc-data pc) (+ i 1))))
             (cond ((= c 32)
                    (vector-push-extend (texpara::make-white-space-glue x)
                                        line))
                   (t
                    (vector-push-extend (texpara::make-box :width x :data c)
                                        line)))
             (incf i)))
          (kern-chunk
           (vector-push-extend (texpara::make-kern x (kern-chunk-amount x))
                               line))
          ((or oc cc)
           (vector-push-extend (texpara::make-kern x 0)
                               line))
          (ro-chunk
           (vector-push-extend (texpara::make-kern x (ro/size (ro-chunk-ro x)))
                               line))
          )))
    (vector-push-extend (texpara::copy-glue texpara::+hfil-glue+)
                        line)
    (dolist (line
              (texpara::format-paragraph (coerce line 'list)
                                         (pc-avail pc)))
      (setf (pc-eline pc) 0)
      (dolist (elt line)
        (etypecase elt
          (texpara::box
           (cond ((texpara::box-font elt)
                  (setf (aref (pc-data pc) (pc-eline pc)) (texpara::box-font elt))
                  (incf (pc-eline pc)) )
                 (t
                  (setf (aref (pc-data pc) (pc-eline pc)) (texpara::box-width elt))
                  (incf (pc-eline pc))
                  (setf (aref (pc-data pc) (pc-eline pc)) (texpara::box-data elt))
                  (incf (pc-eline pc))))
           )
          (texpara::glue
           (setf (aref (pc-data pc) (pc-eline pc))
                 (make-kern-chunk :amount (+ (texpara::glue-width elt)
                                             (texpara::glue-assigned elt))))
           (incf (pc-eline pc)))
          ))
      (setf (pc-espc pc) (pc-eline pc))
      (setf (pc-eword pc) (pc-eline pc))
      (spill-line rc pc))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-oc (rc pc pt &optional (adder (lambda (oc)
                                           (vector-push-extend oc (pc-current-word pc))
                                           (add-to-word pc oc )
                                           )))
  ;; add an open-chunk for pt
  ;; xxx ist die addierte breite richtig, wenn z.b. border-style :none ist?
  ;; xxx was passiert, wenn dieses chunk umkopiert wird?
  (let ((ts (pt-text-style (rc-device rc) pt))
        (b  (if (eq pt css::*first-line-element*) ;hack
                nil 
              (pt-border pt)))
        w)

    (unless (car (pc-style-stack pc))
      (error "Oops corrupted style-stack? ~S." (pc-style-stack pc)))
    
    (let* ((old-computed-style (style-computed (car (pc-style-stack pc))))
           (new-style (lookup-style rc pt))
           (new-computed-style (compute-style new-style old-computed-style (rc-device rc))))
      (setf (style-computed new-style) new-computed-style)
      (push new-style (pc-style-stack pc)))

    (setf ts (computed-style-text-style (style-computed (car (pc-style-stack pc)))))

    (push ts (pc-ts-stack pc))
    (let ()
      ;;(css::find-style css::*style-sheet* pt nil))
      (let ((oc (make-oc :pt pt :ts ts
                         :border-width (setq w (if b (border-left-width b) 0))
                         :border-style (if b (border-left-style b) :none)
                         :border-color (if b (border-left-color b) "black") ;xx??
                         :border-top-width (if b (border-top-width b) 0)
                         :border-top-style (if b (border-top-style b) :none)
                         :border-top-color (if b (border-top-color b) "black")
                         :border-bottom-width (if b (border-bottom-width b) 0)
                         :border-bottom-style (if b (border-bottom-style b) :none)
                         :border-bottom-color (if b (border-bottom-color b) "black")
                         :padding-top (maybe-resolve-percentage
                                       (css:style-attr pt 'css:@padding-top)
                                       (pc-parent-width pc))
                         :padding-bottom (maybe-resolve-percentage
                                          (css:style-attr pt 'css:@padding-bottom)
                                          (pc-parent-width pc))
                         :color (style-stack-color (pc-style-stack pc))
                         :style-stack (pc-style-stack pc)
                         :ro *replaced-object*
                         :margin-top (maybe-resolve-percentage
                                       (css:style-attr pt 'css:@margin-top)
                                       (pc-parent-width pc))
                         :margin-bottom (maybe-resolve-percentage
                                          (css:style-attr pt 'css:@margin-bottom)
                                          (pc-parent-width pc))
                         )))
        (funcall adder oc)))
    (incf (the fixnum (pc-word-width pc)) w) ))

(defun add-cc (rc pc pt)
  rc
  ;; xxx ist die addierte breite richtig, wenn z.b. border-style :none ist?
  ;; xxx was passiert, wenn dieses chunk umkopiert wird?
  (let ((b (pt-border pt))
        w)
    ;; add a close-chunk for pt
    (let ((cc (make-cc :pt pt
                             :border-width (setq w (if b (border-right-width b) 0))
                             :border-style (if b (border-right-style b) :none)
                             :border-color (if b (border-right-color b) "black") )))
      (vector-push-extend cc (pc-current-word pc))
      (add-to-word pc cc))
    (pop (pc-ts-stack pc))
    (pop (pc-style-stack pc))
    (incf (the fixnum (pc-word-width pc)) w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Justifying lines works by wraping a justify box around the relevant
;; parts.

(defun draw-text-decoration (xx1 yy xx text-decoration color)
  (when (consp text-decoration)
    (dolist (deco text-decoration)
      (case deco
        (:underline
         (clim:draw-line* clim-user::*medium*
          xx1 (+ yy 2) xx (+ yy 2) :ink (clim-user::parse-x11-color color)))
        (:overline
         ;; xxx hack
         (clim:draw-line* clim-user::*medium*
          xx1 (- yy 12) xx (- yy 12) :ink (clim-user::parse-x11-color color)))
        (:line-through
         (clim:draw-line* clim-user::*medium*
          xx1 (- yy 6) xx (- yy 6) :ink (clim-user::parse-x11-color color)))
        ))))

(defun spill-line-aux (rc pc &aux first-line-was-here
                       (yet-flushed-p nil))
  ;; To get all this somewhat more regular, we concatenate the danging
  ;; open chunks and the rest of the line together into a new line. 
  ;; That way we could use a simple recursive procedure to maintain
  ;; the stack.
  ;;
  ;; also it would be a nice idea if we could have an extra open chunk
  ;; for the line itself.
  ;;
  (debug/show-current-line pc)
  (let ((line-chunks (make-array (+ (* 1 (length (pc-dangling-ocs pc)))
                                    (pc-eline pc))
                                 :initial-element :unset))
        (number-of-dangling-ocs (length (pc-dangling-ocs pc))))
    (declare (type (simple-array t (*)) line-chunks))
    (let ((i 0))
      (declare (type fixnum i))
      (dolist (k (pc-dangling-ocs pc))
        (setf (aref line-chunks i) k)
        (setf i (the fixnum (+ i 1))))
      (let ((data (pc-data pc)))
        (loop 
            for src fixnum from 0 below (pc-eline pc)
            for dst fixnum from i
            do
            (setf (aref line-chunks dst) (aref data src)))))
    ;;
    (multiple-value-bind (line-height line-depth)
        (vertically-align-line-2 rc pc line-chunks (length line-chunks))
      (let ((ocs nil)
            yy 
            (i 0)
            (end (length line-chunks))
            ;;
            ;; Now care for horizontal alignment:
            ;;
            (xx (let* ((line-width (pc-line-width pc))
                       (lack (- (- (pc-x1 pc) (pc-x0 pc)) line-width))
                       (line-box-x
                        (+ (pc-x0 pc)
                           (case (pc-justify pc)
                             (:left     0)
                             (:right    lack)
                             (:center   (floor lack 2))
                             (otherwise 0)))))
                  line-box-x))
            ;;
            (dy 0))
        (declare (type fixnum i))

        (unless yet-flushed-p
          (flush-vertical-margin rc)
          (setf yy (+ (rc-y rc) line-height))
          (setf yet-flushed-p t))

        (labels ((foo-int (i)
                   (declare (type fixnum i))
                   ;; -> new-i
                   (let ((j i))
                     (declare (type fixnum j))
                     (do ()
                         ((or (= j end)
                              (not (integerp (svref 
                                              (the (simple-array t (*)) line-chunks)
                                              (the fixnum j))))))
                       (setf j (the fixnum (+ j 2))))
                     (let ((rod (make-rod (floor (- j i) 2)))
                           (w 0))
                       (declare (type fixnum w))
                       (do ((k i (+ k 2))
                            (p 0 (+ p 1)))
                           ((= k j))
                         (declare (type fixnum k p))
                         (setf w (the fixnum
                                   (+ (the fixnum w)
                                      (the fixnum
                                        (svref (the (simple-array t (*)) line-chunks)
                                               (the fixnum k))))))
                         (setf (%rune rod p)
                               (svref (the (simple-array t (*)) line-chunks)
                                      (the fixnum (1+ k)))))
                       ;;
                       
                       ;;
                       (let ((color (if ocs
                                        (oc-color (car ocs))
                                        (computed-style-color
                                         (style-computed
                                          (car (last (pc-style-stack pc))))))))
                         (clim:with-drawing-options (clim-user::*medium*
                                                     :ink (clim-user::parse-x11-color
                                                           color))
                          (let ((ts (if ocs
                                        (oc-ts (car ocs))
                                        (computed-style-text-style
                                         (style-computed (car (last (pc-style-stack pc))))) )))
                            (clim-draw-runes
                             clim-user::*medium* xx (+ yy dy) rod 0 (length rod)
                             ts
                             :pre))))
                       (incf xx w))
                     (setf i j))
                   i)
                 ;;
                 ;;
                 ;;
                 (foo-oc (x i)
                   (declare (type fixnum i))
                   (let ((this-is-dangling-oc-p (< i number-of-dangling-ocs))
                         ;; kludge!
                         (ody dy)
                         (replaced-element (oc-ro x))
                         (bg-record(clim:with-new-output-record (clim-user::*medium*)
                                    )))
                     (push x ocs)
                     (let* ((style (style-computed (car (oc-style-stack x))))
                            (text-decoration (computed-style-text-decoration style))
                            (color           (if ocs
                                                 (oc-color (car ocs))
                                                 (computed-style-color
                                                  (style-computed
                                                   (car (last (pc-style-stack pc))))))))
                       (let ((xx1 xx))
                         (cond ((maybe-pt-imap rc (oc-pt x))
                                (setq **x1* xx)
                                (let ()
                                  (clim:with-output-as-presentation
                                   (clim-user::*medium*
                                    (url:unparse-url
                                     (hyper-link-url (imap-area-link (maybe-pt-imap rc (oc-pt x)))))
                                    'clim-user::url)
                                   (setf dy (oc-dy x))
                                   (setf i (foo (+ i 1))))))
                               (t
                                (setf dy (oc-dy x))
                                (setf i (foo (+ i 1)))))
                         (unless replaced-element
                           (draw-text-decoration xx1 (+ yy (oc-dy x)) xx text-decoration color))
                         (let* ((cc (let ((cc (svref (the (simple-array t (*)) line-chunks)
                                                     (the fixnum (1- i)))))
                                      (and (cc-p cc)
                                           (eq (cc-pt cc) (oc-pt x))
                                           cc)))
                                (border-top-width    (oc-border-top-width x))
                                (border-top-style    (oc-border-top-style x))
                                (border-top-color    (oc-border-top-color x))
                                (border-bottom-width (oc-border-bottom-width x))
                                (border-bottom-style (oc-border-bottom-style x))
                                (border-bottom-color (oc-border-bottom-color x))
                                (border-left-width   (oc-border-width x))
                                (border-left-style   (oc-border-style x))
                                (border-left-color   (oc-border-color x))
                                (border-right-width  (if cc (cc-border-width cc) 0))
                                (border-right-style  (if cc (cc-border-style cc) :none))
                                (border-right-color  (if cc (cc-border-color cc) "black"))
                                (padding-top         (oc-padding-top x))
                                (padding-bottom      (oc-padding-bottom x))
                                )
                           (multiple-value-bind (x1 y1 x2 y2)
                               (if replaced-element
                                   (values
                                    xx1 (+ yy (oc-dy x) (- (nth-value 1 (ro/size replaced-element)))
                                           (- border-top-width)
                                           (- border-bottom-width)
                                           (- padding-bottom)
                                           (- padding-top)
                                           (- (oc-margin-bottom x))
                                           ;;(- (oc-margin-top x))
                                           )
                                    xx  (+ yy (oc-dy x) (nth-value 2 (ro/size replaced-element))
                                           (- (oc-margin-bottom x))
                                           )
                                    )
                                   (values xx1 (- (+ yy (oc-dy x))
                                                  (text-style-ascent (oc-ts x))
                                                  border-top-width padding-top)
                                           xx  (+ (+ yy (oc-dy x))
                                                  (text-style-descent (oc-ts x))
                                                  border-bottom-width padding-bottom)))

                             (let ((new-record
                                    (clim:with-new-output-record (clim-user::*medium*)
                                     (clim-draw-background clim-user::*medium*
                                      x1 y1 x2 y2
                                      (background-color (computed-style-background style)))
                                     (clim-draw-border clim-user::*medium*
                                      x1 y1 x2 y2
                                      BORDER-TOP-WIDTH BORDER-TOP-STYLE BORDER-TOP-COLOR
                                      BORDER-RIGHT-WIDTH BORDER-RIGHT-STYLE BORDER-RIGHT-COLOR
                                      ;; 0 :none "black"
                                      BORDER-BOTTOM-WIDTH BORDER-BOTTOM-STYLE BORDER-BOTTOM-COLOR
                                      (if this-is-dangling-oc-p 0 BORDER-LEFT-WIDTH)
                                      BORDER-LEFT-STYLE BORDER-LEFT-COLOR) )))
                               (clim:delete-output-record new-record (clim:output-record-parent new-record))
                               (clim:add-output-record new-record bg-record)
                               )
                             ))))
                     (setf dy ody)
                     )
                   i)
                 ;;
                 (foo (i)
                   (do ()
                       ((= i end)
                        i)
                     (let ((x (svref (the (simple-array t (*)) line-chunks) (the fixnum i))))
                         
                       (typecase x
                         (integer
                          (let ((j (foo-int i)))
                            (setf i j)))
                         (oc
                          (setf i (foo-oc x i)))
                         (CC
                          (when (maybe-pt-imap rc (cc-pt x))
                            (when yy
                              (let ()
                                '(clim:with-drawing-options (clim-user::*medium* :ink clim:+blue+)
                                  (clim:draw-line* clim-user::*medium* **x1* (+ yy 1) (+ xx 0) (+ yy 1) )))))
                          (let ((oc (pop ocs)))
                            (unless oc
                              (error "More close chunks as open chunks, please debug."))
                            (when oc    ;xxx shouldn't happen
                              (decf dy (oc-dy oc))))
                          (return-from foo (+ i 1)))
                         (KERN-CHUNK
                          (incf xx (kern-chunk-amount x))
                          (incf i 1))
                         (RO-CHUNK
                          ;; hmm what is this?
                          (unless yet-flushed-p
                            (flush-vertical-margin rc)
                            (setf yy (+ (rc-y rc) line-height))
                            (setf yet-flushed-p t))
                          (closure/clim-device::medium-draw-ro*
                           clim-user::*medium*
                           (ro-chunk-ro x) xx (- (+ yy dy)
                                                 (oc-margin-bottom (car ocs))
                                                 (oc-padding-bottom (car ocs))
                                                 (oc-border-bottom-width (car ocs))
                                                 ))
                          (incf xx (ro/size (ro-chunk-ro x)))
                          (incf i 1))
                         (t
                          (incf i)) )))) )
          (let ((xx1 xx))
            (foo 0)

            ;;
            ;; setup new dangling open chunks
            ;;
            (setf (pc-dangling-ocs pc) nil)
            (let ((dangling-ocs nil))
              (do ()
                  ((null ocs))
                (let ((x (copy-oc (pop ocs))))
                  ;; clear the open chunks border
                  (cond ((eq (oc-pt x) css::*first-line-element*)
                         ;; This is the :first-line pseudo element.
                         ;; we leave that out
                         (setf first-line-was-here t))
                        (t
                         (setf (oc-border-width x) 0
                               (oc-border-style x) :none
                               (oc-border-color x) :none)
                         (push x dangling-ocs)))))
              ;;
              (setf (pc-dangling-ocs pc) dangling-ocs) )
      

            ;; kludge for the containing block element's text-decoration
            (unless first-line-was-here
              (draw-text-decoration xx1 (+ yy dy) xx
                                    (css:style-attr (pc-block-pt pc) 'css:@text-decoration)
                                    (css:style-attr (pc-block-pt pc) 'css:@color)))
            ))

        ;;
        (incf (rc-y rc) line-height)
        (incf (rc-y rc) line-depth)
        ;;
        (values
         first-line-was-here))) ))

(defun spill-line (rc pc &aux first-line-was-here)
   ;;#+CMU (mp:process-yield)
   ;;(flush-vertical-margin rc)            ;xxx
   ;; hier wird es etwas complicierter -- wir müßen die
   ;; ausgeflachte Structur wieder in einen Baum
   ;; umwandeln.
  
   (cond ((and (> (pc-eline pc) 0)
               ;; das also ist der Grund für dangling-ocs!
               ;; This condition below is still beyond me ...
               (let ((data (pc-data pc)))
                 (declare (type (simple-array t (*)) data))
                 (loop for i fixnum from 0 below (pc-eline pc) do
                       (let ((x (svref data i)))
                         (unless (and (oc-p x)
                                      (eq (oc-pt x) css::*first-line-element*))
                           (return t)))
                       finally nil)) )
          (clim:with-new-output-record (clim-user::*medium*)
           (setf first-line-was-here (spill-line-aux rc pc))))
         (t
          ))
   ;; reflect new margins
   (find-margins rc pc)
   ;; 
   (when (pc-dangling-fboxen pc)
     (dolist (k (prog1 (reverse (pc-dangling-fboxen pc)) (setf (pc-dangling-fboxen pc) nil)))
       ;; big XXX -- add-fbox may put this on `(pc-dangling-fboxen pc)' again.
       (add-fbox rc pc k t)) )

   (setf (pc-line-width pc) 0
         (pc-word-width pc) 0
         (pc-spc-width pc)  0
         (pc-eline pc)      0
         (pc-eline pc)      0
         (pc-eline pc)      0
         (pc-eline pc)      0
         (pc-eline pc)      0
         (pc-eline pc)      0
         (pc-eline pc)      0
         (pc-eword pc)      0
         (pc-espc  pc)      0
         (pc-last-was-space-p pc) t)

   ;; Style stack leer räumen
   (setf (pc-style-stack pc) (last (pc-style-stack pc) 1))
   (setf (pc-ts-stack pc) (last (pc-ts-stack pc) 1))

   ;; Und wieder befüllen
   (let ((q nil))
     (dolist (k (pc-dangling-ocs pc))
       (add-oc rc pc (oc-pt k) (lambda (oc) (push oc q))))
     (setf (pc-dangling-ocs pc) (reverse q)))
   (setf (pc-last-was-word-space-p pc) t
         (pc-last-was-space-p pc) t)

   (assert (= (length (pc-style-stack pc))
              (1+ (length (pc-dangling-ocs pc)))))
  
   (let ((foo (subseq (pc-current-word pc) 0 (length (pc-current-word pc)))))
     (setf (fill-pointer (pc-current-word pc)) 0)
     (let ((*defunt-level* (+ 1 *defunt-level*)))
       (loop for x across foo do
             (cond ((integerp x)
                    (add-rune* rc pc x :normal)) ;xxx
                   ((oc-p x)
                    ;; still not correct wrt to replaced elements,
                    ;; display attribute and stuff.
                    (let ((*replaced-object* (oc-ro x))) ;kludge
                      (walk/add-oc rc pc (oc-pt x))))
                   ((ro-chunk-p x)
                    (walk/add-ro rc pc (ro-chunk-ro x)) )
                   ((cc-p x)
                    (walk/add-cc rc pc (cc-pt x)))
                   (t
                    (error "oops")))))
     )

   ;; (assert (word-empty-p pc)) warum war das drin?
   ;; xxx -- floating boxen!!
   ;; we need to render all floating boxen off (pc-word-fbs pc), which still fit.
   (setf (pc-word-fbs pc) (reverse (pc-word-fbs pc)))
   (dolist (fd (pc-word-fbs pc))
     (add-fbox rc pc fd nil))
   (setf (pc-word-fbs pc) nil)

   ;;(format T "~&***** SPILL-OUT:~%")
   ;;(debug/show-current-line pc)

   )

(defun clim-draw-runes (medium x0 y0 runes start end text-style white-space)
  (let ((font nil)
        (bptr 0)
        bx0 by0 bw)
    (prog1
        (iterate-over-runes
         (lambda (rune index x cw)
           index
           (let ((fid (css-font-desc-glyph-fid (text-style-font text-style) rune))
                 (i   (css-font-desc-glyph-index (text-style-font text-style) rune)))
             (unless (eql font fid)
               ;; we have to spill
               (unless (= bptr 0)
                 (clim:draw-text* medium (map 'string #'code-char (subseq *buffer* 0 bptr))
                                  bx0 by0
                                  :text-style font))
               (setf bptr 0
                     bx0 (round (+ x0 x))
                     by0 (round y0)
                     bw 0)
               (setf font fid))
             (setf (aref *buffer* bptr) i
                   bptr (+ bptr 1)
                   bw (+ bw (round cw)))))
         runes start end text-style :pre) ;; white-space)
      (unless (= bptr 0)
        (clim:draw-text* medium (map 'string #'code-char (subseq *buffer* 0 bptr))
                         bx0 by0
                         :text-style font))) ))

;;;;
;;;; vertical align
;;;;


;;; ---- Vertical Geometry of Lines -------------------------------------------

(defun walk-line (rc pc continuation)
  (dolist (k (pc-dangling-ocs pc))
    (funcall continuation k 0))
  (let ((i 0))
    (declare (type fixnum i))
    (do ()
        ((= i (pc-eline pc)))
      (let ((x (svref (the (simple-array t (*)) (pc-data pc)) (the fixnum i))))
        (cond ((integerp x)
               (funcall continuation
                        x (svref (the (simple-array t (*)) (pc-data pc)) (the fixnum (1+ i))))
               (incf i))
              (t
               (funcall continuation
                        x (svref (the (simple-array t (*)) (pc-data pc)) (the fixnum (1+ i))))
               (incf i)))))))

(defun oc-vertical-align (oc)
  (computed-style-vertical-align 
   (style-computed (car (oc-style-stack oc)))))

(defun oc-line-height (oc)
  (computed-style-line-height
   (style-computed
    (car (oc-style-stack oc)))))

(defun text-style-ascent (text-style)
  (font-desc-ascent (text-style-font text-style)))

(defun text-style-descent (text-style)
  (font-desc-descent (text-style-font text-style)))

(defun oc-text-height-and-depth (oc)
  (cond ((oc-ro oc)
         ;; hmm... css1 does not consider replaced objects to have a baseline
         (values (+ (nth-value 1 (ro/size (oc-ro oc)))
                    (oc-padding-top oc)
                    (oc-border-top-width oc)
                    (oc-margin-top oc)
                    (oc-padding-bottom oc)
                    (oc-border-bottom-width oc)
                    (oc-margin-bottom oc))
                 (+ (nth-value 2 (ro/size (oc-ro oc)))
                    )))
        (t
         (let* ((ts (text-style-font (oc-ts oc)))
                (as (font-desc-ascent ts))
                (ds (font-desc-descent ts))
                (lh;;(oc-line-height oc)
                 (pt-effective-line-height (oc-pt oc)))
                (hl-1 (floor   (- lh (+ as ds)) 2))) ;DEVRND
    
           (let* ((ds2 (max 0 (+ ds hl-1)))
                  (as2 (- lh ds2)))
             (assert (= lh (+ as2 ds2)))
             ;; wir müssen hier leider negative werte vermeiden, deswegen
             ;; dieser hack mit 'max' oben.
             (values as2 ds2) )))))

;;; Vertical Align

;; There is certain confusion about what the geometry on an inline box
;; is. Here is my interpretation:

;; The specification says that the height of an inline box is given by
;; its line-height property. Since the height is the inside height, we
;; need to add the padding, border, and margin to get to the top of an
;; inline box.

;; Note however that the top of an line-box is not influenced by
;; possible margins to paddings.


;; 'baseline'    ⇒ dy ← 0
;; 'middle'      ⇒ dy ← H/2 - D/2 - parent.x-height/2
;; 'sub'         ⇒ dy ← ...
;; 'super'       ⇒ dy ← ...
;; 'text-top'    ⇒ dy ← H - parent.ascent
;; 'text-bottom' ⇒ dy ← - (D - parent.descent)

;; While:
;; non-replaced elements:
;;   H ← [line-height - ascent - descent]/2 + ascent
;;   D ← [line-height - ascent - descent]/2 + descent
;;
;; replaced elements:
;;   H ← ro.height + padding-top + border-top-width + margin-top
;;   D ← ro.depth + padding-bottom + border-bottom-width + margin-bottom

;; CAUTION
;;   CSS1 makes this glory exception that some replaced objects are
;;   not considered to have a baseline, in which case one should calculate:
;;     H ← ro.height + padding-top + border-top-width + margin-top +
;;          ro.depth + padding-bottom + border-bottom-width + margin-bottom 
;;     D ← 0

;; XXX: sometimes dy is absolute, sometimes it is relative! Fix that!

(defun vertically-align-line-2 (rc pc chunks end
                                &aux text-seen-p
                                no-line-height-mode-p)
  (declare (type fixnum end))
  (declare (type (simple-array t (*)) chunks)
           (type rc rc)
           (type pc pc))

  (let ((*tops* nil)
        (*btms* nil))
    
    ;; This still fails because the line-height of the surrounding block
    ;; is not taken into account.

    ;; Special mode for lines, which contain no text at all, that are
    ;; often images in tables, which should align without any gaps between them.
    (setf no-line-height-mode-p
          (not (find-if #'integerp chunks :end end)))
    ;;

    (multiple-value-bind (total-height total-depth)
        ;; xxx hack
        (if no-line-height-mode-p
            (values 0 0)
            (oc-text-height-and-depth (make-oc :ts (car (last (pc-ts-stack pc)))
                                               :pt (pc-block-pt pc)
                                               :style-stack (list (car (last (pc-style-stack pc)))))))
      (let ((oc-stack nil))
        (labels ((klose (oc)
                   (let ((dy
                          (- (new-resolve-valign oc (car oc-stack)))))
                     ;; xxx top/bottom!
                     (setf (oc-dy oc) dy)
                     (cond (oc-stack
                            (setf (oc-height (car oc-stack))
                                  (max 0 (oc-height (car oc-stack))(- (oc-height oc) dy)))
                            (setf (oc-depth  (car oc-stack))
                                  (max 0 (oc-depth (car oc-stack))(+ (oc-depth  oc) dy))))
                           (t
                            (setf total-height (max 0 total-height (- (oc-height oc) dy)))
                            (setf total-depth (max 0 total-depth (+ (oc-depth oc) dy))))))))

          ;; kludge: Add a chunk for the line-box itself
          (let ((item))
            (add-oc rc pc (pc-block-pt pc)
                    (lambda (oc) (setf item oc)))
            (setf (values (oc-height item) (oc-depth item))
                  (if (and no-line-height-mode-p (null (oc-ro item)))
                      (values 0 0)
                      (oc-text-height-and-depth item)))
            (push item oc-stack))
          
          (do ((i 0 (+ i 1)))
              ((= i end))
            (declare (type fixnum i))
            (let ((item (aref chunks i)))
              (cond ((typep item 'fixnum)
                     (incf i 1);; xxx
                     )
                    (t
                     (typecase item
                       (oc
                        (setf (values (oc-height item) (oc-depth item))
                              (if (and no-line-height-mode-p (null (oc-ro item)))
                                  (values 0 0)
                                  (oc-text-height-and-depth item)))
                        (push item oc-stack))
                       (ro-chunk
                        ;; NOTE: these are now handled at the corresponding OC.
                        #||             ;
                        ;; Now ro-chunks are somewhat special, since the padding
                        ;; and border of its containing element should influence
                        ;; the line height (indirectly by affecting the
                        ;; height/depth of the container).
                        ;;
                        ;; Unfortunately we currently have no access to these
                        ;; properties.
                        ;;
                        (multiple-value-bind (w h d) (ro/size (ro-chunk-ro item))
                        (setf (oc-height (car oc-stack))
                        (max 0 (+ (oc-height (car oc-stack))
                        h)))
                        (setf (oc-depth (car oc-stack))
                        (max 0 (oc-depth (car oc-stack)) d)))
                        ||#
                        )
                       (cc
                        (let ((oc (pop oc-stack)))
                          (unless oc
                            (error "More close chunks as open chunks, please debug."))
                          (when oc      ;xxx shouln't happen
                            (klose oc))))
                       )))))
          (do () ((null oc-stack)) (klose (pop oc-stack)))
          ;;
          ;; now care for top/bottom
          ;;
          (dotimes (i 2)
            (when *tops*
              (dolist (k *tops*)
                (setf (oc-dy k) (- (oc-height k) total-height))
                (setf total-height (max total-height (- (oc-height k) (oc-dy k))))
                (setf total-depth  (max total-depth (+ (oc-depth k) (oc-dy k))))
                ))
            (when *btms*
              (dolist (k *btms*)
                (setf (oc-dy k) (- total-depth (oc-depth k)))
                (setf total-height (max total-height (- (oc-height k) (oc-dy k))))
                (setf total-depth  (max total-depth (+ (oc-depth k) (oc-dy k))))
                )))
          ;;
          (values total-height total-depth) )))))

(defun dprint (fmt &rest args)
  (fresh-line *trace-output*)
  (apply #'format *trace-output* fmt args)
  (fresh-line *trace-output*)
  (finish-output *trace-output*))

(defun new-resolve-valign (oc parent-oc)
  (let ((valign (oc-vertical-align oc)))
    (cond ((realp valign)
           valign)
          (t
           (ecase valign
             (:BASELINE
              0)
             (:MIDDLE      
              ;; align the vertical midpoint of the element (typically an image) with
              ;; the baseline plus half the x-height of the parent 
              (if (null parent-oc)
                  (progn
                    (warn "Cannot align middle without a parent element.")
                    0)
                (/ (+ (- (oc-depth oc) (oc-height oc)) 
                      (+ (font-desc-x-height (text-style-font (oc-ts parent-oc)))) )
                   2)))
             (:IMG-MIDDLE
              (floor (+ (- (oc-depth oc) (oc-height oc))) 2))
             (:SUB
              (/ (- (font-desc-x-height (text-style-font (oc-ts oc)))) 2))
             (:SUPER
              (/ (+ (font-desc-x-height (text-style-font (oc-ts oc)))) 1))
             (:TEXT-TOP
              ;; align the top of the element with the top of the parent element's font 
              (if (null parent-oc)
                  (progn
                    (warn "Cannot align 'text-top' without parent.")
                    0)
                (- (font-desc-ascent (text-style-font (oc-ts parent-oc)))
                   (oc-height oc))))
             (:TEXT-BOTTOM
              ;; align the bottom of the element with the bottom of the parent element's font
              (if (null parent-oc)
                  (progn
                    (warn "Cannot align 'text-bottom' without parent.")
                    0)
                (- (oc-depth oc)
                   (font-desc-descent (text-style-font (oc-ts parent-oc))))))
             (:TOP
              (push oc *tops*)
              :top
              0)
             (:BOTTOM
              (push oc *btms*)
              :bottom
              0) )))))

;;;;
;;;; Marker Boxen
;;;;

;; Marker boxen behave like line boxen, only that line-height
;; calculation also takes other lines into account.
;; (This is a similiar situation to table-cells).
;;

;; $Log: renderer.lisp,v $
;; Revision 1.7  2002-07-30 13:40:28  gilbert
;; some more work on vertical align
;;
