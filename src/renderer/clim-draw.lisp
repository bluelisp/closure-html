;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RENDERER; Encoding: utf-8; Readtable: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Drawing
;;;   Created: 2003-03-08
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;;       $Id: clim-draw.lisp,v 1.1 2003-03-13 19:30:56 gilbert Exp $
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-2003 by Gilbert Baumann

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
 
(in-package :renderer)

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

(defun clim-draw-border (medium ix1 iy1 ix2 iy2
                         border-top-width border-top-style border-top-color
                         border-right-width border-right-style border-right-color
                         border-bottom-width border-bottom-style border-bottom-color
                         border-left-width border-left-style border-left-color)
  (let* ((x1 (- ix1 border-left-width))
         (y1 (- iy1 border-top-width))
         (x2 (+ ix2 border-right-width))
         (y2 (+ iy2 border-bottom-width))
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

;;;; Text Decoration

;; Q: what is the precise meaning of "If the element has no content or no text
;;    content (e.g., the IMG element in HTML), user agents must ignore this
;;    property."

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
          xx1 (- yy 6) xx (- yy 6) :ink (clim-user::parse-x11-color color))) ))))

;;;; Runes

;; Note: This glorious ITERATE-OVER-RUNES is only used by
;; CLIM-DRAW-RUNES, so we should dismantle it.

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

(defmacro iterate-over-runes (fun runes start end text-style white-space)
  (assert (eq (car fun) 'lambda))
  `((lambda (runes start end text-style white-space)
      (cond ((and (eq (text-style-letter-spacing text-style) 0)
                  (eq (text-style-word-spacing text-style) :normal))
             (ecase white-space
               ((:pre)
                (iterate-over-runes/pre* ,fun runes start end text-style)) ))
            (t
             (ecase white-space
               ((:pre)
                (iterate-over-runes/pre/generic ,fun runes start end text-style)) )) ))
    ,runes ,start ,end ,text-style ,white-space))

;;; Aehem, this buffer business must get better ...

(clim-sys:defresource draw-text-buffer ()
  :constructor (make-string 1000))

(defun clim-draw-runes* (medium x0 y0 runes start end text-style)
  (let ((font nil)
        (bptr 0)
        bx0 by0 bw)
    (clim-sys:using-resource (buffer draw-text-buffer)
      (let ((buffer-size (length buffer)))
        (prog1
            (iterate-over-runes
             (lambda (rune index x cw)
               index
               (let ((fid (css-font-desc-glyph-fid (text-style-font text-style) rune))
                     (i   (css-font-desc-glyph-index (text-style-font text-style) rune)))
                 (when (or (not (eq font fid))
                           (= bptr buffer-size))
                   ;; we have to spill
                   (unless (= bptr 0)
                     (clim:draw-text* medium (subseq buffer 0 bptr)
                                      bx0 by0
                                      :text-style font))
                   (setf bptr 0
                         bx0 (round (+ x0 x))
                         by0 (round y0)
                         bw 0)
                   (setf font fid))
                 (setf (aref buffer bptr) (code-char i)
                       bptr (+ bptr 1)
                       bw (+ bw (round cw)))))
             runes start end text-style :pre)
          (unless (= bptr 0)
            (clim:draw-text* medium (subseq buffer 0 bptr)
                             bx0 by0
                             :text-style font))) ))))

#+NIL
(climi::def-grecording draw-runes (medium x0 y0 runes start end text-style)
        (values x0
                (- y0 10)
                (+ x0 100)
                (+ y 10)))

