;;;
;;; Copyright (c) 2007 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package #:zpng)

(defclass png ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (color-type :initform :truecolor :initarg :color-type :reader color-type)
   (bpp :initform 8 :initarg :bpp :reader bpp)
   (image-data :initarg :image-data :reader image-data
               :writer (setf %image-data))
   (data-array :reader data-array
               :writer (setf %data-array))))


(defgeneric ihdr-color-type (png))
(defgeneric samples-per-pixel (png))
(defgeneric scanline-offset (png scanline))
(defgeneric rowstride (png))

(defgeneric write-png-header (png stream))
(defgeneric write-ihdr (png stream))
(defgeneric write-idat (png stream))
(defgeneric write-iend (png stream))

(defgeneric write-png-stream (png stream))
(defgeneric write-png (png pathname &key if-exists))

(defmethod slot-unbound (class (png png) (slot (eql 'data-array)))
  (let ((array (make-array (list (height png)
                                 (width png)
                                 (samples-per-pixel png))
                           :displaced-to (image-data png)
                           :element-type '(unsigned-byte 8))))
    (setf (%data-array png) array)))

(defmethod initialize-instance :after ((png png) &rest args &key image-data)
  (declare (ignore args))
  (unless image-data
    (setf (%image-data png)
          (make-array (* (height png) (rowstride png))
                      :initial-element 0
                      :element-type '(unsigned-byte 8)))))

(defmethod ihdr-color-type (png)
  (cdr (assoc (color-type png) *color-types*)))

(defmethod samples-per-pixel (png)
  (ecase (color-type png)
    (:grayscale 1)
    (:truecolor 3)
    (:indexed-color 1)
    (:grayscale-alpha 2)
    (:truecolor-alpha 4)))

(defmethod rowstride (png)
  (* (width png) (samples-per-pixel png)))

(defmethod scanline-offset (png scanline)
  (* scanline (rowstride png)))




(defmethod write-png-header (png stream)
  (write-sequence *png-signature* stream))

(defmethod write-ihdr (png stream)
  (let ((chunk (make-chunk 73 72 68 82 13)))
    (chunk-write-uint32 (width png) chunk)
    (chunk-write-uint32 (height png) chunk)
    (chunk-write-byte (bpp png) chunk)
    (chunk-write-byte (ihdr-color-type png) chunk)
    (chunk-write-byte +png-compression-method+ chunk)
    (chunk-write-byte +png-filtering+ chunk)
    (chunk-write-byte +png-interlace+ chunk)
    (write-chunk chunk stream)))

(defmethod write-idat (png stream)
  (let* ((chunk (make-chunk 73 68 65 84 16384))
         (buffer (buffer chunk))
         (callback (lambda (data end)
                     (replace buffer data :start1 4 :end2 end)
                     (setf (pos chunk) (+ end 4))
                     (write-chunk chunk stream))))
    (with-compressor (compressor 'zlib-compressor
                                 :callback callback)
      (dotimes (i (height png))
        (let* ((start-offset (scanline-offset png i))
               (end-offset (+ start-offset (rowstride png))))
          (compress-octet 0 compressor)
          (compress-octet-vector (image-data png)
                                 compressor
                                 :start start-offset
                                 :end end-offset))))))


(defmethod write-iend (png stream)
  (let ((chunk (make-chunk 73 69 78 68 0)))
    (write-chunk chunk stream)))


(defmethod write-png-stream (png stream)
  (write-png-header png stream)
  (write-ihdr png stream)
  (write-idat png stream)
  (write-iend png stream))
  
(defmethod write-png (png file &key (if-exists :supersede))
  (with-open-file (stream file
                   :direction :output
                   :if-exists if-exists
                   :if-does-not-exist :create
                   :element-type '(unsigned-byte 8))
    (write-png-stream png stream)
    (truename file)))
