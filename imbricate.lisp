;;;; imbricate.lisp

(defpackage #:imbricate
  (:use #:cl)
  (:export #:imbricate
           #:imbricate-and-save))

(in-package #:imbricate)

(defclass rect ()
  ((x :accessor rect-x
      :initarg :x
      :initform 0
      :type fixnum)
   (y :accessor rect-y
      :initarg :y
      :initform 0
      :type fixnum)
   (width :accessor rect-width
          :initarg :width
          :initform 0
          :type fixnum)
   (height :accessor rect-height
           :initarg :height
           :initform 0
           :type fixnum)))


(defun rect-area (r)
  (* (rect-width r) (rect-height r)))

(defun contains-point-p (r px py)
  (with-slots (x y width height) r
    (and (<= x px (1- (+ x width)))
         (<= y py (1- (+ y height))))))


(defun top-left (rect)
  (with-slots (x y) rect
    (cons x y)))

(defun top-right (rect)
  (with-slots (x y width) rect
    (cons (1- (+ x width))
          y)))

(defun bottom-left (rect)
  (with-slots (x y width height) rect
    (cons (1- (+ x width))
          (1- (+ y height)))))

(defun bottom-right (rect)
  (with-slots (x y height) rect
      (cons x
            (1- (+ y height)))))

(defun translate-pt (pt dx dy)
  (cons (+ dx (car pt))
        (+ dy (cdr pt))))

(defun left-most (rect) (rect-x rect))
(defun right-most (rect) (1- (+ (rect-x rect) (rect-width rect))))
(defun bottom-most (rect) (1- (+ (rect-y rect) (rect-height rect))))
(defun top-most (rect ) (rect-y rect))

(defun corners (rect)
  (list (top-left rect)
        (top-right rect)
        (bottom-right rect)
        (bottom-left rect)))

(defun intersects-p (r1 r2)
  (loop :for (x . y) :in (corners r2)
        :when (contains-point-p r1 x y)
          :do (return-from intersects-p t)))

(defclass tile (rect)
  ((path :accessor tile-path
         :initarg :path
         :initform (error "must supply path"))
   (data :accessor tile-data
         :initarg :data
         :initform (error "Must supply data")))) 

(defmethod print-object ((ob tile) stream)
  (with-slots (x y width height path) ob
    (format stream "#<tile: ~a~%    dimensions: ~ax~a~%    at: ~a,~a>"
            path width height x y)))

(defun tile-meta-info (tile)
  (with-slots (x y width height path) tile
    (list :path path :x x :y y :width width :height height)))

(defclass sheet-plan (rect)
  ((candidates :accessor candidates
               :initform nil)
   (positioned :accessor positioned
               :initform nil)))


(defun validly-positioned-p (plan tile)
  (and (contains-point-p plan (right-most tile) (bottom-most tile))
       (not (some (lambda (other) (intersects-p tile other)) (positioned plan)))))

(defun position-tile (plan tile)
  "finds a place for the tile in the tilesheet under construction and
places the tile into the 'positioned' list of the corner plan
instance. "
  (loop :for (x . y) :in (candidates plan)
        :do (setf (rect-x tile) x
                  (rect-y tile) y)
        :until (validly-positioned-p plan tile))
  ;; if no position was found, set a position based
  ;; on the current size of the tilesheet
  (unless (validly-positioned-p plan tile)
    (with-slots (width height) plan
      (if (< width height)
          (setf (rect-x tile) width
                (rect-y tile) 0)
          (setf (rect-x tile) 0
                (rect-y tile) height))))

  ;; update width and height of the sheet
  (setf (rect-width plan) (max (rect-width plan)
                                 (1+  (right-most tile)))
        (rect-height plan) (max (rect-height plan)
                                  (1+ (bottom-most tile))))

  ;; update the corner plan
  (push tile (positioned plan))       
  (setf (candidates plan)
        (delete (top-left tile)
                (candidates plan)
                :test #'equal))
  (pushnew (translate-pt (top-right tile) 1 0) (candidates plan)
           :test #'equal)
  (pushnew (translate-pt (bottom-left tile) 0 1) (candidates plan)
           :test #'equal))



(defun position-tiles (tiles)
  (let ((plan (make-instance 'sheet-plan))
        (tiles (sort tiles #'> :key #'rect-area)))
    (dolist (tile tiles plan)
      (position-tile plan tile))))



(defun render-sheet (plan)
  (let ((sheet (opticl:make-8-bit-rgba-image (rect-width plan) (rect-height plan))))
    (dolist (tile (positioned plan))
      (with-slots (x y width height data) tile 
        (dotimes (px width)
          (dotimes (py height)
            (setf (opticl:pixel sheet  (+ x px) (+ y py) )
                  (opticl:pixel data  px py ))))))
    sheet))


(defun make-sheet-info (plan)
  (mapcar #'tile-meta-info (positioned plan)))

(defun load-tile (path)
  (let ((data
          (opticl:convert-image-to-rgba
           (opticl:read-png-file path))))

    (opticl:with-image-bounds (w h) data
      (make-instance 'tile
                     :path path
                     :data data
                     :width w
                     :height h))))

(defun png-file-p (path)
  (declare (type pathname path))
  (string-equal "png" (pathname-type path)))

(defvar *bad-images* nil)

(defun images-under-dir (dir)
  (let ((images '()))
    (uiop:collect-sub*directories
     dir
     (constantly t)
     (constantly t)
     (lambda (subdir)
       (dolist (file (uiop:directory-files subdir))
         (when (png-file-p file)
           (handler-case 
               (push (load-tile file) images)
             (error (e)
               (declare (ignore e))
               (push file *bad-images*)))))))
    images))


(defun imbricate (dir)
  (let ((*bad-images* nil)
        (plan (position-tiles
               (images-under-dir dir))))
    (values 

     (render-sheet plan)

     (make-sheet-info plan)

     *bad-images*)))

(defun imbricate-and-save (dir save-to title)
  (multiple-value-bind (sheet info bad) (imbricate dir)
    (let ((png-path (merge-pathnames (format nil "~a.png" title) save-to))
          (info-path (merge-pathnames (format nil "~a.sexp" title) save-to))
          (bad-path (merge-pathnames (format nil "~a.errors.txt" title) save-to)))
      (ensure-directories-exist png-path)
      (with-open-file (out info-path :direction :output :if-exists :supersede)
        (print info out))
      (with-open-file (out bad-path :direction :output :if-exists :supersede)
        (print bad out))
      (opticl:write-png-file png-path sheet))))
