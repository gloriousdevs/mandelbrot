#|

Para cada ponto c = (x, y) do gráfico:
- fc(z) = z² + c, começando com z = 0
  iterar N vezes até abs(fc(z)) > 2
  colorir pixel de acordo com valor de N
  até limite N = 42

|#

(defparameter *absolute-threshold* 2)
(defparameter *iteration-limit* 42)

(progn
  (defparameter *width* 5000)
  (defparameter *height* 5000)

  (defparameter *min-x* -2.0)
  (defparameter *max-x* +2.0)
  (defparameter *min-y* -2.0)
  (defparameter *max-y* +2.0)

  (defparameter *x-step* (/ (- *max-x* *min-x*) *width*))
  (defparameter *y-step* (/ (- *max-y* *min-y*) *height*)))


(declaim (inline iterations))

(defun iterations (c)
  (labels ((iterate (n z)
             (let ((val (+ (expt z 2) c)))
               ;;(format t "val = ~a~%" val)
               (cond ((or (> (abs val) *absolute-threshold*)
                          (>= n *iteration-limit*))
                      n)
                     (t
                      (iterate (+ n 1) val))))))
    (declare (ftype (function (fixnum (complex single-float)) fixnum) iterate))
    (iterate 0 #C(0.0 0.0))))


(defun get-coordinate (line column)
  (let ((x (+ *min-x* (* *x-step* column)))
        (y (- *max-y* (* *y-step* line))))
    (complex x y)))

(defun print-grid-to-terminal ()
  (loop for line from 0 to *height* do
    (loop for column from 0 to *width* do
      (let ((iters (iterations (get-coordinate line column))))
        (cond ((>= iters 42) (princ #\*))
              (t             (princ #\space)))))
    (terpri)))

(defun generate-ppm ()
  (with-open-file (output "mandelbrot.ppm" :direction :output :if-exists :supersede)
    (format output "P3~%~a ~a~%255~%" *width* *height*) ;; header
    (loop for line from 0 below *height* do
      (when (= (mod line 500) 0) (format t "Processing line ~a~%" line))
      (loop for column from 0 below *width* do
        (let ((iters (iterations (get-coordinate line column))))
          (cond ((>= iters 42) (format output "0 0 0 "))
                (t             (format output "255 255 255 ")))))
      (terpri output)))
  (format t "Done!~%"))


(defun generate-ppm-loucura ()
  (let ((x-coords (coerce (loop for column from 0 below *width*
                                collect (+ *min-x* (* *x-step* column)))
                          '(vector single-float *)))
        (y-coords (coerce (loop for line from 0 below *height*
                                collect (- *max-y* (* *y-step* line)))
                          '(vector single-float *))))
    (with-open-file (output "mandelbrot.ppm" :direction :output :if-exists :supersede)
      (format output "P3~%~a ~a~%255~%" *width* *height*) ;; header
      (loop for y across y-coords do
        (loop for x across x-coords do
          (let ((iters (iterations (complex x y))))
            (cond ((>= iters 42) (format output "0 0 0 "))
                  (t             (format output "255 255 255 ")))))
        (terpri output)))
    (format t "Done!~%")))


;;(generate-ppm)


;; (defmacro nlet (name params &body body)
;;   `(labels ((,name ,(mapcar #'first params)
;;               ,@body))
;;      (,name ,@(mapcar #'second params))))


;; (defun iterations (c)
;;   (nlet iterate ((n 0) (z 0))
;;     (let ((val (+ (expt z 2) c)))
;;       (cond ((or (> (abs val) *threshold*)
;;                  (>= n *iteration-limit*))
;;              n)
;;             (t
;;              (iterate (+ n 1) val))))))
