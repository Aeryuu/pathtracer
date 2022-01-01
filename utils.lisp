(in-package #:pathtracer)

(defun dot (x y)
  "Given two vectors, returns x dot y."
  (+  (* (svref  x 0) (svref  y 0))
      (* (svref x 1) (svref y 1))
      (* (svref x 2) (svref y 2))))

(defun cross (x y)
  "Given two vectors, returns x cross y."
  (let ((x1 (svref x 0)) (x2 (svref x 1)) (x3 (svref x 2))
        (y1 (svref y 0)) (y2 (svref y 1)) (y3 (svref y 2)))
    (vector  (-  (* x2 y3) (* x3 y2))
     (- (* x3 y1) (* x1 y3))
     (-  (* x1 y2) (* x2 y1))
     1)))

(defun magnitude (vect)
  "Calculates the magnitude of the given vector."
  (let ((v1 (svref vect 0)) (v2 (svref vect 1)) (v3 (svref vect 2)))
    (sqrt (+ (* v1 v1) (* v2 v2) (* v3 v3)))))

(defun normalize  (vect)
  "Normalizes the given vector."
  (let ((v1 (svref vect 0)) (v2 (svref vect 1)) (v3 (svref vect 2)) (m (magnitude vect)))
    (vector (/ v1 m) (/ v2 m) (/ v3 m))))

(defmacro vectOp (x y f)
  `(vector (,f (svref ,x 0) (svref ,y 0))
           (,f (svref ,x 1) (svref ,y 1))
           (,f (svref ,x 2) (svref ,y 2))
           1))

(defun addVect (x y)
  "Given two vectors, return x + y"
  (vectOp x y +))

(defun subVect (x y)
  "Given two vectors, return x - y."
  (vectOp x y -))
