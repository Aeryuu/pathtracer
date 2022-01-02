;;;; pathtracer.lisp

(in-package #:pathtracer)

(ql:quickload :3d-matrices)
(use-package :3d-matrices)

;;  input: ray to trace
;; output: colour, typically as (R, G, B) (required), other stuff as needed (not even sure if needed)

(defclass ray ()
    ((point :initarg :point :accessor :point)
     (ic :initarg :ic :accessor :ic :initform #(0 0 0))
     (direction :initarg :direction :accessor :direction)
     (colour :initarg :colour :accessor :colour :initform #(1 1 1))))

(defun find-first-hit ()
  (values '() #(2 3) 1 2))

(defun pathtracer (object ray depth)
  (if (> depth max-depth) (ic ray))
  ;;   if hitObj is null: return col = r.IC

  ;; if hitObj is a light source: return col = r.IC + r.C * light_colour/intensity * dot(rayOriginObject's normal, ray direction)

  ;; // else ray hit an object
  ;; mode = rand()
  (multiple-value-bind (hit-object point-of-intersection normal lambda) (find-first-hit)
    (if (null hit-object) (ic ray)))
  ;; // based on BRDF percentages, we pick whether the ray will be reflected as though object is diffuse (opaque), reflective, or refractive
  ;; if mode is diffuse:
  ;;     r.d = random sample a direction (phi, theta) over a hemisphere centered around normal vector at POI
  ;;     r.c = brdf / pdf // which is either objCol * dot(n, d) or objCol

  ;; elif mode is reflect:
  ;;     r.d = perfect reflection angle
  ;;     r.c = brdf / pdf // which is objCol

  ;; elif mode is refract:
  ;;     get % of light that's transmissive and % of light that's reflective
  ;;     mode = rand()

  ;;     // % from 2 lines above determine whether we shall trace a reflective or refractive ray
  ;;     if mode is reflective:
  ;;         r.d = perfect reflection angle
  ;;     if mode is transmissive:
  ;;         r.d = some direction into object based on snells law n stuff
  (let ((mode (random 1.0f))
	(diffuse-percentage ))
    (cond (((< mode (diffuse-percentage object))
	    (set-array-direction ray ))))))


(defclass object ()
  ;;;  types of %: diffuse, reflect, refract
  ((diffuse-percentage
    :initarg :diffuse-percentage
    :accessor diffuse-percentage)
   (reflective-percentage
    :initarg :reflective-percentage
    :accessor reflective-percentage)
   (refractive-percentage
    :initarg :refractive-percentage
    :accessor refractive-percentage)
   (transformation-matrix
    :initarg :transformation-matrix
    :accessor transformation-matrix
    :initform (meyesmh 4))
   (inverse-transformation-matrix
    :initarg :inverse-transformation-matrix
    :accessor inverse-transformation-matrix)))

((defclass sphere (object)
  ((radius
    :initarg :radius
    :initform 1)))
