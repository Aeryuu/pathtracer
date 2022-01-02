;;;; pathtracer.asd

(asdf:defsystem #:pathtracer
  :description "Pathtracer in Common Lisp"
  :version "0.0.1"
  :serial t
  :depends-on ("3d-matrices" "3d-vectors")
  :components ((:file "package")
               (:file "pathtracer")))
