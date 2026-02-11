(cl:defpackage :godot.example
  (:use :cl)
  (:export #:run))
(cl:in-package :godot.example)


(cffi:define-foreign-library (godot
                              :search-path (asdf:system-relative-pathname :pz-godot/wrapper "src/lib/build/desktop/library/"))
  (:linux "libgodot.so"))


(defun init-godot ()
  (cffi:with-foreign-object (godot-version '%gdext.types:godot-version2)
    (%gdext.interface:get-godot-version2 godot-version)
    (cffi:with-foreign-slots (((major %gdext.types::major)
                               (minor %gdext.types::minor)
                               (patch %gdext.types::patch))
                              godot-version %gdext.types:godot-version2)
      (format *standard-output* "~&Godot version: ~A.~A.~A"
              major minor patch))))


(%gdext.common:defprotocallback (libgodot-init
                                 %gdext.types:initialization-function)
    (get-proc-addr-ptr class-lib-ptr init-record-ptr)
  (%gdext.common:initialize-interface get-proc-addr-ptr)
  (init-godot)
  1)

(defun handle-instance (godot-instance)
  (format *standard-output* "~&Yay! We have an instance: ~A" godot-instance))


(defun run-with-godot-instance ()
  (let ((exec-path (namestring
                    (asdf:system-relative-pathname :pz-godot/example "."))))
    (cffi:with-foreign-string (exec-path-ptr exec-path)
      (cffi:with-foreign-object (argv :pointer)
        (setf (cffi:mem-ref argv :pointer) exec-path-ptr)
        (let ((instance (%libgodot:create-godot-instance 1 argv (cffi:callback libgodot-init))))
          (if (cffi:null-pointer-p instance)
              (error "Failed to create Godot instance")
              (unwind-protect
                   (handle-instance instance)
                (%libgodot:destroy-godot-instance instance))))))))


(defun run ()
  (cffi:load-foreign-library 'godot)
  (unwind-protect
       (float-features:with-float-traps-masked t
         (run-with-godot-instance))
    (cffi:close-foreign-library 'godot)))
