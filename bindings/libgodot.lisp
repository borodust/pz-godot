(cl:defpackage :%libgodot
  (:use)
  (:export #:create-godot-instance
           #:destroy-godot-instance))
(cl:in-package :%libgodot)


(cffi:defcfun ("libgodot_create_godot_instance" create-godot-instance)
    %gdext.types:object-ptr
  "
 * @name libgodot_create_godot_instance
 * @since 4.6
 *
 * Creates a new Godot instance.
 *
 * @param p_argc The number of command line arguments.
 * @param p_argv The C-style array of command line arguments.
 * @param p_init_func GDExtension initialization function of the host application.
 *
 * @return A pointer to created \ref GodotInstance GDExtension object or nullptr if there was an error.
"
  (argc :int)
  (argv :pointer)
  (init-func %gdext.types:initialization-function))


(cffi:defcfun ("libgodot_destroy_godot_instance" destroy-godot-instance)
    :void
"
 * @name libgodot_destroy_godot_instance
 * @since 4.6
 *
 * Destroys an existing Godot instance.
 *
 * @param p_godot_instance The reference to the GodotInstance object to destroy."
  (godot-instance %gdext.types:object-ptr))
