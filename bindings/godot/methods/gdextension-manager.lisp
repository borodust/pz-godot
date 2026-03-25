(common-lisp:in-package :%godot)


(defgmethod
 (gdextension-manager+load-extension :class 'gdextension-manager :bind
  "load_extension" :hash 4024158731)
 gdextension-manager+load-status (path string))

(defgmethod
 (gdextension-manager+load-extension-from-function :class 'gdextension-manager
  :bind "load_extension_from_function" :hash 1565094761)
 gdextension-manager+load-status (path string)
 (init-func (:pointer %gdext:initialization-function)))

(defgmethod
 (gdextension-manager+reload-extension :class 'gdextension-manager :bind
  "reload_extension" :hash 4024158731)
 gdextension-manager+load-status (path string))

(defgmethod
 (gdextension-manager+unload-extension :class 'gdextension-manager :bind
  "unload_extension" :hash 4024158731)
 gdextension-manager+load-status (path string))

(defgmethod
 (gdextension-manager+is-extension-loaded :class 'gdextension-manager :bind
  "is_extension_loaded" :hash 3927539163)
 bool (path string))

(defgmethod
 (gdextension-manager+get-loaded-extensions :class 'gdextension-manager :bind
  "get_loaded_extensions" :hash 1139954409)
 packed-string-array)

(defgmethod
 (gdextension-manager+get-extension :class 'gdextension-manager :bind
  "get_extension" :hash 49743343)
 || (path string))