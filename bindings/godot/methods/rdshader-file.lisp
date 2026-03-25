(common-lisp:in-package :%godot)


(defgmethod
 (rdshader-file+set-bytecode :class 'rdshader-file :bind "set_bytecode" :hash
  1526857008)
 :void (bytecode rdshader-spirv) (version string-name))

(defgmethod
 (rdshader-file+get-spirv :class 'rdshader-file :bind "get_spirv" :hash
  2689310080)
 rdshader-spirv (version string-name))

(defgmethod
 (rdshader-file+get-version-list :class 'rdshader-file :bind "get_version_list"
  :hash 3995934104)
 array)

(defgmethod
 (rdshader-file+set-base-error :class 'rdshader-file :bind "set_base_error"
  :hash 83702148)
 :void (error string))

(defgmethod
 (rdshader-file+get-base-error :class 'rdshader-file :bind "get_base_error"
  :hash 201670096)
 string)