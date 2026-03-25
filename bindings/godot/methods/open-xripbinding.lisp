(common-lisp:in-package :%godot)


(defgmethod
 (open-xripbinding+set-action :class 'open-xripbinding :bind "set_action" :hash
  349361333)
 :void (action open-xraction))

(defgmethod
 (open-xripbinding+get-action :class 'open-xripbinding :bind "get_action" :hash
  4072409085)
 open-xraction)

(defgmethod
 (open-xripbinding+set-binding-path :class 'open-xripbinding :bind
  "set_binding_path" :hash 83702148)
 :void (binding-path string))

(defgmethod
 (open-xripbinding+get-binding-path :class 'open-xripbinding :bind
  "get_binding_path" :hash 201670096)
 string)

(defgmethod
 (open-xripbinding+get-binding-modifier-count :class 'open-xripbinding :bind
  "get_binding_modifier_count" :hash 3905245786)
 int)

(defgmethod
 (open-xripbinding+get-binding-modifier :class 'open-xripbinding :bind
  "get_binding_modifier" :hash 3538296211)
 open-xraction-binding-modifier (index int))

(defgmethod
 (open-xripbinding+set-binding-modifiers :class 'open-xripbinding :bind
  "set_binding_modifiers" :hash 381264803)
 :void (binding-modifiers array))

(defgmethod
 (open-xripbinding+get-binding-modifiers :class 'open-xripbinding :bind
  "get_binding_modifiers" :hash 3995934104)
 array)

(defgmethod
 (open-xripbinding+set-paths :class 'open-xripbinding :bind "set_paths" :hash
  4015028928)
 :void (paths packed-string-array))

(defgmethod
 (open-xripbinding+get-paths :class 'open-xripbinding :bind "get_paths" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (open-xripbinding+get-path-count :class 'open-xripbinding :bind
  "get_path_count" :hash 3905245786)
 int)

(defgmethod
 (open-xripbinding+has-path :class 'open-xripbinding :bind "has_path" :hash
  3927539163)
 bool (path string))

(defgmethod
 (open-xripbinding+add-path :class 'open-xripbinding :bind "add_path" :hash
  83702148)
 :void (path string))

(defgmethod
 (open-xripbinding+remove-path :class 'open-xripbinding :bind "remove_path"
  :hash 83702148)
 :void (path string))