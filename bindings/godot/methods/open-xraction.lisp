(common-lisp:in-package :%godot)


(defgmethod
 (open-xraction+set-localized-name :class 'open-xraction :bind
  "set_localized_name" :hash 83702148)
 :void (localized-name string))

(defgmethod
 (open-xraction+get-localized-name :class 'open-xraction :bind
  "get_localized_name" :hash 201670096)
 string)

(defgmethod
 (open-xraction+set-action-type :class 'open-xraction :bind "set_action_type"
  :hash 1675238366)
 :void (action-type open-xraction+action-type))

(defgmethod
 (open-xraction+get-action-type :class 'open-xraction :bind "get_action_type"
  :hash 3536542431)
 open-xraction+action-type)

(defgmethod
 (open-xraction+set-toplevel-paths :class 'open-xraction :bind
  "set_toplevel_paths" :hash 4015028928)
 :void (toplevel-paths packed-string-array))

(defgmethod
 (open-xraction+get-toplevel-paths :class 'open-xraction :bind
  "get_toplevel_paths" :hash 1139954409)
 packed-string-array)