(common-lisp:in-package :%godot)


(defgmethod
 (skin+set-bind-count :class 'skin :bind "set_bind_count" :hash 1286410249)
 :void (bind-count int))

(defgmethod
 (skin+get-bind-count :class 'skin :bind "get_bind_count" :hash 3905245786) int)

(defgmethod (skin+add-bind :class 'skin :bind "add_bind" :hash 3616898986)
 :void (bone int) (pose transform-3d))

(defgmethod
 (skin+add-named-bind :class 'skin :bind "add_named_bind" :hash 3154712474)
 :void (name string) (pose transform-3d))

(defgmethod
 (skin+set-bind-pose :class 'skin :bind "set_bind_pose" :hash 3616898986) :void
 (bind-index int) (pose transform-3d))

(defgmethod
 (skin+get-bind-pose :class 'skin :bind "get_bind_pose" :hash 1965739696)
 transform-3d (bind-index int))

(defgmethod
 (skin+set-bind-name :class 'skin :bind "set_bind_name" :hash 3780747571) :void
 (bind-index int) (name string-name))

(defgmethod
 (skin+get-bind-name :class 'skin :bind "get_bind_name" :hash 659327637)
 string-name (bind-index int))

(defgmethod
 (skin+set-bind-bone :class 'skin :bind "set_bind_bone" :hash 3937882851) :void
 (bind-index int) (bone int))

(defgmethod
 (skin+get-bind-bone :class 'skin :bind "get_bind_bone" :hash 923996154) int
 (bind-index int))

(defgmethod
 (skin+clear-binds :class 'skin :bind "clear_binds" :hash 3218959716) :void)