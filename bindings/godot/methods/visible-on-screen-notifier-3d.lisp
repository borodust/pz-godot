(common-lisp:in-package :%godot)


(defgmethod
 (visible-on-screen-notifier-3d+set-aabb :class 'visible-on-screen-notifier-3d
  :bind "set_aabb" :hash 259215842)
 :void (rect aabb))

(defgmethod
 (visible-on-screen-notifier-3d+is-on-screen :class
  'visible-on-screen-notifier-3d :bind "is_on_screen" :hash 36873697)
 bool)