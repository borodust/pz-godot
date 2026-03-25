(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-modification-2dstack-holder+set-held-modification-stack :class
  'skeleton-modification-2dstack-holder :bind "set_held_modification_stack"
  :hash 3907307132)
 :void (held-modification-stack skeleton-modification-stack-2d))

(defgmethod
 (skeleton-modification-2dstack-holder+get-held-modification-stack :class
  'skeleton-modification-2dstack-holder :bind "get_held_modification_stack"
  :hash 2107508396)
 skeleton-modification-stack-2d)