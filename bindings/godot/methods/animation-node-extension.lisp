(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-extension+%process-animation-node :class
  'animation-node-extension :bind "_process_animation_node" :hash 912931771
  :virtual common-lisp:t)
 packed-float-32array (playback-info packed-float-64array) (test-only bool))

(defgmethod
 (animation-node-extension+is-looping :class 'animation-node-extension :bind
  "is_looping" :hash 2035584311 :static common-lisp:t)
 bool (node-info packed-float-32array))

(defgmethod
 (animation-node-extension+get-remaining-time :class 'animation-node-extension
  :bind "get_remaining_time" :hash 2851904656 :static common-lisp:t)
 float (node-info packed-float-32array) (break-loop bool))