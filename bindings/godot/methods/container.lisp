(common-lisp:in-package :%godot)


(defgmethod
 (container+%get-allowed-size-flags-horizontal :class 'container :bind
  "_get_allowed_size_flags_horizontal" :hash 1930428628 :virtual common-lisp:t)
 packed-int-32array)

(defgmethod
 (container+%get-allowed-size-flags-vertical :class 'container :bind
  "_get_allowed_size_flags_vertical" :hash 1930428628 :virtual common-lisp:t)
 packed-int-32array)

(defgmethod
 (container+queue-sort :class 'container :bind "queue_sort" :hash 3218959716)
 :void)

(defgmethod
 (container+fit-child-in-rect :class 'container :bind "fit_child_in_rect" :hash
  1993438598)
 :void (child control) (rect rect-2))

(defgmethod
 (container+set-accessibility-region :class 'container :bind
  "set_accessibility_region" :hash 2586408642)
 :void (region bool))

(defgmethod
 (container+is-accessibility-region :class 'container :bind
  "is_accessibility_region" :hash 36873697)
 bool)