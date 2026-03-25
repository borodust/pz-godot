(common-lisp:in-package :%godot)


(defgmethod
 (foldable-group+get-expanded-container :class 'foldable-group :bind
  "get_expanded_container" :hash 1427441056)
 foldable-container)

(defgmethod
 (foldable-group+get-containers :class 'foldable-group :bind "get_containers"
  :hash 3995934104)
 array)

(defgmethod
 (foldable-group+set-allow-folding-all :class 'foldable-group :bind
  "set_allow_folding_all" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (foldable-group+is-allow-folding-all :class 'foldable-group :bind
  "is_allow_folding_all" :hash 36873697)
 bool)