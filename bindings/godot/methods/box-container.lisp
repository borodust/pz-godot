(common-lisp:in-package :%godot)


(defgmethod
 (box-container+add-spacer :class 'box-container :bind "add_spacer" :hash
  1326660695)
 control (begin bool))

(defgmethod
 (box-container+set-alignment :class 'box-container :bind "set_alignment" :hash
  2456745134)
 :void (alignment box-container+alignment-mode))

(defgmethod
 (box-container+get-alignment :class 'box-container :bind "get_alignment" :hash
  1915476527)
 box-container+alignment-mode)

(defgmethod
 (box-container+set-vertical :class 'box-container :bind "set_vertical" :hash
  2586408642)
 :void (vertical bool))

(defgmethod
 (box-container+is-vertical :class 'box-container :bind "is_vertical" :hash
  36873697)
 bool)