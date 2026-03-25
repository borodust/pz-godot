(common-lisp:in-package :%godot)


(defgmethod
 (flow-container+get-line-count :class 'flow-container :bind "get_line_count"
  :hash 3905245786)
 int)

(defgmethod
 (flow-container+set-alignment :class 'flow-container :bind "set_alignment"
  :hash 575250951)
 :void (alignment flow-container+alignment-mode))

(defgmethod
 (flow-container+get-alignment :class 'flow-container :bind "get_alignment"
  :hash 3749743559)
 flow-container+alignment-mode)

(defgmethod
 (flow-container+set-last-wrap-alignment :class 'flow-container :bind
  "set_last_wrap_alignment" :hash 2899697495)
 :void (last-wrap-alignment flow-container+last-wrap-alignment-mode))

(defgmethod
 (flow-container+get-last-wrap-alignment :class 'flow-container :bind
  "get_last_wrap_alignment" :hash 3743456014)
 flow-container+last-wrap-alignment-mode)

(defgmethod
 (flow-container+set-vertical :class 'flow-container :bind "set_vertical" :hash
  2586408642)
 :void (vertical bool))

(defgmethod
 (flow-container+is-vertical :class 'flow-container :bind "is_vertical" :hash
  36873697)
 bool)

(defgmethod
 (flow-container+set-reverse-fill :class 'flow-container :bind
  "set_reverse_fill" :hash 2586408642)
 :void (reverse-fill bool))

(defgmethod
 (flow-container+is-reverse-fill :class 'flow-container :bind "is_reverse_fill"
  :hash 36873697)
 bool)