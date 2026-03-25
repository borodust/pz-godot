(common-lisp:in-package :%godot)


(defgmethod
 (menu-button+get-popup :class 'menu-button :bind "get_popup" :hash 229722558)
 popup-menu)

(defgmethod
 (menu-button+show-popup :class 'menu-button :bind "show_popup" :hash
  3218959716)
 :void)

(defgmethod
 (menu-button+set-switch-on-hover :class 'menu-button :bind
  "set_switch_on_hover" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (menu-button+is-switch-on-hover :class 'menu-button :bind "is_switch_on_hover"
  :hash 2240911060)
 bool)

(defgmethod
 (menu-button+set-disable-shortcuts :class 'menu-button :bind
  "set_disable_shortcuts" :hash 2586408642)
 :void (disabled bool))

(defgmethod
 (menu-button+set-item-count :class 'menu-button :bind "set_item_count" :hash
  1286410249)
 :void (count int))

(defgmethod
 (menu-button+get-item-count :class 'menu-button :bind "get_item_count" :hash
  3905245786)
 int)