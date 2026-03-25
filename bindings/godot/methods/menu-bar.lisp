(common-lisp:in-package :%godot)


(defgmethod
 (menu-bar+set-switch-on-hover :class 'menu-bar :bind "set_switch_on_hover"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (menu-bar+is-switch-on-hover :class 'menu-bar :bind "is_switch_on_hover" :hash
  2240911060)
 bool)

(defgmethod
 (menu-bar+set-disable-shortcuts :class 'menu-bar :bind "set_disable_shortcuts"
  :hash 2586408642)
 :void (disabled bool))

(defgmethod
 (menu-bar+set-prefer-global-menu :class 'menu-bar :bind
  "set_prefer_global_menu" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (menu-bar+is-prefer-global-menu :class 'menu-bar :bind "is_prefer_global_menu"
  :hash 36873697)
 bool)

(defgmethod
 (menu-bar+is-native-menu :class 'menu-bar :bind "is_native_menu" :hash
  36873697)
 bool)

(defgmethod
 (menu-bar+get-menu-count :class 'menu-bar :bind "get_menu_count" :hash
  3905245786)
 int)

(defgmethod
 (menu-bar+set-text-direction :class 'menu-bar :bind "set_text_direction" :hash
  119160795)
 :void (direction control+text-direction))

(defgmethod
 (menu-bar+get-text-direction :class 'menu-bar :bind "get_text_direction" :hash
  797257663)
 control+text-direction)

(defgmethod
 (menu-bar+set-language :class 'menu-bar :bind "set_language" :hash 83702148)
 :void (language string))

(defgmethod
 (menu-bar+get-language :class 'menu-bar :bind "get_language" :hash 201670096)
 string)

(defgmethod
 (menu-bar+set-flat :class 'menu-bar :bind "set_flat" :hash 2586408642) :void
 (enabled bool))

(defgmethod (menu-bar+is-flat :class 'menu-bar :bind "is_flat" :hash 36873697)
 bool)

(defgmethod
 (menu-bar+set-start-index :class 'menu-bar :bind "set_start_index" :hash
  1286410249)
 :void (enabled int))

(defgmethod
 (menu-bar+get-start-index :class 'menu-bar :bind "get_start_index" :hash
  3905245786)
 int)

(defgmethod
 (menu-bar+set-menu-title :class 'menu-bar :bind "set_menu_title" :hash
  501894301)
 :void (menu int) (title string))

(defgmethod
 (menu-bar+get-menu-title :class 'menu-bar :bind "get_menu_title" :hash
  844755477)
 string (menu int))

(defgmethod
 (menu-bar+set-menu-tooltip :class 'menu-bar :bind "set_menu_tooltip" :hash
  501894301)
 :void (menu int) (tooltip string))

(defgmethod
 (menu-bar+get-menu-tooltip :class 'menu-bar :bind "get_menu_tooltip" :hash
  844755477)
 string (menu int))

(defgmethod
 (menu-bar+set-menu-disabled :class 'menu-bar :bind "set_menu_disabled" :hash
  300928843)
 :void (menu int) (disabled bool))

(defgmethod
 (menu-bar+is-menu-disabled :class 'menu-bar :bind "is_menu_disabled" :hash
  1116898809)
 bool (menu int))

(defgmethod
 (menu-bar+set-menu-hidden :class 'menu-bar :bind "set_menu_hidden" :hash
  300928843)
 :void (menu int) (hidden bool))

(defgmethod
 (menu-bar+is-menu-hidden :class 'menu-bar :bind "is_menu_hidden" :hash
  1116898809)
 bool (menu int))

(defgmethod
 (menu-bar+get-menu-popup :class 'menu-bar :bind "get_menu_popup" :hash
  2100501353)
 popup-menu (menu int))