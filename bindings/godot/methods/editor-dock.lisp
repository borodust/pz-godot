(common-lisp:in-package :%godot)


(defgmethod
 (editor-dock+%update-layout :class 'editor-dock :bind "_update_layout" :hash
  1286410249 :virtual common-lisp:t)
 :void (layout int))

(defgmethod
 (editor-dock+%save-layout-to-config :class 'editor-dock :bind
  "_save_layout_to_config" :hash 3076455711 :virtual common-lisp:t)
 :void (config config-file) (section string))

(defgmethod
 (editor-dock+%load-layout-from-config :class 'editor-dock :bind
  "_load_layout_from_config" :hash 2838822993 :virtual common-lisp:t)
 :void (config config-file) (section string))

(defgmethod
 (editor-dock+open :class 'editor-dock :bind "open" :hash 3218959716) :void)

(defgmethod
 (editor-dock+make-visible :class 'editor-dock :bind "make_visible" :hash
  3218959716)
 :void)

(defgmethod
 (editor-dock+close :class 'editor-dock :bind "close" :hash 3218959716) :void)

(defgmethod
 (editor-dock+set-title :class 'editor-dock :bind "set_title" :hash 83702148)
 :void (title string))

(defgmethod
 (editor-dock+get-title :class 'editor-dock :bind "get_title" :hash 201670096)
 string)

(defgmethod
 (editor-dock+set-layout-key :class 'editor-dock :bind "set_layout_key" :hash
  83702148)
 :void (layout-key string))

(defgmethod
 (editor-dock+get-layout-key :class 'editor-dock :bind "get_layout_key" :hash
  201670096)
 string)

(defgmethod
 (editor-dock+set-global :class 'editor-dock :bind "set_global" :hash
  2586408642)
 :void (global bool))

(defgmethod
 (editor-dock+is-global :class 'editor-dock :bind "is_global" :hash 36873697)
 bool)

(defgmethod
 (editor-dock+set-transient :class 'editor-dock :bind "set_transient" :hash
  2586408642)
 :void (transient bool))

(defgmethod
 (editor-dock+is-transient :class 'editor-dock :bind "is_transient" :hash
  36873697)
 bool)

(defgmethod
 (editor-dock+set-closable :class 'editor-dock :bind "set_closable" :hash
  2586408642)
 :void (closable bool))

(defgmethod
 (editor-dock+is-closable :class 'editor-dock :bind "is_closable" :hash
  36873697)
 bool)

(defgmethod
 (editor-dock+set-icon-name :class 'editor-dock :bind "set_icon_name" :hash
  3304788590)
 :void (icon-name string-name))

(defgmethod
 (editor-dock+get-icon-name :class 'editor-dock :bind "get_icon_name" :hash
  2002593661)
 string-name)

(defgmethod
 (editor-dock+set-dock-icon :class 'editor-dock :bind "set_dock_icon" :hash
  4051416890)
 :void (icon texture-2d))

(defgmethod
 (editor-dock+get-dock-icon :class 'editor-dock :bind "get_dock_icon" :hash
  3635182373)
 texture-2d)

(defgmethod
 (editor-dock+set-force-show-icon :class 'editor-dock :bind
  "set_force_show_icon" :hash 2586408642)
 :void (force bool))

(defgmethod
 (editor-dock+get-force-show-icon :class 'editor-dock :bind
  "get_force_show_icon" :hash 36873697)
 bool)

(defgmethod
 (editor-dock+set-title-color :class 'editor-dock :bind "set_title_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (editor-dock+get-title-color :class 'editor-dock :bind "get_title_color" :hash
  3444240500)
 color)

(defgmethod
 (editor-dock+set-dock-shortcut :class 'editor-dock :bind "set_dock_shortcut"
  :hash 857163497)
 :void (shortcut shortcut))

(defgmethod
 (editor-dock+get-dock-shortcut :class 'editor-dock :bind "get_dock_shortcut"
  :hash 3415666916)
 shortcut)

(defgmethod
 (editor-dock+set-default-slot :class 'editor-dock :bind "set_default_slot"
  :hash 4142995464)
 :void (slot editor-dock+dock-slot))

(defgmethod
 (editor-dock+get-default-slot :class 'editor-dock :bind "get_default_slot"
  :hash 3298961740)
 editor-dock+dock-slot)

(defgmethod
 (editor-dock+set-available-layouts :class 'editor-dock :bind
  "set_available_layouts" :hash 3440531249)
 :void (layouts editor-dock+dock-layout))

(defgmethod
 (editor-dock+get-available-layouts :class 'editor-dock :bind
  "get_available_layouts" :hash 495015512)
 editor-dock+dock-layout)