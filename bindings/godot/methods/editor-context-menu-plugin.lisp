(common-lisp:in-package :%godot)


(defgmethod
 (editor-context-menu-plugin+%popup-menu :class 'editor-context-menu-plugin
  :bind "_popup_menu" :hash 4015028928 :virtual common-lisp:t)
 :void (paths packed-string-array))

(defgmethod
 (editor-context-menu-plugin+add-menu-shortcut :class
  'editor-context-menu-plugin :bind "add_menu_shortcut" :hash 851596305)
 :void (shortcut shortcut) (callback callable))

(defgmethod
 (editor-context-menu-plugin+add-context-menu-item :class
  'editor-context-menu-plugin :bind "add_context_menu_item" :hash 2748336951)
 :void (name string) (callback callable) (icon texture-2d))

(defgmethod
 (editor-context-menu-plugin+add-context-menu-item-from-shortcut :class
  'editor-context-menu-plugin :bind "add_context_menu_item_from_shortcut" :hash
  3799546916)
 :void (name string) (shortcut shortcut) (icon texture-2d))

(defgmethod
 (editor-context-menu-plugin+add-context-submenu-item :class
  'editor-context-menu-plugin :bind "add_context_submenu_item" :hash
  1994674995)
 :void (name string) (menu popup-menu) (icon texture-2d))