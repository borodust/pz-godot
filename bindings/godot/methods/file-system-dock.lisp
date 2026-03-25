(common-lisp:in-package :%godot)


(defgmethod
 (file-system-dock+navigate-to-path :class 'file-system-dock :bind
  "navigate_to_path" :hash 83702148)
 :void (path string))

(defgmethod
 (file-system-dock+add-resource-tooltip-plugin :class 'file-system-dock :bind
  "add_resource_tooltip_plugin" :hash 2258356838)
 :void (plugin editor-resource-tooltip-plugin))

(defgmethod
 (file-system-dock+remove-resource-tooltip-plugin :class 'file-system-dock
  :bind "remove_resource_tooltip_plugin" :hash 2258356838)
 :void (plugin editor-resource-tooltip-plugin))