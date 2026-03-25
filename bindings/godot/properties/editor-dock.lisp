(common-lisp:in-package :%godot)


(defgproperty editor-dock+title 'editor-dock :get 'editor-dock+get-title :set
 'editor-dock+set-title)

(defgproperty editor-dock+layout-key 'editor-dock :get
 'editor-dock+get-layout-key :set 'editor-dock+set-layout-key)

(defgproperty editor-dock+global 'editor-dock :get 'editor-dock+is-global :set
 'editor-dock+set-global)

(defgproperty editor-dock+transient 'editor-dock :get 'editor-dock+is-transient
 :set 'editor-dock+set-transient)

(defgproperty editor-dock+closable 'editor-dock :get 'editor-dock+is-closable
 :set 'editor-dock+set-closable)

(defgproperty editor-dock+icon-name 'editor-dock :get
 'editor-dock+get-icon-name :set 'editor-dock+set-icon-name)

(defgproperty editor-dock+dock-icon 'editor-dock :get
 'editor-dock+get-dock-icon :set 'editor-dock+set-dock-icon)

(defgproperty editor-dock+force-show-icon 'editor-dock :get
 'editor-dock+get-force-show-icon :set 'editor-dock+set-force-show-icon)

(defgproperty editor-dock+title-color 'editor-dock :get
 'editor-dock+get-title-color :set 'editor-dock+set-title-color)

(defgproperty editor-dock+dock-shortcut 'editor-dock :get
 'editor-dock+get-dock-shortcut :set 'editor-dock+set-dock-shortcut)

(defgproperty editor-dock+default-slot 'editor-dock :get
 'editor-dock+get-default-slot :set 'editor-dock+set-default-slot)

(defgproperty editor-dock+available-layouts 'editor-dock :get
 'editor-dock+get-available-layouts :set 'editor-dock+set-available-layouts)