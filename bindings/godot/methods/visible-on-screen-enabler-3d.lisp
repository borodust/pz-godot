(common-lisp:in-package :%godot)


(defgmethod
 (visible-on-screen-enabler-3d+set-enable-mode :class
  'visible-on-screen-enabler-3d :bind "set_enable_mode" :hash 320303646)
 :void (mode visible-on-screen-enabler-3d+enable-mode))

(defgmethod
 (visible-on-screen-enabler-3d+get-enable-mode :class
  'visible-on-screen-enabler-3d :bind "get_enable_mode" :hash 3352990031)
 visible-on-screen-enabler-3d+enable-mode)

(defgmethod
 (visible-on-screen-enabler-3d+set-enable-node-path :class
  'visible-on-screen-enabler-3d :bind "set_enable_node_path" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (visible-on-screen-enabler-3d+get-enable-node-path :class
  'visible-on-screen-enabler-3d :bind "get_enable_node_path" :hash 277076166)
 node-path)