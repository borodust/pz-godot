(common-lisp:in-package :%godot)


(defgmethod
 (visible-on-screen-enabler-2d+set-enable-mode :class
  'visible-on-screen-enabler-2d :bind "set_enable_mode" :hash 2961788752)
 :void (mode visible-on-screen-enabler-2d+enable-mode))

(defgmethod
 (visible-on-screen-enabler-2d+get-enable-mode :class
  'visible-on-screen-enabler-2d :bind "get_enable_mode" :hash 2650445576)
 visible-on-screen-enabler-2d+enable-mode)

(defgmethod
 (visible-on-screen-enabler-2d+set-enable-node-path :class
  'visible-on-screen-enabler-2d :bind "set_enable_node_path" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (visible-on-screen-enabler-2d+get-enable-node-path :class
  'visible-on-screen-enabler-2d :bind "get_enable_node_path" :hash 277076166)
 node-path)