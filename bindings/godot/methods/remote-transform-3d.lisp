(common-lisp:in-package :%godot)


(defgmethod
 (remote-transform-3d+set-remote-node :class 'remote-transform-3d :bind
  "set_remote_node" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (remote-transform-3d+get-remote-node :class 'remote-transform-3d :bind
  "get_remote_node" :hash 4075236667)
 node-path)

(defgmethod
 (remote-transform-3d+force-update-cache :class 'remote-transform-3d :bind
  "force_update_cache" :hash 3218959716)
 :void)

(defgmethod
 (remote-transform-3d+set-use-global-coordinates :class 'remote-transform-3d
  :bind "set_use_global_coordinates" :hash 2586408642)
 :void (use-global-coordinates bool))

(defgmethod
 (remote-transform-3d+get-use-global-coordinates :class 'remote-transform-3d
  :bind "get_use_global_coordinates" :hash 36873697)
 bool)

(defgmethod
 (remote-transform-3d+set-update-position :class 'remote-transform-3d :bind
  "set_update_position" :hash 2586408642)
 :void (update-remote-position bool))

(defgmethod
 (remote-transform-3d+get-update-position :class 'remote-transform-3d :bind
  "get_update_position" :hash 36873697)
 bool)

(defgmethod
 (remote-transform-3d+set-update-rotation :class 'remote-transform-3d :bind
  "set_update_rotation" :hash 2586408642)
 :void (update-remote-rotation bool))

(defgmethod
 (remote-transform-3d+get-update-rotation :class 'remote-transform-3d :bind
  "get_update_rotation" :hash 36873697)
 bool)

(defgmethod
 (remote-transform-3d+set-update-scale :class 'remote-transform-3d :bind
  "set_update_scale" :hash 2586408642)
 :void (update-remote-scale bool))

(defgmethod
 (remote-transform-3d+get-update-scale :class 'remote-transform-3d :bind
  "get_update_scale" :hash 36873697)
 bool)