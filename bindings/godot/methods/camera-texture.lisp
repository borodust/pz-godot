(common-lisp:in-package :%godot)


(defgmethod
 (camera-texture+set-camera-feed-id :class 'camera-texture :bind
  "set_camera_feed_id" :hash 1286410249)
 :void (feed-id int))

(defgmethod
 (camera-texture+get-camera-feed-id :class 'camera-texture :bind
  "get_camera_feed_id" :hash 3905245786)
 int)

(defgmethod
 (camera-texture+set-which-feed :class 'camera-texture :bind "set_which_feed"
  :hash 1595299230)
 :void (which-feed camera-server+feed-image))

(defgmethod
 (camera-texture+get-which-feed :class 'camera-texture :bind "get_which_feed"
  :hash 91039457)
 camera-server+feed-image)

(defgmethod
 (camera-texture+set-camera-active :class 'camera-texture :bind
  "set_camera_active" :hash 2586408642)
 :void (active bool))

(defgmethod
 (camera-texture+get-camera-active :class 'camera-texture :bind
  "get_camera_active" :hash 36873697)
 bool)