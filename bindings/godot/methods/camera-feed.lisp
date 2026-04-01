(common-lisp:in-package :%godot)


(defgmethod
 (camera-feed+%activate-feed :class 'camera-feed :bind "_activate_feed" :hash
  2240911060 :virtual common-lisp:t)
 bool)

(defgmethod
 (camera-feed+%deactivate-feed :class 'camera-feed :bind "_deactivate_feed"
  :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (camera-feed+get-id :class 'camera-feed :bind "get_id" :hash 3905245786) int)

(defgmethod
 (camera-feed+is-active :class 'camera-feed :bind "is_active" :hash 36873697)
 bool)

(defgmethod
 (camera-feed+set-active :class 'camera-feed :bind "set_active" :hash
  2586408642)
 :void (active bool))

(defgmethod
 (camera-feed+get-name :class 'camera-feed :bind "get_name" :hash 201670096)
 string)

(defgmethod
 (camera-feed+set-name :class 'camera-feed :bind "set_name" :hash 83702148)
 :void (name string))

(defgmethod
 (camera-feed+get-position :class 'camera-feed :bind "get_position" :hash
  2711679033)
 camera-feed+feed-position)

(defgmethod
 (camera-feed+set-position :class 'camera-feed :bind "set_position" :hash
  611162623)
 :void (position camera-feed+feed-position))

(defgmethod
 (camera-feed+get-transform :class 'camera-feed :bind "get_transform" :hash
  3814499831)
 transform-2d)

(defgmethod
 (camera-feed+set-transform :class 'camera-feed :bind "set_transform" :hash
  2761652528)
 :void (transform transform-2d))

(defgmethod
 (camera-feed+set-rgb-image :class 'camera-feed :bind "set_rgb_image" :hash
  532598488)
 :void (rgb-image image))

(defgmethod
 (camera-feed+set-ycbcr-image :class 'camera-feed :bind "set_ycbcr_image" :hash
  532598488)
 :void (ycbcr-image image))

(defgmethod
 (camera-feed+set-ycbcr-images :class 'camera-feed :bind "set_ycbcr_images"
  :hash 1986484629)
 :void (y-image image) (cbcr-image image))

(defgmethod
 (camera-feed+set-external :class 'camera-feed :bind "set_external" :hash
  3937882851)
 :void (width int) (height int))

(defgmethod
 (camera-feed+get-texture-tex-id :class 'camera-feed :bind "get_texture_tex_id"
  :hash 1135699418)
 int (feed-image-type camera-server+feed-image))

(defgmethod
 (camera-feed+get-datatype :class 'camera-feed :bind "get_datatype" :hash
  1477782850)
 camera-feed+feed-data-type)

(defgmethod
 (camera-feed+get-formats :class 'camera-feed :bind "get_formats" :hash
  3995934104)
 array)

(defgmethod
 (camera-feed+set-format :class 'camera-feed :bind "set_format" :hash 31872775)
 bool (index int) (parameters dictionary))