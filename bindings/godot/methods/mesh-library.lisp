(common-lisp:in-package :%godot)


(defgmethod
 (mesh-library+create-item :class 'mesh-library :bind "create_item" :hash
  1286410249)
 :void (id int))

(defgmethod
 (mesh-library+set-item-name :class 'mesh-library :bind "set_item_name" :hash
  501894301)
 :void (id int) (name string))

(defgmethod
 (mesh-library+set-item-mesh :class 'mesh-library :bind "set_item_mesh" :hash
  969122797)
 :void (id int) (mesh mesh))

(defgmethod
 (mesh-library+set-item-mesh-transform :class 'mesh-library :bind
  "set_item_mesh_transform" :hash 3616898986)
 :void (id int) (mesh-transform transform-3d))

(defgmethod
 (mesh-library+set-item-mesh-cast-shadow :class 'mesh-library :bind
  "set_item_mesh_cast_shadow" :hash 3923400443)
 :void (id int)
 (shadow-casting-setting rendering-server+shadow-casting-setting))

(defgmethod
 (mesh-library+set-item-navigation-mesh :class 'mesh-library :bind
  "set_item_navigation_mesh" :hash 3483353960)
 :void (id int) (navigation-mesh navigation-mesh))

(defgmethod
 (mesh-library+set-item-navigation-mesh-transform :class 'mesh-library :bind
  "set_item_navigation_mesh_transform" :hash 3616898986)
 :void (id int) (navigation-mesh transform-3d))

(defgmethod
 (mesh-library+set-item-navigation-layers :class 'mesh-library :bind
  "set_item_navigation_layers" :hash 3937882851)
 :void (id int) (navigation-layers int))

(defgmethod
 (mesh-library+set-item-shapes :class 'mesh-library :bind "set_item_shapes"
  :hash 537221740)
 :void (id int) (shapes array))

(defgmethod
 (mesh-library+set-item-preview :class 'mesh-library :bind "set_item_preview"
  :hash 666127730)
 :void (id int) (texture texture-2d))

(defgmethod
 (mesh-library+get-item-name :class 'mesh-library :bind "get_item_name" :hash
  844755477)
 string (id int))

(defgmethod
 (mesh-library+get-item-mesh :class 'mesh-library :bind "get_item_mesh" :hash
  1576363275)
 mesh (id int))

(defgmethod
 (mesh-library+get-item-mesh-transform :class 'mesh-library :bind
  "get_item_mesh_transform" :hash 1965739696)
 transform-3d (id int))

(defgmethod
 (mesh-library+get-item-mesh-cast-shadow :class 'mesh-library :bind
  "get_item_mesh_cast_shadow" :hash 1841766007)
 rendering-server+shadow-casting-setting (id int))

(defgmethod
 (mesh-library+get-item-navigation-mesh :class 'mesh-library :bind
  "get_item_navigation_mesh" :hash 2729647406)
 navigation-mesh (id int))

(defgmethod
 (mesh-library+get-item-navigation-mesh-transform :class 'mesh-library :bind
  "get_item_navigation_mesh_transform" :hash 1965739696)
 transform-3d (id int))

(defgmethod
 (mesh-library+get-item-navigation-layers :class 'mesh-library :bind
  "get_item_navigation_layers" :hash 923996154)
 int (id int))

(defgmethod
 (mesh-library+get-item-shapes :class 'mesh-library :bind "get_item_shapes"
  :hash 663333327)
 array (id int))

(defgmethod
 (mesh-library+get-item-preview :class 'mesh-library :bind "get_item_preview"
  :hash 3536238170)
 texture-2d (id int))

(defgmethod
 (mesh-library+remove-item :class 'mesh-library :bind "remove_item" :hash
  1286410249)
 :void (id int))

(defgmethod
 (mesh-library+find-item-by-name :class 'mesh-library :bind "find_item_by_name"
  :hash 1321353865)
 int (name string))

(defgmethod
 (mesh-library+clear :class 'mesh-library :bind "clear" :hash 3218959716) :void)

(defgmethod
 (mesh-library+get-item-list :class 'mesh-library :bind "get_item_list" :hash
  1930428628)
 packed-int-32array)

(defgmethod
 (mesh-library+get-last-unused-item-id :class 'mesh-library :bind
  "get_last_unused_item_id" :hash 3905245786)
 int)