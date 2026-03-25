(common-lisp:in-package :%godot)


(defgproperty gltfdocument+image-format 'gltfdocument :get
 'gltfdocument+get-image-format :set 'gltfdocument+set-image-format)

(defgproperty gltfdocument+lossy-quality 'gltfdocument :get
 'gltfdocument+get-lossy-quality :set 'gltfdocument+set-lossy-quality)

(defgproperty gltfdocument+fallback-image-format 'gltfdocument :get
 'gltfdocument+get-fallback-image-format :set
 'gltfdocument+set-fallback-image-format)

(defgproperty gltfdocument+fallback-image-quality 'gltfdocument :get
 'gltfdocument+get-fallback-image-quality :set
 'gltfdocument+set-fallback-image-quality)

(defgproperty gltfdocument+root-node-mode 'gltfdocument :get
 'gltfdocument+get-root-node-mode :set 'gltfdocument+set-root-node-mode)

(defgproperty gltfdocument+visibility-mode 'gltfdocument :get
 'gltfdocument+get-visibility-mode :set 'gltfdocument+set-visibility-mode)