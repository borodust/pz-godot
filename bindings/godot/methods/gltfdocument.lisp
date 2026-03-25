(common-lisp:in-package :%godot)


(defgmethod
 (gltfdocument+set-image-format :class 'gltfdocument :bind "set_image_format"
  :hash 83702148)
 :void (image-format string))

(defgmethod
 (gltfdocument+get-image-format :class 'gltfdocument :bind "get_image_format"
  :hash 201670096)
 string)

(defgmethod
 (gltfdocument+set-lossy-quality :class 'gltfdocument :bind "set_lossy_quality"
  :hash 373806689)
 :void (lossy-quality float))

(defgmethod
 (gltfdocument+get-lossy-quality :class 'gltfdocument :bind "get_lossy_quality"
  :hash 1740695150)
 float)

(defgmethod
 (gltfdocument+set-fallback-image-format :class 'gltfdocument :bind
  "set_fallback_image_format" :hash 83702148)
 :void (fallback-image-format string))

(defgmethod
 (gltfdocument+get-fallback-image-format :class 'gltfdocument :bind
  "get_fallback_image_format" :hash 201670096)
 string)

(defgmethod
 (gltfdocument+set-fallback-image-quality :class 'gltfdocument :bind
  "set_fallback_image_quality" :hash 373806689)
 :void (fallback-image-quality float))

(defgmethod
 (gltfdocument+get-fallback-image-quality :class 'gltfdocument :bind
  "get_fallback_image_quality" :hash 1740695150)
 float)

(defgmethod
 (gltfdocument+set-root-node-mode :class 'gltfdocument :bind
  "set_root_node_mode" :hash 463633402)
 :void (root-node-mode gltfdocument+root-node-mode))

(defgmethod
 (gltfdocument+get-root-node-mode :class 'gltfdocument :bind
  "get_root_node_mode" :hash 948057992)
 gltfdocument+root-node-mode)

(defgmethod
 (gltfdocument+set-visibility-mode :class 'gltfdocument :bind
  "set_visibility_mode" :hash 2803579218)
 :void (visibility-mode gltfdocument+visibility-mode))

(defgmethod
 (gltfdocument+get-visibility-mode :class 'gltfdocument :bind
  "get_visibility_mode" :hash 3885445962)
 gltfdocument+visibility-mode)

(defgmethod
 (gltfdocument+append-from-file :class 'gltfdocument :bind "append_from_file"
  :hash 866380864)
 error (path string) (state gltfstate) (flags int) (base-path string))

(defgmethod
 (gltfdocument+append-from-buffer :class 'gltfdocument :bind
  "append_from_buffer" :hash 1616081266)
 error (bytes packed-byte-array) (base-path string) (state gltfstate)
 (flags int))

(defgmethod
 (gltfdocument+append-from-scene :class 'gltfdocument :bind "append_from_scene"
  :hash 1622574258)
 error (node node) (state gltfstate) (flags int))

(defgmethod
 (gltfdocument+generate-scene :class 'gltfdocument :bind "generate_scene" :hash
  596118388)
 node (state gltfstate) (bake-fps float) (trimming bool)
 (remove-immutable-tracks bool))

(defgmethod
 (gltfdocument+generate-buffer :class 'gltfdocument :bind "generate_buffer"
  :hash 741783455)
 packed-byte-array (state gltfstate))

(defgmethod
 (gltfdocument+write-to-filesystem :class 'gltfdocument :bind
  "write_to_filesystem" :hash 1784551478)
 error (state gltfstate) (path string))

(defgmethod
 (gltfdocument+import-object-model-property :class 'gltfdocument :bind
  "import_object_model_property" :hash 1206708632 :static common-lisp:t)
 gltfobject-model-property (state gltfstate) (json-pointer string))

(defgmethod
 (gltfdocument+export-object-model-property :class 'gltfdocument :bind
  "export_object_model_property" :hash 314209806 :static common-lisp:t)
 gltfobject-model-property (state gltfstate) (node-path node-path)
 (godot-node node) (gltf-node-index int))

(defgmethod
 (gltfdocument+register-gltf-document-extension :class 'gltfdocument :bind
  "register_gltf_document_extension" :hash 3752678331 :static common-lisp:t)
 :void (extension gltfdocument-extension) (first-priority bool))

(defgmethod
 (gltfdocument+unregister-gltf-document-extension :class 'gltfdocument :bind
  "unregister_gltf_document_extension" :hash 2684415758 :static common-lisp:t)
 :void (extension gltfdocument-extension))

(defgmethod
 (gltfdocument+get-supported-gltf-extensions :class 'gltfdocument :bind
  "get_supported_gltf_extensions" :hash 2981934095 :static common-lisp:t)
 packed-string-array)