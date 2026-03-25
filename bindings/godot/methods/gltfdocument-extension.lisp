(common-lisp:in-package :%godot)


(defgmethod
 (gltfdocument-extension+-import-preflight :class 'gltfdocument-extension :bind
  "_import_preflight" :hash 412946943 :virtual common-lisp:t)
 error (state gltfstate) (extensions packed-string-array))

(defgmethod
 (gltfdocument-extension+-get-supported-extensions :class
  'gltfdocument-extension :bind "_get_supported_extensions" :hash 2981934095
  :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (gltfdocument-extension+-parse-node-extensions :class 'gltfdocument-extension
  :bind "_parse_node_extensions" :hash 2067053794 :virtual common-lisp:t)
 error (state gltfstate) (gltf-node gltfnode) (extensions dictionary))

(defgmethod
 (gltfdocument-extension+-parse-image-data :class 'gltfdocument-extension :bind
  "_parse_image_data" :hash 3201673288 :virtual common-lisp:t)
 error (state gltfstate) (image-data packed-byte-array) (mime-type string)
 (ret-image image))

(defgmethod
 (gltfdocument-extension+-get-image-file-extension :class
  'gltfdocument-extension :bind "_get_image_file_extension" :hash 2841200299
  :virtual common-lisp:t)
 string)

(defgmethod
 (gltfdocument-extension+-parse-texture-json :class 'gltfdocument-extension
  :bind "_parse_texture_json" :hash 1624327185 :virtual common-lisp:t)
 error (state gltfstate) (texture-json dictionary)
 (ret-gltf-texture gltftexture))

(defgmethod
 (gltfdocument-extension+-import-object-model-property :class
  'gltfdocument-extension :bind "_import_object_model_property" :hash
  1446147484 :virtual common-lisp:t)
 gltfobject-model-property (state gltfstate)
 (split-json-pointer packed-string-array) (partial-paths array))

(defgmethod
 (gltfdocument-extension+-import-post-parse :class 'gltfdocument-extension
  :bind "_import_post_parse" :hash 1704600462 :virtual common-lisp:t)
 error (state gltfstate))

(defgmethod
 (gltfdocument-extension+-import-pre-generate :class 'gltfdocument-extension
  :bind "_import_pre_generate" :hash 1704600462 :virtual common-lisp:t)
 error (state gltfstate))

(defgmethod
 (gltfdocument-extension+-generate-scene-node :class 'gltfdocument-extension
  :bind "_generate_scene_node" :hash 3810899026 :virtual common-lisp:t)
 node-3d (state gltfstate) (gltf-node gltfnode) (scene-parent node))

(defgmethod
 (gltfdocument-extension+-import-node :class 'gltfdocument-extension :bind
  "_import_node" :hash 4064279746 :virtual common-lisp:t)
 error (state gltfstate) (gltf-node gltfnode) (json dictionary) (node node))

(defgmethod
 (gltfdocument-extension+-import-post :class 'gltfdocument-extension :bind
  "_import_post" :hash 295478427 :virtual common-lisp:t)
 error (state gltfstate) (root node))

(defgmethod
 (gltfdocument-extension+-export-preflight :class 'gltfdocument-extension :bind
  "_export_preflight" :hash 295478427 :virtual common-lisp:t)
 error (state gltfstate) (root node))

(defgmethod
 (gltfdocument-extension+-convert-scene-node :class 'gltfdocument-extension
  :bind "_convert_scene_node" :hash 147612932 :virtual common-lisp:t)
 :void (state gltfstate) (gltf-node gltfnode) (scene-node node))

(defgmethod
 (gltfdocument-extension+-export-post-convert :class 'gltfdocument-extension
  :bind "_export_post_convert" :hash 295478427 :virtual common-lisp:t)
 error (state gltfstate) (root node))

(defgmethod
 (gltfdocument-extension+-export-preserialize :class 'gltfdocument-extension
  :bind "_export_preserialize" :hash 1704600462 :virtual common-lisp:t)
 error (state gltfstate))

(defgmethod
 (gltfdocument-extension+-export-object-model-property :class
  'gltfdocument-extension :bind "_export_object_model_property" :hash
  4111022730 :virtual common-lisp:t)
 gltfobject-model-property (state gltfstate) (node-path node-path)
 (godot-node node) (gltf-node-index int) (target-object object)
 (target-depth int))

(defgmethod
 (gltfdocument-extension+-get-saveable-image-formats :class
  'gltfdocument-extension :bind "_get_saveable_image_formats" :hash 2981934095
  :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (gltfdocument-extension+-serialize-image-to-bytes :class
  'gltfdocument-extension :bind "_serialize_image_to_bytes" :hash 276886664
  :virtual common-lisp:t)
 packed-byte-array (state gltfstate) (image image) (image-dict dictionary)
 (image-format string) (lossy-quality float))

(defgmethod
 (gltfdocument-extension+-save-image-at-path :class 'gltfdocument-extension
  :bind "_save_image_at_path" :hash 1844337242 :virtual common-lisp:t)
 error (state gltfstate) (image image) (file-path string) (image-format string)
 (lossy-quality float))

(defgmethod
 (gltfdocument-extension+-serialize-texture-json :class 'gltfdocument-extension
  :bind "_serialize_texture_json" :hash 2565166506 :virtual common-lisp:t)
 error (state gltfstate) (texture-json dictionary) (gltf-texture gltftexture)
 (image-format string))

(defgmethod
 (gltfdocument-extension+-export-node :class 'gltfdocument-extension :bind
  "_export_node" :hash 4064279746 :virtual common-lisp:t)
 error (state gltfstate) (gltf-node gltfnode) (json dictionary) (node node))

(defgmethod
 (gltfdocument-extension+-export-post :class 'gltfdocument-extension :bind
  "_export_post" :hash 1704600462 :virtual common-lisp:t)
 error (state gltfstate))