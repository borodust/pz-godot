(common-lisp:in-package :%godot)


(defgmethod
 (gltfstate+add-used-extension :class 'gltfstate :bind "add_used_extension"
  :hash 2678287736)
 :void (extension-name string) (required bool))

(defgmethod
 (gltfstate+append-data-to-buffers :class 'gltfstate :bind
  "append_data_to_buffers" :hash 1460416665)
 int (data packed-byte-array) (deduplication bool))

(defgmethod
 (gltfstate+append-gltf-node :class 'gltfstate :bind "append_gltf_node" :hash
  3562288551)
 int (gltf-node gltfnode) (godot-scene-node node) (parent-node-index int))

(defgmethod
 (gltfstate+get-json :class 'gltfstate :bind "get_json" :hash 3102165223)
 dictionary)

(defgmethod
 (gltfstate+set-json :class 'gltfstate :bind "set_json" :hash 4155329257) :void
 (json dictionary))

(defgmethod
 (gltfstate+get-major-version :class 'gltfstate :bind "get_major_version" :hash
  3905245786)
 int)

(defgmethod
 (gltfstate+set-major-version :class 'gltfstate :bind "set_major_version" :hash
  1286410249)
 :void (major-version int))

(defgmethod
 (gltfstate+get-minor-version :class 'gltfstate :bind "get_minor_version" :hash
  3905245786)
 int)

(defgmethod
 (gltfstate+set-minor-version :class 'gltfstate :bind "set_minor_version" :hash
  1286410249)
 :void (minor-version int))

(defgmethod
 (gltfstate+get-copyright :class 'gltfstate :bind "get_copyright" :hash
  201670096)
 string)

(defgmethod
 (gltfstate+set-copyright :class 'gltfstate :bind "set_copyright" :hash
  83702148)
 :void (copyright string))

(defgmethod
 (gltfstate+get-glb-data :class 'gltfstate :bind "get_glb_data" :hash
  2362200018)
 packed-byte-array)

(defgmethod
 (gltfstate+set-glb-data :class 'gltfstate :bind "set_glb_data" :hash
  2971499966)
 :void (glb-data packed-byte-array))

(defgmethod
 (gltfstate+get-use-named-skin-binds :class 'gltfstate :bind
  "get_use_named_skin_binds" :hash 36873697)
 bool)

(defgmethod
 (gltfstate+set-use-named-skin-binds :class 'gltfstate :bind
  "set_use_named_skin_binds" :hash 2586408642)
 :void (use-named-skin-binds bool))

(defgmethod
 (gltfstate+get-nodes :class 'gltfstate :bind "get_nodes" :hash 3995934104)
 array)

(defgmethod
 (gltfstate+set-nodes :class 'gltfstate :bind "set_nodes" :hash 381264803)
 :void (nodes array))

(defgmethod
 (gltfstate+get-buffers :class 'gltfstate :bind "get_buffers" :hash 3995934104)
 array)

(defgmethod
 (gltfstate+set-buffers :class 'gltfstate :bind "set_buffers" :hash 381264803)
 :void (buffers array))

(defgmethod
 (gltfstate+get-buffer-views :class 'gltfstate :bind "get_buffer_views" :hash
  3995934104)
 array)

(defgmethod
 (gltfstate+set-buffer-views :class 'gltfstate :bind "set_buffer_views" :hash
  381264803)
 :void (buffer-views array))

(defgmethod
 (gltfstate+get-accessors :class 'gltfstate :bind "get_accessors" :hash
  3995934104)
 array)

(defgmethod
 (gltfstate+set-accessors :class 'gltfstate :bind "set_accessors" :hash
  381264803)
 :void (accessors array))

(defgmethod
 (gltfstate+get-meshes :class 'gltfstate :bind "get_meshes" :hash 3995934104)
 array)

(defgmethod
 (gltfstate+set-meshes :class 'gltfstate :bind "set_meshes" :hash 381264803)
 :void (meshes array))

(defgmethod
 (gltfstate+get-animation-players-count :class 'gltfstate :bind
  "get_animation_players_count" :hash 923996154)
 int (anim-player-index int))

(defgmethod
 (gltfstate+get-animation-player :class 'gltfstate :bind "get_animation_player"
  :hash 1550200483)
 animation-player (anim-player-index int))

(defgmethod
 (gltfstate+get-materials :class 'gltfstate :bind "get_materials" :hash
  3995934104)
 array)

(defgmethod
 (gltfstate+set-materials :class 'gltfstate :bind "set_materials" :hash
  381264803)
 :void (materials array))

(defgmethod
 (gltfstate+get-scene-name :class 'gltfstate :bind "get_scene_name" :hash
  201670096)
 string)

(defgmethod
 (gltfstate+set-scene-name :class 'gltfstate :bind "set_scene_name" :hash
  83702148)
 :void (scene-name string))

(defgmethod
 (gltfstate+get-base-path :class 'gltfstate :bind "get_base_path" :hash
  201670096)
 string)

(defgmethod
 (gltfstate+set-base-path :class 'gltfstate :bind "set_base_path" :hash
  83702148)
 :void (base-path string))

(defgmethod
 (gltfstate+get-filename :class 'gltfstate :bind "get_filename" :hash
  201670096)
 string)

(defgmethod
 (gltfstate+set-filename :class 'gltfstate :bind "set_filename" :hash 83702148)
 :void (filename string))

(defgmethod
 (gltfstate+get-root-nodes :class 'gltfstate :bind "get_root_nodes" :hash
  1930428628)
 packed-int-32array)

(defgmethod
 (gltfstate+set-root-nodes :class 'gltfstate :bind "set_root_nodes" :hash
  3614634198)
 :void (root-nodes packed-int-32array))

(defgmethod
 (gltfstate+get-textures :class 'gltfstate :bind "get_textures" :hash
  3995934104)
 array)

(defgmethod
 (gltfstate+set-textures :class 'gltfstate :bind "set_textures" :hash
  381264803)
 :void (textures array))

(defgmethod
 (gltfstate+get-texture-samplers :class 'gltfstate :bind "get_texture_samplers"
  :hash 3995934104)
 array)

(defgmethod
 (gltfstate+set-texture-samplers :class 'gltfstate :bind "set_texture_samplers"
  :hash 381264803)
 :void (texture-samplers array))

(defgmethod
 (gltfstate+get-images :class 'gltfstate :bind "get_images" :hash 3995934104)
 array)

(defgmethod
 (gltfstate+set-images :class 'gltfstate :bind "set_images" :hash 381264803)
 :void (images array))

(defgmethod
 (gltfstate+get-skins :class 'gltfstate :bind "get_skins" :hash 3995934104)
 array)

(defgmethod
 (gltfstate+set-skins :class 'gltfstate :bind "set_skins" :hash 381264803)
 :void (skins array))

(defgmethod
 (gltfstate+get-cameras :class 'gltfstate :bind "get_cameras" :hash 3995934104)
 array)

(defgmethod
 (gltfstate+set-cameras :class 'gltfstate :bind "set_cameras" :hash 381264803)
 :void (cameras array))

(defgmethod
 (gltfstate+get-lights :class 'gltfstate :bind "get_lights" :hash 3995934104)
 array)

(defgmethod
 (gltfstate+set-lights :class 'gltfstate :bind "set_lights" :hash 381264803)
 :void (lights array))

(defgmethod
 (gltfstate+get-unique-names :class 'gltfstate :bind "get_unique_names" :hash
  3995934104)
 array)

(defgmethod
 (gltfstate+set-unique-names :class 'gltfstate :bind "set_unique_names" :hash
  381264803)
 :void (unique-names array))

(defgmethod
 (gltfstate+get-unique-animation-names :class 'gltfstate :bind
  "get_unique_animation_names" :hash 3995934104)
 array)

(defgmethod
 (gltfstate+set-unique-animation-names :class 'gltfstate :bind
  "set_unique_animation_names" :hash 381264803)
 :void (unique-animation-names array))

(defgmethod
 (gltfstate+get-skeletons :class 'gltfstate :bind "get_skeletons" :hash
  3995934104)
 array)

(defgmethod
 (gltfstate+set-skeletons :class 'gltfstate :bind "set_skeletons" :hash
  381264803)
 :void (skeletons array))

(defgmethod
 (gltfstate+get-create-animations :class 'gltfstate :bind
  "get_create_animations" :hash 36873697)
 bool)

(defgmethod
 (gltfstate+set-create-animations :class 'gltfstate :bind
  "set_create_animations" :hash 2586408642)
 :void (create-animations bool))

(defgmethod
 (gltfstate+get-import-as-skeleton-bones :class 'gltfstate :bind
  "get_import_as_skeleton_bones" :hash 36873697)
 bool)

(defgmethod
 (gltfstate+set-import-as-skeleton-bones :class 'gltfstate :bind
  "set_import_as_skeleton_bones" :hash 2586408642)
 :void (import-as-skeleton-bones bool))

(defgmethod
 (gltfstate+get-animations :class 'gltfstate :bind "get_animations" :hash
  3995934104)
 array)

(defgmethod
 (gltfstate+set-animations :class 'gltfstate :bind "set_animations" :hash
  381264803)
 :void (animations array))

(defgmethod
 (gltfstate+get-scene-node :class 'gltfstate :bind "get_scene_node" :hash
  539202265)
 node (gltf-node-index int))

(defgmethod
 (gltfstate+get-node-index :class 'gltfstate :bind "get_node_index" :hash
  3810805390)
 int (scene-node node))

(defgmethod
 (gltfstate+get-additional-data :class 'gltfstate :bind "get_additional_data"
  :hash 2760726917)
 variant (extension-name string-name))

(defgmethod
 (gltfstate+set-additional-data :class 'gltfstate :bind "set_additional_data"
  :hash 3776071444)
 :void (extension-name string-name) (additional-data variant))

(defgmethod
 (gltfstate+get-handle-binary-image-mode :class 'gltfstate :bind
  "get_handle_binary_image_mode" :hash 1363384196)
 gltfstate+handle-binary-image-mode)

(defgmethod
 (gltfstate+set-handle-binary-image-mode :class 'gltfstate :bind
  "set_handle_binary_image_mode" :hash 854676334)
 :void (method gltfstate+handle-binary-image-mode))

(defgmethod
 (gltfstate+set-bake-fps :class 'gltfstate :bind "set_bake_fps" :hash
  373806689)
 :void (value float))

(defgmethod
 (gltfstate+get-bake-fps :class 'gltfstate :bind "get_bake_fps" :hash
  1740695150)
 float)

(defgmethod
 (gltfstate+get-handle-binary-image :class 'gltfstate :bind
  "get_handle_binary_image" :hash 3905245786)
 int)

(defgmethod
 (gltfstate+set-handle-binary-image :class 'gltfstate :bind
  "set_handle_binary_image" :hash 1286410249)
 :void (method int))