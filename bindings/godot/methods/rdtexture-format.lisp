(common-lisp:in-package :%godot)


(defgmethod
 (rdtexture-format+set-format :class 'rdtexture-format :bind "set_format" :hash
  565531219)
 :void (p-member rendering-device+data-format))

(defgmethod
 (rdtexture-format+get-format :class 'rdtexture-format :bind "get_format" :hash
  2235804183)
 rendering-device+data-format)

(defgmethod
 (rdtexture-format+set-width :class 'rdtexture-format :bind "set_width" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdtexture-format+get-width :class 'rdtexture-format :bind "get_width" :hash
  3905245786)
 int)

(defgmethod
 (rdtexture-format+set-height :class 'rdtexture-format :bind "set_height" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdtexture-format+get-height :class 'rdtexture-format :bind "get_height" :hash
  3905245786)
 int)

(defgmethod
 (rdtexture-format+set-depth :class 'rdtexture-format :bind "set_depth" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdtexture-format+get-depth :class 'rdtexture-format :bind "get_depth" :hash
  3905245786)
 int)

(defgmethod
 (rdtexture-format+set-array-layers :class 'rdtexture-format :bind
  "set_array_layers" :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdtexture-format+get-array-layers :class 'rdtexture-format :bind
  "get_array_layers" :hash 3905245786)
 int)

(defgmethod
 (rdtexture-format+set-mipmaps :class 'rdtexture-format :bind "set_mipmaps"
  :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdtexture-format+get-mipmaps :class 'rdtexture-format :bind "get_mipmaps"
  :hash 3905245786)
 int)

(defgmethod
 (rdtexture-format+set-texture-type :class 'rdtexture-format :bind
  "set_texture_type" :hash 652343381)
 :void (p-member rendering-device+texture-type))

(defgmethod
 (rdtexture-format+get-texture-type :class 'rdtexture-format :bind
  "get_texture_type" :hash 4036357416)
 rendering-device+texture-type)

(defgmethod
 (rdtexture-format+set-samples :class 'rdtexture-format :bind "set_samples"
  :hash 3774171498)
 :void (p-member rendering-device+texture-samples))

(defgmethod
 (rdtexture-format+get-samples :class 'rdtexture-format :bind "get_samples"
  :hash 407791724)
 rendering-device+texture-samples)

(defgmethod
 (rdtexture-format+set-usage-bits :class 'rdtexture-format :bind
  "set_usage_bits" :hash 245642367)
 :void (p-member rendering-device+texture-usage-bits))

(defgmethod
 (rdtexture-format+get-usage-bits :class 'rdtexture-format :bind
  "get_usage_bits" :hash 1313398998)
 rendering-device+texture-usage-bits)

(defgmethod
 (rdtexture-format+set-is-resolve-buffer :class 'rdtexture-format :bind
  "set_is_resolve_buffer" :hash 2586408642)
 :void (p-member bool))

(defgmethod
 (rdtexture-format+get-is-resolve-buffer :class 'rdtexture-format :bind
  "get_is_resolve_buffer" :hash 36873697)
 bool)

(defgmethod
 (rdtexture-format+set-is-discardable :class 'rdtexture-format :bind
  "set_is_discardable" :hash 2586408642)
 :void (p-member bool))

(defgmethod
 (rdtexture-format+get-is-discardable :class 'rdtexture-format :bind
  "get_is_discardable" :hash 36873697)
 bool)

(defgmethod
 (rdtexture-format+add-shareable-format :class 'rdtexture-format :bind
  "add_shareable_format" :hash 565531219)
 :void (format rendering-device+data-format))

(defgmethod
 (rdtexture-format+remove-shareable-format :class 'rdtexture-format :bind
  "remove_shareable_format" :hash 565531219)
 :void (format rendering-device+data-format))