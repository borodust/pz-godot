(common-lisp:in-package :%godot)


(defgmethod
 (gltfspec-gloss+get-diffuse-img :class 'gltfspec-gloss :bind "get_diffuse_img"
  :hash 564927088)
 image)

(defgmethod
 (gltfspec-gloss+set-diffuse-img :class 'gltfspec-gloss :bind "set_diffuse_img"
  :hash 532598488)
 :void (diffuse-img image))

(defgmethod
 (gltfspec-gloss+get-diffuse-factor :class 'gltfspec-gloss :bind
  "get_diffuse_factor" :hash 3200896285)
 color)

(defgmethod
 (gltfspec-gloss+set-diffuse-factor :class 'gltfspec-gloss :bind
  "set_diffuse_factor" :hash 2920490490)
 :void (diffuse-factor color))

(defgmethod
 (gltfspec-gloss+get-gloss-factor :class 'gltfspec-gloss :bind
  "get_gloss_factor" :hash 191475506)
 float)

(defgmethod
 (gltfspec-gloss+set-gloss-factor :class 'gltfspec-gloss :bind
  "set_gloss_factor" :hash 373806689)
 :void (gloss-factor float))

(defgmethod
 (gltfspec-gloss+get-specular-factor :class 'gltfspec-gloss :bind
  "get_specular_factor" :hash 3200896285)
 color)

(defgmethod
 (gltfspec-gloss+set-specular-factor :class 'gltfspec-gloss :bind
  "set_specular_factor" :hash 2920490490)
 :void (specular-factor color))

(defgmethod
 (gltfspec-gloss+get-spec-gloss-img :class 'gltfspec-gloss :bind
  "get_spec_gloss_img" :hash 564927088)
 image)

(defgmethod
 (gltfspec-gloss+set-spec-gloss-img :class 'gltfspec-gloss :bind
  "set_spec_gloss_img" :hash 532598488)
 :void (spec-gloss-img image))