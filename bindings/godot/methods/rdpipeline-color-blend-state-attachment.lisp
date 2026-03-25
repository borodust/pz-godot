(common-lisp:in-package :%godot)


(defgmethod
 (rdpipeline-color-blend-state-attachment+set-as-mix :class
  'rdpipeline-color-blend-state-attachment :bind "set_as_mix" :hash 3218959716)
 :void)

(defgmethod
 (rdpipeline-color-blend-state-attachment+set-enable-blend :class
  'rdpipeline-color-blend-state-attachment :bind "set_enable_blend" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-color-blend-state-attachment+get-enable-blend :class
  'rdpipeline-color-blend-state-attachment :bind "get_enable_blend" :hash
  36873697)
 bool)

(defgmethod
 (rdpipeline-color-blend-state-attachment+set-src-color-blend-factor :class
  'rdpipeline-color-blend-state-attachment :bind "set_src_color_blend_factor"
  :hash 2251019273)
 :void (p-member rendering-device+blend-factor))

(defgmethod
 (rdpipeline-color-blend-state-attachment+get-src-color-blend-factor :class
  'rdpipeline-color-blend-state-attachment :bind "get_src_color_blend_factor"
  :hash 3691288359)
 rendering-device+blend-factor)

(defgmethod
 (rdpipeline-color-blend-state-attachment+set-dst-color-blend-factor :class
  'rdpipeline-color-blend-state-attachment :bind "set_dst_color_blend_factor"
  :hash 2251019273)
 :void (p-member rendering-device+blend-factor))

(defgmethod
 (rdpipeline-color-blend-state-attachment+get-dst-color-blend-factor :class
  'rdpipeline-color-blend-state-attachment :bind "get_dst_color_blend_factor"
  :hash 3691288359)
 rendering-device+blend-factor)

(defgmethod
 (rdpipeline-color-blend-state-attachment+set-color-blend-op :class
  'rdpipeline-color-blend-state-attachment :bind "set_color_blend_op" :hash
  3073022720)
 :void (p-member rendering-device+blend-operation))

(defgmethod
 (rdpipeline-color-blend-state-attachment+get-color-blend-op :class
  'rdpipeline-color-blend-state-attachment :bind "get_color_blend_op" :hash
  1385093561)
 rendering-device+blend-operation)

(defgmethod
 (rdpipeline-color-blend-state-attachment+set-src-alpha-blend-factor :class
  'rdpipeline-color-blend-state-attachment :bind "set_src_alpha_blend_factor"
  :hash 2251019273)
 :void (p-member rendering-device+blend-factor))

(defgmethod
 (rdpipeline-color-blend-state-attachment+get-src-alpha-blend-factor :class
  'rdpipeline-color-blend-state-attachment :bind "get_src_alpha_blend_factor"
  :hash 3691288359)
 rendering-device+blend-factor)

(defgmethod
 (rdpipeline-color-blend-state-attachment+set-dst-alpha-blend-factor :class
  'rdpipeline-color-blend-state-attachment :bind "set_dst_alpha_blend_factor"
  :hash 2251019273)
 :void (p-member rendering-device+blend-factor))

(defgmethod
 (rdpipeline-color-blend-state-attachment+get-dst-alpha-blend-factor :class
  'rdpipeline-color-blend-state-attachment :bind "get_dst_alpha_blend_factor"
  :hash 3691288359)
 rendering-device+blend-factor)

(defgmethod
 (rdpipeline-color-blend-state-attachment+set-alpha-blend-op :class
  'rdpipeline-color-blend-state-attachment :bind "set_alpha_blend_op" :hash
  3073022720)
 :void (p-member rendering-device+blend-operation))

(defgmethod
 (rdpipeline-color-blend-state-attachment+get-alpha-blend-op :class
  'rdpipeline-color-blend-state-attachment :bind "get_alpha_blend_op" :hash
  1385093561)
 rendering-device+blend-operation)

(defgmethod
 (rdpipeline-color-blend-state-attachment+set-write-r :class
  'rdpipeline-color-blend-state-attachment :bind "set_write_r" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-color-blend-state-attachment+get-write-r :class
  'rdpipeline-color-blend-state-attachment :bind "get_write_r" :hash 36873697)
 bool)

(defgmethod
 (rdpipeline-color-blend-state-attachment+set-write-g :class
  'rdpipeline-color-blend-state-attachment :bind "set_write_g" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-color-blend-state-attachment+get-write-g :class
  'rdpipeline-color-blend-state-attachment :bind "get_write_g" :hash 36873697)
 bool)

(defgmethod
 (rdpipeline-color-blend-state-attachment+set-write-b :class
  'rdpipeline-color-blend-state-attachment :bind "set_write_b" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-color-blend-state-attachment+get-write-b :class
  'rdpipeline-color-blend-state-attachment :bind "get_write_b" :hash 36873697)
 bool)

(defgmethod
 (rdpipeline-color-blend-state-attachment+set-write-a :class
  'rdpipeline-color-blend-state-attachment :bind "set_write_a" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-color-blend-state-attachment+get-write-a :class
  'rdpipeline-color-blend-state-attachment :bind "get_write_a" :hash 36873697)
 bool)