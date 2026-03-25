(common-lisp:in-package :%godot)


(defgmethod
 (rdpipeline-multisample-state+set-sample-count :class
  'rdpipeline-multisample-state :bind "set_sample_count" :hash 3774171498)
 :void (p-member rendering-device+texture-samples))

(defgmethod
 (rdpipeline-multisample-state+get-sample-count :class
  'rdpipeline-multisample-state :bind "get_sample_count" :hash 407791724)
 rendering-device+texture-samples)

(defgmethod
 (rdpipeline-multisample-state+set-enable-sample-shading :class
  'rdpipeline-multisample-state :bind "set_enable_sample_shading" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-multisample-state+get-enable-sample-shading :class
  'rdpipeline-multisample-state :bind "get_enable_sample_shading" :hash
  36873697)
 bool)

(defgmethod
 (rdpipeline-multisample-state+set-min-sample-shading :class
  'rdpipeline-multisample-state :bind "set_min_sample_shading" :hash 373806689)
 :void (p-member float))

(defgmethod
 (rdpipeline-multisample-state+get-min-sample-shading :class
  'rdpipeline-multisample-state :bind "get_min_sample_shading" :hash
  1740695150)
 float)

(defgmethod
 (rdpipeline-multisample-state+set-enable-alpha-to-coverage :class
  'rdpipeline-multisample-state :bind "set_enable_alpha_to_coverage" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-multisample-state+get-enable-alpha-to-coverage :class
  'rdpipeline-multisample-state :bind "get_enable_alpha_to_coverage" :hash
  36873697)
 bool)

(defgmethod
 (rdpipeline-multisample-state+set-enable-alpha-to-one :class
  'rdpipeline-multisample-state :bind "set_enable_alpha_to_one" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-multisample-state+get-enable-alpha-to-one :class
  'rdpipeline-multisample-state :bind "get_enable_alpha_to_one" :hash 36873697)
 bool)

(defgmethod
 (rdpipeline-multisample-state+set-sample-masks :class
  'rdpipeline-multisample-state :bind "set_sample_masks" :hash 381264803)
 :void (masks array))

(defgmethod
 (rdpipeline-multisample-state+get-sample-masks :class
  'rdpipeline-multisample-state :bind "get_sample_masks" :hash 3995934104)
 array)