(common-lisp:in-package :%godot)


(defgmethod
 (canvas-item-material+set-blend-mode :class 'canvas-item-material :bind
  "set_blend_mode" :hash 1786054936)
 :void (blend-mode canvas-item-material+blend-mode))

(defgmethod
 (canvas-item-material+get-blend-mode :class 'canvas-item-material :bind
  "get_blend_mode" :hash 3318684035)
 canvas-item-material+blend-mode)

(defgmethod
 (canvas-item-material+set-light-mode :class 'canvas-item-material :bind
  "set_light_mode" :hash 628074070)
 :void (light-mode canvas-item-material+light-mode))

(defgmethod
 (canvas-item-material+get-light-mode :class 'canvas-item-material :bind
  "get_light_mode" :hash 3863292382)
 canvas-item-material+light-mode)

(defgmethod
 (canvas-item-material+set-particles-animation :class 'canvas-item-material
  :bind "set_particles_animation" :hash 2586408642)
 :void (particles-anim bool))

(defgmethod
 (canvas-item-material+get-particles-animation :class 'canvas-item-material
  :bind "get_particles_animation" :hash 36873697)
 bool)

(defgmethod
 (canvas-item-material+set-particles-anim-h-frames :class 'canvas-item-material
  :bind "set_particles_anim_h_frames" :hash 1286410249)
 :void (frames int))

(defgmethod
 (canvas-item-material+get-particles-anim-h-frames :class 'canvas-item-material
  :bind "get_particles_anim_h_frames" :hash 3905245786)
 int)

(defgmethod
 (canvas-item-material+set-particles-anim-v-frames :class 'canvas-item-material
  :bind "set_particles_anim_v_frames" :hash 1286410249)
 :void (frames int))

(defgmethod
 (canvas-item-material+get-particles-anim-v-frames :class 'canvas-item-material
  :bind "get_particles_anim_v_frames" :hash 3905245786)
 int)

(defgmethod
 (canvas-item-material+set-particles-anim-loop :class 'canvas-item-material
  :bind "set_particles_anim_loop" :hash 2586408642)
 :void (loop bool))

(defgmethod
 (canvas-item-material+get-particles-anim-loop :class 'canvas-item-material
  :bind "get_particles_anim_loop" :hash 36873697)
 bool)