(common-lisp:in-package :%godot)


(defgmethod
 (material+-get-shader-rid :class 'material :bind "_get_shader_rid" :hash
  2944877500 :virtual common-lisp:t)
 rid)

(defgmethod
 (material+-get-shader-mode :class 'material :bind "_get_shader_mode" :hash
  3392948163 :virtual common-lisp:t)
 shader+mode)

(defgmethod
 (material+-can-do-next-pass :class 'material :bind "_can_do_next_pass" :hash
  36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (material+-can-use-render-priority :class 'material :bind
  "_can_use_render_priority" :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (material+set-next-pass :class 'material :bind "set_next_pass" :hash
  2757459619)
 :void (next-pass material))

(defgmethod
 (material+get-next-pass :class 'material :bind "get_next_pass" :hash 5934680)
 material)

(defgmethod
 (material+set-render-priority :class 'material :bind "set_render_priority"
  :hash 1286410249)
 :void (priority int))

(defgmethod
 (material+get-render-priority :class 'material :bind "get_render_priority"
  :hash 3905245786)
 int)

(defgmethod
 (material+inspect-native-shader-code :class 'material :bind
  "inspect_native_shader_code" :hash 3218959716)
 :void)

(defgmethod
 (material+create-placeholder :class 'material :bind "create_placeholder" :hash
  121922552)
 resource)