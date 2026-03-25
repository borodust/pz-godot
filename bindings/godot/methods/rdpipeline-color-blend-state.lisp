(common-lisp:in-package :%godot)


(defgmethod
 (rdpipeline-color-blend-state+set-enable-logic-op :class
  'rdpipeline-color-blend-state :bind "set_enable_logic_op" :hash 2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-color-blend-state+get-enable-logic-op :class
  'rdpipeline-color-blend-state :bind "get_enable_logic_op" :hash 36873697)
 bool)

(defgmethod
 (rdpipeline-color-blend-state+set-logic-op :class
  'rdpipeline-color-blend-state :bind "set_logic_op" :hash 3610841058)
 :void (p-member rendering-device+logic-operation))

(defgmethod
 (rdpipeline-color-blend-state+get-logic-op :class
  'rdpipeline-color-blend-state :bind "get_logic_op" :hash 988254690)
 rendering-device+logic-operation)

(defgmethod
 (rdpipeline-color-blend-state+set-blend-constant :class
  'rdpipeline-color-blend-state :bind "set_blend_constant" :hash 2920490490)
 :void (p-member color))

(defgmethod
 (rdpipeline-color-blend-state+get-blend-constant :class
  'rdpipeline-color-blend-state :bind "get_blend_constant" :hash 3444240500)
 color)

(defgmethod
 (rdpipeline-color-blend-state+set-attachments :class
  'rdpipeline-color-blend-state :bind "set_attachments" :hash 381264803)
 :void (attachments array))

(defgmethod
 (rdpipeline-color-blend-state+get-attachments :class
  'rdpipeline-color-blend-state :bind "get_attachments" :hash 3995934104)
 array)