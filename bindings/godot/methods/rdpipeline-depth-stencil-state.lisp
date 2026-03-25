(common-lisp:in-package :%godot)


(defgmethod
 (rdpipeline-depth-stencil-state+set-enable-depth-test :class
  'rdpipeline-depth-stencil-state :bind "set_enable_depth_test" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-depth-stencil-state+get-enable-depth-test :class
  'rdpipeline-depth-stencil-state :bind "get_enable_depth_test" :hash 36873697)
 bool)

(defgmethod
 (rdpipeline-depth-stencil-state+set-enable-depth-write :class
  'rdpipeline-depth-stencil-state :bind "set_enable_depth_write" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-depth-stencil-state+get-enable-depth-write :class
  'rdpipeline-depth-stencil-state :bind "get_enable_depth_write" :hash
  36873697)
 bool)

(defgmethod
 (rdpipeline-depth-stencil-state+set-depth-compare-operator :class
  'rdpipeline-depth-stencil-state :bind "set_depth_compare_operator" :hash
  2573711505)
 :void (p-member rendering-device+compare-operator))

(defgmethod
 (rdpipeline-depth-stencil-state+get-depth-compare-operator :class
  'rdpipeline-depth-stencil-state :bind "get_depth_compare_operator" :hash
  269730778)
 rendering-device+compare-operator)

(defgmethod
 (rdpipeline-depth-stencil-state+set-enable-depth-range :class
  'rdpipeline-depth-stencil-state :bind "set_enable_depth_range" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-depth-stencil-state+get-enable-depth-range :class
  'rdpipeline-depth-stencil-state :bind "get_enable_depth_range" :hash
  36873697)
 bool)

(defgmethod
 (rdpipeline-depth-stencil-state+set-depth-range-min :class
  'rdpipeline-depth-stencil-state :bind "set_depth_range_min" :hash 373806689)
 :void (p-member float))

(defgmethod
 (rdpipeline-depth-stencil-state+get-depth-range-min :class
  'rdpipeline-depth-stencil-state :bind "get_depth_range_min" :hash 1740695150)
 float)

(defgmethod
 (rdpipeline-depth-stencil-state+set-depth-range-max :class
  'rdpipeline-depth-stencil-state :bind "set_depth_range_max" :hash 373806689)
 :void (p-member float))

(defgmethod
 (rdpipeline-depth-stencil-state+get-depth-range-max :class
  'rdpipeline-depth-stencil-state :bind "get_depth_range_max" :hash 1740695150)
 float)

(defgmethod
 (rdpipeline-depth-stencil-state+set-enable-stencil :class
  'rdpipeline-depth-stencil-state :bind "set_enable_stencil" :hash 2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-depth-stencil-state+get-enable-stencil :class
  'rdpipeline-depth-stencil-state :bind "get_enable_stencil" :hash 36873697)
 bool)

(defgmethod
 (rdpipeline-depth-stencil-state+set-front-op-fail :class
  'rdpipeline-depth-stencil-state :bind "set_front_op_fail" :hash 2092799566)
 :void (p-member rendering-device+stencil-operation))

(defgmethod
 (rdpipeline-depth-stencil-state+get-front-op-fail :class
  'rdpipeline-depth-stencil-state :bind "get_front_op_fail" :hash 1714732389)
 rendering-device+stencil-operation)

(defgmethod
 (rdpipeline-depth-stencil-state+set-front-op-pass :class
  'rdpipeline-depth-stencil-state :bind "set_front_op_pass" :hash 2092799566)
 :void (p-member rendering-device+stencil-operation))

(defgmethod
 (rdpipeline-depth-stencil-state+get-front-op-pass :class
  'rdpipeline-depth-stencil-state :bind "get_front_op_pass" :hash 1714732389)
 rendering-device+stencil-operation)

(defgmethod
 (rdpipeline-depth-stencil-state+set-front-op-depth-fail :class
  'rdpipeline-depth-stencil-state :bind "set_front_op_depth_fail" :hash
  2092799566)
 :void (p-member rendering-device+stencil-operation))

(defgmethod
 (rdpipeline-depth-stencil-state+get-front-op-depth-fail :class
  'rdpipeline-depth-stencil-state :bind "get_front_op_depth_fail" :hash
  1714732389)
 rendering-device+stencil-operation)

(defgmethod
 (rdpipeline-depth-stencil-state+set-front-op-compare :class
  'rdpipeline-depth-stencil-state :bind "set_front_op_compare" :hash
  2573711505)
 :void (p-member rendering-device+compare-operator))

(defgmethod
 (rdpipeline-depth-stencil-state+get-front-op-compare :class
  'rdpipeline-depth-stencil-state :bind "get_front_op_compare" :hash 269730778)
 rendering-device+compare-operator)

(defgmethod
 (rdpipeline-depth-stencil-state+set-front-op-compare-mask :class
  'rdpipeline-depth-stencil-state :bind "set_front_op_compare_mask" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdpipeline-depth-stencil-state+get-front-op-compare-mask :class
  'rdpipeline-depth-stencil-state :bind "get_front_op_compare_mask" :hash
  3905245786)
 int)

(defgmethod
 (rdpipeline-depth-stencil-state+set-front-op-write-mask :class
  'rdpipeline-depth-stencil-state :bind "set_front_op_write_mask" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdpipeline-depth-stencil-state+get-front-op-write-mask :class
  'rdpipeline-depth-stencil-state :bind "get_front_op_write_mask" :hash
  3905245786)
 int)

(defgmethod
 (rdpipeline-depth-stencil-state+set-front-op-reference :class
  'rdpipeline-depth-stencil-state :bind "set_front_op_reference" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdpipeline-depth-stencil-state+get-front-op-reference :class
  'rdpipeline-depth-stencil-state :bind "get_front_op_reference" :hash
  3905245786)
 int)

(defgmethod
 (rdpipeline-depth-stencil-state+set-back-op-fail :class
  'rdpipeline-depth-stencil-state :bind "set_back_op_fail" :hash 2092799566)
 :void (p-member rendering-device+stencil-operation))

(defgmethod
 (rdpipeline-depth-stencil-state+get-back-op-fail :class
  'rdpipeline-depth-stencil-state :bind "get_back_op_fail" :hash 1714732389)
 rendering-device+stencil-operation)

(defgmethod
 (rdpipeline-depth-stencil-state+set-back-op-pass :class
  'rdpipeline-depth-stencil-state :bind "set_back_op_pass" :hash 2092799566)
 :void (p-member rendering-device+stencil-operation))

(defgmethod
 (rdpipeline-depth-stencil-state+get-back-op-pass :class
  'rdpipeline-depth-stencil-state :bind "get_back_op_pass" :hash 1714732389)
 rendering-device+stencil-operation)

(defgmethod
 (rdpipeline-depth-stencil-state+set-back-op-depth-fail :class
  'rdpipeline-depth-stencil-state :bind "set_back_op_depth_fail" :hash
  2092799566)
 :void (p-member rendering-device+stencil-operation))

(defgmethod
 (rdpipeline-depth-stencil-state+get-back-op-depth-fail :class
  'rdpipeline-depth-stencil-state :bind "get_back_op_depth_fail" :hash
  1714732389)
 rendering-device+stencil-operation)

(defgmethod
 (rdpipeline-depth-stencil-state+set-back-op-compare :class
  'rdpipeline-depth-stencil-state :bind "set_back_op_compare" :hash 2573711505)
 :void (p-member rendering-device+compare-operator))

(defgmethod
 (rdpipeline-depth-stencil-state+get-back-op-compare :class
  'rdpipeline-depth-stencil-state :bind "get_back_op_compare" :hash 269730778)
 rendering-device+compare-operator)

(defgmethod
 (rdpipeline-depth-stencil-state+set-back-op-compare-mask :class
  'rdpipeline-depth-stencil-state :bind "set_back_op_compare_mask" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdpipeline-depth-stencil-state+get-back-op-compare-mask :class
  'rdpipeline-depth-stencil-state :bind "get_back_op_compare_mask" :hash
  3905245786)
 int)

(defgmethod
 (rdpipeline-depth-stencil-state+set-back-op-write-mask :class
  'rdpipeline-depth-stencil-state :bind "set_back_op_write_mask" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdpipeline-depth-stencil-state+get-back-op-write-mask :class
  'rdpipeline-depth-stencil-state :bind "get_back_op_write_mask" :hash
  3905245786)
 int)

(defgmethod
 (rdpipeline-depth-stencil-state+set-back-op-reference :class
  'rdpipeline-depth-stencil-state :bind "set_back_op_reference" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdpipeline-depth-stencil-state+get-back-op-reference :class
  'rdpipeline-depth-stencil-state :bind "get_back_op_reference" :hash
  3905245786)
 int)