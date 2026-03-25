(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-compare+set-comparison-type :class
  'visual-shader-node-compare :bind "set_comparison_type" :hash 516558320)
 :void (type visual-shader-node-compare+comparison-type))

(defgmethod
 (visual-shader-node-compare+get-comparison-type :class
  'visual-shader-node-compare :bind "get_comparison_type" :hash 3495315961)
 visual-shader-node-compare+comparison-type)

(defgmethod
 (visual-shader-node-compare+set-function :class 'visual-shader-node-compare
  :bind "set_function" :hash 2370951349)
 :void (func visual-shader-node-compare+function))

(defgmethod
 (visual-shader-node-compare+get-function :class 'visual-shader-node-compare
  :bind "get_function" :hash 4089164265)
 visual-shader-node-compare+function)

(defgmethod
 (visual-shader-node-compare+set-condition :class 'visual-shader-node-compare
  :bind "set_condition" :hash 918742392)
 :void (condition visual-shader-node-compare+condition))

(defgmethod
 (visual-shader-node-compare+get-condition :class 'visual-shader-node-compare
  :bind "get_condition" :hash 3281078941)
 visual-shader-node-compare+condition)