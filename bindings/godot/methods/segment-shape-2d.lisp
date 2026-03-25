(common-lisp:in-package :%godot)


(defgmethod
 (segment-shape-2d+set-a :class 'segment-shape-2d :bind "set_a" :hash
  743155724)
 :void (a vector-2))

(defgmethod
 (segment-shape-2d+get-a :class 'segment-shape-2d :bind "get_a" :hash
  3341600327)
 vector-2)

(defgmethod
 (segment-shape-2d+set-b :class 'segment-shape-2d :bind "set_b" :hash
  743155724)
 :void (b vector-2))

(defgmethod
 (segment-shape-2d+get-b :class 'segment-shape-2d :bind "get_b" :hash
  3341600327)
 vector-2)