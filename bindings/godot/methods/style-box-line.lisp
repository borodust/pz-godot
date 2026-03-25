(common-lisp:in-package :%godot)


(defgmethod
 (style-box-line+set-color :class 'style-box-line :bind "set_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (style-box-line+get-color :class 'style-box-line :bind "get_color" :hash
  3444240500)
 color)

(defgmethod
 (style-box-line+set-thickness :class 'style-box-line :bind "set_thickness"
  :hash 1286410249)
 :void (thickness int))

(defgmethod
 (style-box-line+get-thickness :class 'style-box-line :bind "get_thickness"
  :hash 3905245786)
 int)

(defgmethod
 (style-box-line+set-grow-begin :class 'style-box-line :bind "set_grow_begin"
  :hash 373806689)
 :void (offset float))

(defgmethod
 (style-box-line+get-grow-begin :class 'style-box-line :bind "get_grow_begin"
  :hash 1740695150)
 float)

(defgmethod
 (style-box-line+set-grow-end :class 'style-box-line :bind "set_grow_end" :hash
  373806689)
 :void (offset float))

(defgmethod
 (style-box-line+get-grow-end :class 'style-box-line :bind "get_grow_end" :hash
  1740695150)
 float)

(defgmethod
 (style-box-line+set-vertical :class 'style-box-line :bind "set_vertical" :hash
  2586408642)
 :void (vertical bool))

(defgmethod
 (style-box-line+is-vertical :class 'style-box-line :bind "is_vertical" :hash
  36873697)
 bool)