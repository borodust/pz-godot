(common-lisp:in-package :%godot)


(defgmethod
 (style-box+%draw :class 'style-box :bind "_draw" :hash 2275962004 :virtual
  common-lisp:t)
 :void (to-canvas-item rid) (rect rect-2))

(defgmethod
 (style-box+%get-draw-rect :class 'style-box :bind "_get_draw_rect" :hash
  408950903 :virtual common-lisp:t)
 rect-2 (rect rect-2))

(defgmethod
 (style-box+%get-minimum-size :class 'style-box :bind "_get_minimum_size" :hash
  3341600327 :virtual common-lisp:t)
 vector-2)

(defgmethod
 (style-box+%test-mask :class 'style-box :bind "_test_mask" :hash 3735564539
  :virtual common-lisp:t)
 bool (point vector-2) (rect rect-2))

(defgmethod
 (style-box+get-minimum-size :class 'style-box :bind "get_minimum_size" :hash
  3341600327)
 vector-2)

(defgmethod
 (style-box+set-content-margin :class 'style-box :bind "set_content_margin"
  :hash 4290182280)
 :void (margin side) (offset float))

(defgmethod
 (style-box+set-content-margin-all :class 'style-box :bind
  "set_content_margin_all" :hash 373806689)
 :void (offset float))

(defgmethod
 (style-box+get-content-margin :class 'style-box :bind "get_content_margin"
  :hash 2869120046)
 float (margin side))

(defgmethod
 (style-box+get-margin :class 'style-box :bind "get_margin" :hash 2869120046)
 float (margin side))

(defgmethod
 (style-box+get-offset :class 'style-box :bind "get_offset" :hash 3341600327)
 vector-2)

(defgmethod (style-box+draw :class 'style-box :bind "draw" :hash 2275962004)
 :void (canvas-item rid) (rect rect-2))

(defgmethod
 (style-box+get-current-item-drawn :class 'style-box :bind
  "get_current_item_drawn" :hash 3213695180)
 canvas-item)

(defgmethod
 (style-box+test-mask :class 'style-box :bind "test_mask" :hash 3735564539)
 bool (point vector-2) (rect rect-2))