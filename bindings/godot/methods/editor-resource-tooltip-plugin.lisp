(common-lisp:in-package :%godot)


(defgmethod
 (editor-resource-tooltip-plugin+-handles :class
  'editor-resource-tooltip-plugin :bind "_handles" :hash 3927539163 :virtual
  common-lisp:t)
 bool (type string))

(defgmethod
 (editor-resource-tooltip-plugin+-make-tooltip-for-path :class
  'editor-resource-tooltip-plugin :bind "_make_tooltip_for_path" :hash
  4100114520 :virtual common-lisp:t)
 control (path string) (metadata dictionary) (base control))

(defgmethod
 (editor-resource-tooltip-plugin+request-thumbnail :class
  'editor-resource-tooltip-plugin :bind "request_thumbnail" :hash 3245519720)
 :void (path string) (control texture-rect))