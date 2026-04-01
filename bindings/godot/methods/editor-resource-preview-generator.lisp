(common-lisp:in-package :%godot)


(defgmethod
 (editor-resource-preview-generator+%handles :class
  'editor-resource-preview-generator :bind "_handles" :hash 3927539163 :virtual
  common-lisp:t)
 bool (type string))

(defgmethod
 (editor-resource-preview-generator+%generate :class
  'editor-resource-preview-generator :bind "_generate" :hash 255939159 :virtual
  common-lisp:t)
 texture-2d (resource resource) (size vector-2i) (metadata dictionary))

(defgmethod
 (editor-resource-preview-generator+%generate-from-path :class
  'editor-resource-preview-generator :bind "_generate_from_path" :hash
  1601192835 :virtual common-lisp:t)
 texture-2d (path string) (size vector-2i) (metadata dictionary))

(defgmethod
 (editor-resource-preview-generator+%generate-small-preview-automatically
  :class 'editor-resource-preview-generator :bind
  "_generate_small_preview_automatically" :hash 36873697 :virtual
  common-lisp:t)
 bool)

(defgmethod
 (editor-resource-preview-generator+%can-generate-small-preview :class
  'editor-resource-preview-generator :bind "_can_generate_small_preview" :hash
  36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (editor-resource-preview-generator+request-draw-and-wait :class
  'editor-resource-preview-generator :bind "request_draw_and_wait" :hash
  145472570)
 :void (viewport rid))