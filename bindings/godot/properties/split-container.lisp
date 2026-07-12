(common-lisp:in-package :%godot)


(defgproperty split-container+split-offsets 'split-container :get
 'split-container+get-split-offsets :set 'split-container+set-split-offsets)

(defgproperty split-container+collapsed 'split-container :get
 'split-container+is-collapsed :set 'split-container+set-collapsed)

(defgproperty split-container+dragging-enabled 'split-container :get
 'split-container+is-dragging-enabled :set
 'split-container+set-dragging-enabled)

(defgproperty split-container+dragger-visibility 'split-container :get
 'split-container+get-dragger-visibility :set
 'split-container+set-dragger-visibility)

(defgproperty split-container+vertical 'split-container :get
 'split-container+is-vertical :set 'split-container+set-vertical)

(defgproperty split-container+touch-dragger-enabled 'split-container :get
 'split-container+is-touch-dragger-enabled :set
 'split-container+set-touch-dragger-enabled)

(defgproperty split-container+drag-nested-intersections 'split-container :get
 'split-container+is-dragging-nested-intersections :set
 'split-container+set-drag-nested-intersections)

(defgproperty split-container+drag-area-margin-begin 'split-container :get
 'split-container+get-drag-area-margin-begin :set
 'split-container+set-drag-area-margin-begin)

(defgproperty split-container+drag-area-margin-end 'split-container :get
 'split-container+get-drag-area-margin-end :set
 'split-container+set-drag-area-margin-end)

(defgproperty split-container+drag-area-offset 'split-container :get
 'split-container+get-drag-area-offset :set
 'split-container+set-drag-area-offset)

(defgproperty split-container+drag-area-highlight-in-editor 'split-container
 :get 'split-container+is-drag-area-highlight-in-editor-enabled :set
 'split-container+set-drag-area-highlight-in-editor)

(defgproperty split-container+split-offset 'split-container :get
 'split-container+get-split-offset :set 'split-container+set-split-offset)