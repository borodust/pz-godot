(common-lisp:in-package :%godot)


(defgproperty tree-item+collapsed 'tree-item :get 'tree-item+is-collapsed :set
 'tree-item+set-collapsed)

(defgproperty tree-item+visible 'tree-item :get 'tree-item+is-visible :set
 'tree-item+set-visible)

(defgproperty tree-item+disable-folding 'tree-item :get
 'tree-item+is-folding-disabled :set 'tree-item+set-disable-folding)

(defgproperty tree-item+custom-minimum-height 'tree-item :get
 'tree-item+get-custom-minimum-height :set 'tree-item+set-custom-minimum-height)