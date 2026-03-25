(common-lisp:in-package :%godot)


(defgproperty editor-property+label 'editor-property :get
 'editor-property+get-label :set 'editor-property+set-label)

(defgproperty editor-property+read-only 'editor-property :get
 'editor-property+is-read-only :set 'editor-property+set-read-only)

(defgproperty editor-property+draw-label 'editor-property :get
 'editor-property+is-draw-label :set 'editor-property+set-draw-label)

(defgproperty editor-property+draw-background 'editor-property :get
 'editor-property+is-draw-background :set 'editor-property+set-draw-background)

(defgproperty editor-property+checkable 'editor-property :get
 'editor-property+is-checkable :set 'editor-property+set-checkable)

(defgproperty editor-property+checked 'editor-property :get
 'editor-property+is-checked :set 'editor-property+set-checked)

(defgproperty editor-property+draw-warning 'editor-property :get
 'editor-property+is-draw-warning :set 'editor-property+set-draw-warning)

(defgproperty editor-property+keying 'editor-property :get
 'editor-property+is-keying :set 'editor-property+set-keying)

(defgproperty editor-property+deletable 'editor-property :get
 'editor-property+is-deletable :set 'editor-property+set-deletable)

(defgproperty editor-property+selectable 'editor-property :get
 'editor-property+is-selectable :set 'editor-property+set-selectable)

(defgproperty editor-property+use-folding 'editor-property :get
 'editor-property+is-using-folding :set 'editor-property+set-use-folding)

(defgproperty editor-property+name-split-ratio 'editor-property :get
 'editor-property+get-name-split-ratio :set
 'editor-property+set-name-split-ratio)