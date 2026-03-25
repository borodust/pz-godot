(common-lisp:in-package :%godot)


(defgproperty line-edit+text 'line-edit :get 'line-edit+get-text :set
 'line-edit+set-text)

(defgproperty line-edit+placeholder-text 'line-edit :get
 'line-edit+get-placeholder :set 'line-edit+set-placeholder)

(defgproperty line-edit+alignment 'line-edit :get
 'line-edit+get-horizontal-alignment :set 'line-edit+set-horizontal-alignment)

(defgproperty line-edit+max-length 'line-edit :get 'line-edit+get-max-length
 :set 'line-edit+set-max-length)

(defgproperty line-edit+editable 'line-edit :get 'line-edit+is-editable :set
 'line-edit+set-editable)

(defgproperty line-edit+keep-editing-on-text-submit 'line-edit :get
 'line-edit+is-editing-kept-on-text-submit :set
 'line-edit+set-keep-editing-on-text-submit)

(defgproperty line-edit+expand-to-text-length 'line-edit :get
 'line-edit+is-expand-to-text-length-enabled :set
 'line-edit+set-expand-to-text-length-enabled)

(defgproperty line-edit+context-menu-enabled 'line-edit :get
 'line-edit+is-context-menu-enabled :set 'line-edit+set-context-menu-enabled)

(defgproperty line-edit+emoji-menu-enabled 'line-edit :get
 'line-edit+is-emoji-menu-enabled :set 'line-edit+set-emoji-menu-enabled)

(defgproperty line-edit+backspace-deletes-composite-character-enabled
 'line-edit :get 'line-edit+is-backspace-deletes-composite-character-enabled
 :set 'line-edit+set-backspace-deletes-composite-character-enabled)

(defgproperty line-edit+clear-button-enabled 'line-edit :get
 'line-edit+is-clear-button-enabled :set 'line-edit+set-clear-button-enabled)

(defgproperty line-edit+shortcut-keys-enabled 'line-edit :get
 'line-edit+is-shortcut-keys-enabled :set 'line-edit+set-shortcut-keys-enabled)

(defgproperty line-edit+middle-mouse-paste-enabled 'line-edit :get
 'line-edit+is-middle-mouse-paste-enabled :set
 'line-edit+set-middle-mouse-paste-enabled)

(defgproperty line-edit+selecting-enabled 'line-edit :get
 'line-edit+is-selecting-enabled :set 'line-edit+set-selecting-enabled)

(defgproperty line-edit+deselect-on-focus-loss-enabled 'line-edit :get
 'line-edit+is-deselect-on-focus-loss-enabled :set
 'line-edit+set-deselect-on-focus-loss-enabled)

(defgproperty line-edit+drag-and-drop-selection-enabled 'line-edit :get
 'line-edit+is-drag-and-drop-selection-enabled :set
 'line-edit+set-drag-and-drop-selection-enabled)

(defgproperty line-edit+flat 'line-edit :get 'line-edit+is-flat :set
 'line-edit+set-flat)

(defgproperty line-edit+draw-control-chars 'line-edit :get
 'line-edit+get-draw-control-chars :set 'line-edit+set-draw-control-chars)

(defgproperty line-edit+select-all-on-focus 'line-edit :get
 'line-edit+is-select-all-on-focus :set 'line-edit+set-select-all-on-focus)

(defgproperty line-edit+virtual-keyboard-enabled 'line-edit :get
 'line-edit+is-virtual-keyboard-enabled :set
 'line-edit+set-virtual-keyboard-enabled)

(defgproperty line-edit+virtual-keyboard-show-on-focus 'line-edit :get
 'line-edit+get-virtual-keyboard-show-on-focus :set
 'line-edit+set-virtual-keyboard-show-on-focus)

(defgproperty line-edit+virtual-keyboard-type 'line-edit :get
 'line-edit+get-virtual-keyboard-type :set 'line-edit+set-virtual-keyboard-type)

(defgproperty line-edit+caret-blink 'line-edit :get
 'line-edit+is-caret-blink-enabled :set 'line-edit+set-caret-blink-enabled)

(defgproperty line-edit+caret-blink-interval 'line-edit :get
 'line-edit+get-caret-blink-interval :set 'line-edit+set-caret-blink-interval)

(defgproperty line-edit+caret-column 'line-edit :get
 'line-edit+get-caret-column :set 'line-edit+set-caret-column)

(defgproperty line-edit+caret-force-displayed 'line-edit :get
 'line-edit+is-caret-force-displayed :set 'line-edit+set-caret-force-displayed)

(defgproperty line-edit+caret-mid-grapheme 'line-edit :get
 'line-edit+is-caret-mid-grapheme-enabled :set
 'line-edit+set-caret-mid-grapheme-enabled)

(defgproperty line-edit+secret 'line-edit :get 'line-edit+is-secret :set
 'line-edit+set-secret)

(defgproperty line-edit+secret-character 'line-edit :get
 'line-edit+get-secret-character :set 'line-edit+set-secret-character)

(defgproperty line-edit+text-direction 'line-edit :get
 'line-edit+get-text-direction :set 'line-edit+set-text-direction)

(defgproperty line-edit+language 'line-edit :get 'line-edit+get-language :set
 'line-edit+set-language)

(defgproperty line-edit+structured-text-bidi-override 'line-edit :get
 'line-edit+get-structured-text-bidi-override :set
 'line-edit+set-structured-text-bidi-override)

(defgproperty line-edit+structured-text-bidi-override-options 'line-edit :get
 'line-edit+get-structured-text-bidi-override-options :set
 'line-edit+set-structured-text-bidi-override-options)

(defgproperty line-edit+right-icon 'line-edit :get 'line-edit+get-right-icon
 :set 'line-edit+set-right-icon)

(defgproperty line-edit+icon-expand-mode 'line-edit :get
 'line-edit+get-icon-expand-mode :set 'line-edit+set-icon-expand-mode)

(defgproperty line-edit+right-icon-scale 'line-edit :get
 'line-edit+get-right-icon-scale :set 'line-edit+set-right-icon-scale)