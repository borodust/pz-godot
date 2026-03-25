(common-lisp:in-package :%godot)


(defgproperty text-edit+text 'text-edit :get 'text-edit+get-text :set
 'text-edit+set-text)

(defgproperty text-edit+placeholder-text 'text-edit :get
 'text-edit+get-placeholder :set 'text-edit+set-placeholder)

(defgproperty text-edit+editable 'text-edit :get 'text-edit+is-editable :set
 'text-edit+set-editable)

(defgproperty text-edit+context-menu-enabled 'text-edit :get
 'text-edit+is-context-menu-enabled :set 'text-edit+set-context-menu-enabled)

(defgproperty text-edit+emoji-menu-enabled 'text-edit :get
 'text-edit+is-emoji-menu-enabled :set 'text-edit+set-emoji-menu-enabled)

(defgproperty text-edit+backspace-deletes-composite-character-enabled
 'text-edit :get 'text-edit+is-backspace-deletes-composite-character-enabled
 :set 'text-edit+set-backspace-deletes-composite-character-enabled)

(defgproperty text-edit+shortcut-keys-enabled 'text-edit :get
 'text-edit+is-shortcut-keys-enabled :set 'text-edit+set-shortcut-keys-enabled)

(defgproperty text-edit+selecting-enabled 'text-edit :get
 'text-edit+is-selecting-enabled :set 'text-edit+set-selecting-enabled)

(defgproperty text-edit+deselect-on-focus-loss-enabled 'text-edit :get
 'text-edit+is-deselect-on-focus-loss-enabled :set
 'text-edit+set-deselect-on-focus-loss-enabled)

(defgproperty text-edit+drag-and-drop-selection-enabled 'text-edit :get
 'text-edit+is-drag-and-drop-selection-enabled :set
 'text-edit+set-drag-and-drop-selection-enabled)

(defgproperty text-edit+middle-mouse-paste-enabled 'text-edit :get
 'text-edit+is-middle-mouse-paste-enabled :set
 'text-edit+set-middle-mouse-paste-enabled)

(defgproperty text-edit+empty-selection-clipboard-enabled 'text-edit :get
 'text-edit+is-empty-selection-clipboard-enabled :set
 'text-edit+set-empty-selection-clipboard-enabled)

(defgproperty text-edit+wrap-mode 'text-edit :get
 'text-edit+get-line-wrapping-mode :set 'text-edit+set-line-wrapping-mode)

(defgproperty text-edit+autowrap-mode 'text-edit :get
 'text-edit+get-autowrap-mode :set 'text-edit+set-autowrap-mode)

(defgproperty text-edit+indent-wrapped-lines 'text-edit :get
 'text-edit+is-indent-wrapped-lines :set 'text-edit+set-indent-wrapped-lines)

(defgproperty text-edit+tab-input-mode 'text-edit :get
 'text-edit+get-tab-input-mode :set 'text-edit+set-tab-input-mode)

(defgproperty text-edit+virtual-keyboard-enabled 'text-edit :get
 'text-edit+is-virtual-keyboard-enabled :set
 'text-edit+set-virtual-keyboard-enabled)

(defgproperty text-edit+virtual-keyboard-show-on-focus 'text-edit :get
 'text-edit+get-virtual-keyboard-show-on-focus :set
 'text-edit+set-virtual-keyboard-show-on-focus)

(defgproperty text-edit+scroll-smooth 'text-edit :get
 'text-edit+is-smooth-scroll-enabled :set 'text-edit+set-smooth-scroll-enabled)

(defgproperty text-edit+scroll-v-scroll-speed 'text-edit :get
 'text-edit+get-v-scroll-speed :set 'text-edit+set-v-scroll-speed)

(defgproperty text-edit+scroll-past-end-of-file 'text-edit :get
 'text-edit+is-scroll-past-end-of-file-enabled :set
 'text-edit+set-scroll-past-end-of-file-enabled)

(defgproperty text-edit+scroll-vertical 'text-edit :get 'text-edit+get-v-scroll
 :set 'text-edit+set-v-scroll)

(defgproperty text-edit+scroll-horizontal 'text-edit :get
 'text-edit+get-h-scroll :set 'text-edit+set-h-scroll)

(defgproperty text-edit+scroll-fit-content-height 'text-edit :get
 'text-edit+is-fit-content-height-enabled :set
 'text-edit+set-fit-content-height-enabled)

(defgproperty text-edit+scroll-fit-content-width 'text-edit :get
 'text-edit+is-fit-content-width-enabled :set
 'text-edit+set-fit-content-width-enabled)

(defgproperty text-edit+minimap-draw 'text-edit :get
 'text-edit+is-drawing-minimap :set 'text-edit+set-draw-minimap)

(defgproperty text-edit+minimap-width 'text-edit :get
 'text-edit+get-minimap-width :set 'text-edit+set-minimap-width)

(defgproperty text-edit+caret-type 'text-edit :get 'text-edit+get-caret-type
 :set 'text-edit+set-caret-type)

(defgproperty text-edit+caret-blink 'text-edit :get
 'text-edit+is-caret-blink-enabled :set 'text-edit+set-caret-blink-enabled)

(defgproperty text-edit+caret-blink-interval 'text-edit :get
 'text-edit+get-caret-blink-interval :set 'text-edit+set-caret-blink-interval)

(defgproperty text-edit+caret-draw-when-editable-disabled 'text-edit :get
 'text-edit+is-drawing-caret-when-editable-disabled :set
 'text-edit+set-draw-caret-when-editable-disabled)

(defgproperty text-edit+caret-move-on-right-click 'text-edit :get
 'text-edit+is-move-caret-on-right-click-enabled :set
 'text-edit+set-move-caret-on-right-click-enabled)

(defgproperty text-edit+caret-mid-grapheme 'text-edit :get
 'text-edit+is-caret-mid-grapheme-enabled :set
 'text-edit+set-caret-mid-grapheme-enabled)

(defgproperty text-edit+caret-multiple 'text-edit :get
 'text-edit+is-multiple-carets-enabled :set
 'text-edit+set-multiple-carets-enabled)

(defgproperty text-edit+use-default-word-separators 'text-edit :get
 'text-edit+is-default-word-separators-enabled :set
 'text-edit+set-use-default-word-separators)

(defgproperty text-edit+use-custom-word-separators 'text-edit :get
 'text-edit+is-custom-word-separators-enabled :set
 'text-edit+set-use-custom-word-separators)

(defgproperty text-edit+custom-word-separators 'text-edit :get
 'text-edit+get-custom-word-separators :set
 'text-edit+set-custom-word-separators)

(defgproperty text-edit+syntax-highlighter 'text-edit :get
 'text-edit+get-syntax-highlighter :set 'text-edit+set-syntax-highlighter)

(defgproperty text-edit+highlight-all-occurrences 'text-edit :get
 'text-edit+is-highlight-all-occurrences-enabled :set
 'text-edit+set-highlight-all-occurrences)

(defgproperty text-edit+highlight-current-line 'text-edit :get
 'text-edit+is-highlight-current-line-enabled :set
 'text-edit+set-highlight-current-line)

(defgproperty text-edit+draw-control-chars 'text-edit :get
 'text-edit+get-draw-control-chars :set 'text-edit+set-draw-control-chars)

(defgproperty text-edit+draw-tabs 'text-edit :get 'text-edit+is-drawing-tabs
 :set 'text-edit+set-draw-tabs)

(defgproperty text-edit+draw-spaces 'text-edit :get
 'text-edit+is-drawing-spaces :set 'text-edit+set-draw-spaces)

(defgproperty text-edit+text-direction 'text-edit :get
 'text-edit+get-text-direction :set 'text-edit+set-text-direction)

(defgproperty text-edit+language 'text-edit :get 'text-edit+get-language :set
 'text-edit+set-language)

(defgproperty text-edit+structured-text-bidi-override 'text-edit :get
 'text-edit+get-structured-text-bidi-override :set
 'text-edit+set-structured-text-bidi-override)

(defgproperty text-edit+structured-text-bidi-override-options 'text-edit :get
 'text-edit+get-structured-text-bidi-override-options :set
 'text-edit+set-structured-text-bidi-override-options)