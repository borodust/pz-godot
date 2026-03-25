(common-lisp:in-package :%godot)


(defgproperty label+text 'label :get 'label+get-text :set 'label+set-text)

(defgproperty label+label-settings 'label :get 'label+get-label-settings :set
 'label+set-label-settings)

(defgproperty label+horizontal-alignment 'label :get
 'label+get-horizontal-alignment :set 'label+set-horizontal-alignment)

(defgproperty label+vertical-alignment 'label :get
 'label+get-vertical-alignment :set 'label+set-vertical-alignment)

(defgproperty label+autowrap-mode 'label :get 'label+get-autowrap-mode :set
 'label+set-autowrap-mode)

(defgproperty label+autowrap-trim-flags 'label :get
 'label+get-autowrap-trim-flags :set 'label+set-autowrap-trim-flags)

(defgproperty label+justification-flags 'label :get
 'label+get-justification-flags :set 'label+set-justification-flags)

(defgproperty label+paragraph-separator 'label :get
 'label+get-paragraph-separator :set 'label+set-paragraph-separator)

(defgproperty label+clip-text 'label :get 'label+is-clipping-text :set
 'label+set-clip-text)

(defgproperty label+text-overrun-behavior 'label :get
 'label+get-text-overrun-behavior :set 'label+set-text-overrun-behavior)

(defgproperty label+ellipsis-char 'label :get 'label+get-ellipsis-char :set
 'label+set-ellipsis-char)

(defgproperty label+uppercase 'label :get 'label+is-uppercase :set
 'label+set-uppercase)

(defgproperty label+tab-stops 'label :get 'label+get-tab-stops :set
 'label+set-tab-stops)

(defgproperty label+lines-skipped 'label :get 'label+get-lines-skipped :set
 'label+set-lines-skipped)

(defgproperty label+max-lines-visible 'label :get 'label+get-max-lines-visible
 :set 'label+set-max-lines-visible)

(defgproperty label+visible-characters 'label :get
 'label+get-visible-characters :set 'label+set-visible-characters)

(defgproperty label+visible-characters-behavior 'label :get
 'label+get-visible-characters-behavior :set
 'label+set-visible-characters-behavior)

(defgproperty label+visible-ratio 'label :get 'label+get-visible-ratio :set
 'label+set-visible-ratio)

(defgproperty label+text-direction 'label :get 'label+get-text-direction :set
 'label+set-text-direction)

(defgproperty label+language 'label :get 'label+get-language :set
 'label+set-language)

(defgproperty label+structured-text-bidi-override 'label :get
 'label+get-structured-text-bidi-override :set
 'label+set-structured-text-bidi-override)

(defgproperty label+structured-text-bidi-override-options 'label :get
 'label+get-structured-text-bidi-override-options :set
 'label+set-structured-text-bidi-override-options)