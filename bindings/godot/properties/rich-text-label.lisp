(common-lisp:in-package :%godot)


(defgproperty rich-text-label+bbcode-enabled 'rich-text-label :get
 'rich-text-label+is-using-bbcode :set 'rich-text-label+set-use-bbcode)

(defgproperty rich-text-label+text 'rich-text-label :get
 'rich-text-label+get-text :set 'rich-text-label+set-text)

(defgproperty rich-text-label+fit-content 'rich-text-label :get
 'rich-text-label+is-fit-content-enabled :set 'rich-text-label+set-fit-content)

(defgproperty rich-text-label+scroll-active 'rich-text-label :get
 'rich-text-label+is-scroll-active :set 'rich-text-label+set-scroll-active)

(defgproperty rich-text-label+scroll-following 'rich-text-label :get
 'rich-text-label+is-scroll-following :set 'rich-text-label+set-scroll-follow)

(defgproperty rich-text-label+scroll-following-visible-characters
 'rich-text-label :get 'rich-text-label+is-scroll-following-visible-characters
 :set 'rich-text-label+set-scroll-follow-visible-characters)

(defgproperty rich-text-label+autowrap-mode 'rich-text-label :get
 'rich-text-label+get-autowrap-mode :set 'rich-text-label+set-autowrap-mode)

(defgproperty rich-text-label+autowrap-trim-flags 'rich-text-label :get
 'rich-text-label+get-autowrap-trim-flags :set
 'rich-text-label+set-autowrap-trim-flags)

(defgproperty rich-text-label+tab-size 'rich-text-label :get
 'rich-text-label+get-tab-size :set 'rich-text-label+set-tab-size)

(defgproperty rich-text-label+context-menu-enabled 'rich-text-label :get
 'rich-text-label+is-context-menu-enabled :set
 'rich-text-label+set-context-menu-enabled)

(defgproperty rich-text-label+shortcut-keys-enabled 'rich-text-label :get
 'rich-text-label+is-shortcut-keys-enabled :set
 'rich-text-label+set-shortcut-keys-enabled)

(defgproperty rich-text-label+horizontal-alignment 'rich-text-label :get
 'rich-text-label+get-horizontal-alignment :set
 'rich-text-label+set-horizontal-alignment)

(defgproperty rich-text-label+vertical-alignment 'rich-text-label :get
 'rich-text-label+get-vertical-alignment :set
 'rich-text-label+set-vertical-alignment)

(defgproperty rich-text-label+justification-flags 'rich-text-label :get
 'rich-text-label+get-justification-flags :set
 'rich-text-label+set-justification-flags)

(defgproperty rich-text-label+tab-stops 'rich-text-label :get
 'rich-text-label+get-tab-stops :set 'rich-text-label+set-tab-stops)

(defgproperty rich-text-label+custom-effects 'rich-text-label :get
 'rich-text-label+get-effects :set 'rich-text-label+set-effects)

(defgproperty rich-text-label+meta-underlined 'rich-text-label :get
 'rich-text-label+is-meta-underlined :set 'rich-text-label+set-meta-underline)

(defgproperty rich-text-label+hint-underlined 'rich-text-label :get
 'rich-text-label+is-hint-underlined :set 'rich-text-label+set-hint-underline)

(defgproperty rich-text-label+threaded 'rich-text-label :get
 'rich-text-label+is-threaded :set 'rich-text-label+set-threaded)

(defgproperty rich-text-label+progress-bar-delay 'rich-text-label :get
 'rich-text-label+get-progress-bar-delay :set
 'rich-text-label+set-progress-bar-delay)

(defgproperty rich-text-label+selection-enabled 'rich-text-label :get
 'rich-text-label+is-selection-enabled :set
 'rich-text-label+set-selection-enabled)

(defgproperty rich-text-label+deselect-on-focus-loss-enabled 'rich-text-label
 :get 'rich-text-label+is-deselect-on-focus-loss-enabled :set
 'rich-text-label+set-deselect-on-focus-loss-enabled)

(defgproperty rich-text-label+drag-and-drop-selection-enabled 'rich-text-label
 :get 'rich-text-label+is-drag-and-drop-selection-enabled :set
 'rich-text-label+set-drag-and-drop-selection-enabled)

(defgproperty rich-text-label+visible-characters 'rich-text-label :get
 'rich-text-label+get-visible-characters :set
 'rich-text-label+set-visible-characters)

(defgproperty rich-text-label+visible-characters-behavior 'rich-text-label :get
 'rich-text-label+get-visible-characters-behavior :set
 'rich-text-label+set-visible-characters-behavior)

(defgproperty rich-text-label+visible-ratio 'rich-text-label :get
 'rich-text-label+get-visible-ratio :set 'rich-text-label+set-visible-ratio)

(defgproperty rich-text-label+text-direction 'rich-text-label :get
 'rich-text-label+get-text-direction :set 'rich-text-label+set-text-direction)

(defgproperty rich-text-label+language 'rich-text-label :get
 'rich-text-label+get-language :set 'rich-text-label+set-language)

(defgproperty rich-text-label+structured-text-bidi-override 'rich-text-label
 :get 'rich-text-label+get-structured-text-bidi-override :set
 'rich-text-label+set-structured-text-bidi-override)

(defgproperty rich-text-label+structured-text-bidi-override-options
 'rich-text-label :get
 'rich-text-label+get-structured-text-bidi-override-options :set
 'rich-text-label+set-structured-text-bidi-override-options)