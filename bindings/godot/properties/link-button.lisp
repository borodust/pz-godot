(common-lisp:in-package :%godot)


(defgproperty link-button+text 'link-button :get 'link-button+get-text :set
 'link-button+set-text)

(defgproperty link-button+underline 'link-button :get
 'link-button+get-underline-mode :set 'link-button+set-underline-mode)

(defgproperty link-button+uri 'link-button :get 'link-button+get-uri :set
 'link-button+set-uri)

(defgproperty link-button+text-overrun-behavior 'link-button :get
 'link-button+get-text-overrun-behavior :set
 'link-button+set-text-overrun-behavior)

(defgproperty link-button+ellipsis-char 'link-button :get
 'link-button+get-ellipsis-char :set 'link-button+set-ellipsis-char)

(defgproperty link-button+text-direction 'link-button :get
 'link-button+get-text-direction :set 'link-button+set-text-direction)

(defgproperty link-button+language 'link-button :get 'link-button+get-language
 :set 'link-button+set-language)

(defgproperty link-button+structured-text-bidi-override 'link-button :get
 'link-button+get-structured-text-bidi-override :set
 'link-button+set-structured-text-bidi-override)

(defgproperty link-button+structured-text-bidi-override-options 'link-button
 :get 'link-button+get-structured-text-bidi-override-options :set
 'link-button+set-structured-text-bidi-override-options)