(common-lisp:in-package :%godot)


(defgproperty text-paragraph+direction 'text-paragraph :get
 'text-paragraph+get-direction :set 'text-paragraph+set-direction)

(defgproperty text-paragraph+custom-punctuation 'text-paragraph :get
 'text-paragraph+get-custom-punctuation :set
 'text-paragraph+set-custom-punctuation)

(defgproperty text-paragraph+orientation 'text-paragraph :get
 'text-paragraph+get-orientation :set 'text-paragraph+set-orientation)

(defgproperty text-paragraph+preserve-invalid 'text-paragraph :get
 'text-paragraph+get-preserve-invalid :set 'text-paragraph+set-preserve-invalid)

(defgproperty text-paragraph+preserve-control 'text-paragraph :get
 'text-paragraph+get-preserve-control :set 'text-paragraph+set-preserve-control)

(defgproperty text-paragraph+alignment 'text-paragraph :get
 'text-paragraph+get-alignment :set 'text-paragraph+set-alignment)

(defgproperty text-paragraph+break-flags 'text-paragraph :get
 'text-paragraph+get-break-flags :set 'text-paragraph+set-break-flags)

(defgproperty text-paragraph+justification-flags 'text-paragraph :get
 'text-paragraph+get-justification-flags :set
 'text-paragraph+set-justification-flags)

(defgproperty text-paragraph+text-overrun-behavior 'text-paragraph :get
 'text-paragraph+get-text-overrun-behavior :set
 'text-paragraph+set-text-overrun-behavior)

(defgproperty text-paragraph+ellipsis-char 'text-paragraph :get
 'text-paragraph+get-ellipsis-char :set 'text-paragraph+set-ellipsis-char)

(defgproperty text-paragraph+width 'text-paragraph :get
 'text-paragraph+get-width :set 'text-paragraph+set-width)

(defgproperty text-paragraph+max-lines-visible 'text-paragraph :get
 'text-paragraph+get-max-lines-visible :set
 'text-paragraph+set-max-lines-visible)

(defgproperty text-paragraph+line-spacing 'text-paragraph :get
 'text-paragraph+get-line-spacing :set 'text-paragraph+set-line-spacing)