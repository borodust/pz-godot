(common-lisp:in-package :%godot)


(defgmethod
 (link-button+set-text :class 'link-button :bind "set_text" :hash 83702148)
 :void (text string))

(defgmethod
 (link-button+get-text :class 'link-button :bind "get_text" :hash 201670096)
 string)

(defgmethod
 (link-button+set-text-overrun-behavior :class 'link-button :bind
  "set_text_overrun_behavior" :hash 1008890932)
 :void (overrun-behavior text-server+overrun-behavior))

(defgmethod
 (link-button+get-text-overrun-behavior :class 'link-button :bind
  "get_text_overrun_behavior" :hash 3779142101)
 text-server+overrun-behavior)

(defgmethod
 (link-button+set-ellipsis-char :class 'link-button :bind "set_ellipsis_char"
  :hash 83702148)
 :void (char string))

(defgmethod
 (link-button+get-ellipsis-char :class 'link-button :bind "get_ellipsis_char"
  :hash 201670096)
 string)

(defgmethod
 (link-button+set-text-direction :class 'link-button :bind "set_text_direction"
  :hash 119160795)
 :void (direction control+text-direction))

(defgmethod
 (link-button+get-text-direction :class 'link-button :bind "get_text_direction"
  :hash 797257663)
 control+text-direction)

(defgmethod
 (link-button+set-language :class 'link-button :bind "set_language" :hash
  83702148)
 :void (language string))

(defgmethod
 (link-button+get-language :class 'link-button :bind "get_language" :hash
  201670096)
 string)

(defgmethod
 (link-button+set-uri :class 'link-button :bind "set_uri" :hash 83702148) :void
 (uri string))

(defgmethod
 (link-button+get-uri :class 'link-button :bind "get_uri" :hash 201670096)
 string)

(defgmethod
 (link-button+set-underline-mode :class 'link-button :bind "set_underline_mode"
  :hash 4032947085)
 :void (underline-mode link-button+underline-mode))

(defgmethod
 (link-button+get-underline-mode :class 'link-button :bind "get_underline_mode"
  :hash 568343738)
 link-button+underline-mode)

(defgmethod
 (link-button+set-structured-text-bidi-override :class 'link-button :bind
  "set_structured_text_bidi_override" :hash 55961453)
 :void (parser text-server+structured-text-parser))

(defgmethod
 (link-button+get-structured-text-bidi-override :class 'link-button :bind
  "get_structured_text_bidi_override" :hash 3385126229)
 text-server+structured-text-parser)

(defgmethod
 (link-button+set-structured-text-bidi-override-options :class 'link-button
  :bind "set_structured_text_bidi_override_options" :hash 381264803)
 :void (args array))

(defgmethod
 (link-button+get-structured-text-bidi-override-options :class 'link-button
  :bind "get_structured_text_bidi_override_options" :hash 3995934104)
 array)