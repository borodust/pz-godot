(common-lisp:in-package :%godot)


(defgmethod
 (gdscript-language-protocol+get-text-document :class
  'gdscript-language-protocol :bind "get_text_document" :hash 770545799)
 gdscript-text-document)

(defgmethod
 (gdscript-language-protocol+get-workspace :class 'gdscript-language-protocol
  :bind "get_workspace" :hash 969295246)
 gdscript-workspace)

(defgmethod
 (gdscript-language-protocol+is-smart-resolve-enabled :class
  'gdscript-language-protocol :bind "is_smart_resolve_enabled" :hash 36873697)
 bool)

(defgmethod
 (gdscript-language-protocol+is-initialized :class 'gdscript-language-protocol
  :bind "is_initialized" :hash 36873697)
 bool)

(defgmethod
 (gdscript-language-protocol+initialize :class 'gdscript-language-protocol
  :bind "initialize" :hash 3762224011)
 variant (params dictionary))

(defgmethod
 (gdscript-language-protocol+initialized :class 'gdscript-language-protocol
  :bind "initialized" :hash 1114965689)
 :void (params variant))

(defgmethod
 (gdscript-language-protocol+on-client-connected :class
  'gdscript-language-protocol :bind "on_client_connected" :hash 166280745)
 error)

(defgmethod
 (gdscript-language-protocol+on-client-disconnected :class
  'gdscript-language-protocol :bind "on_client_disconnected" :hash 1286410249)
 :void (client-id int))

(defgmethod
 (gdscript-language-protocol+notify-client :class 'gdscript-language-protocol
  :bind "notify_client" :hash 2511212011)
 :void (method string) (params variant) (client-id int))