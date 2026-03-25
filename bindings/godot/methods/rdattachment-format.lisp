(common-lisp:in-package :%godot)


(defgmethod
 (rdattachment-format+set-format :class 'rdattachment-format :bind "set_format"
  :hash 565531219)
 :void (p-member rendering-device+data-format))

(defgmethod
 (rdattachment-format+get-format :class 'rdattachment-format :bind "get_format"
  :hash 2235804183)
 rendering-device+data-format)

(defgmethod
 (rdattachment-format+set-samples :class 'rdattachment-format :bind
  "set_samples" :hash 3774171498)
 :void (p-member rendering-device+texture-samples))

(defgmethod
 (rdattachment-format+get-samples :class 'rdattachment-format :bind
  "get_samples" :hash 407791724)
 rendering-device+texture-samples)

(defgmethod
 (rdattachment-format+set-usage-flags :class 'rdattachment-format :bind
  "set_usage_flags" :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdattachment-format+get-usage-flags :class 'rdattachment-format :bind
  "get_usage_flags" :hash 3905245786)
 int)