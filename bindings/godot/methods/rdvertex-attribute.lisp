(common-lisp:in-package :%godot)


(defgmethod
 (rdvertex-attribute+set-binding :class 'rdvertex-attribute :bind "set_binding"
  :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdvertex-attribute+get-binding :class 'rdvertex-attribute :bind "get_binding"
  :hash 3905245786)
 int)

(defgmethod
 (rdvertex-attribute+set-location :class 'rdvertex-attribute :bind
  "set_location" :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdvertex-attribute+get-location :class 'rdvertex-attribute :bind
  "get_location" :hash 3905245786)
 int)

(defgmethod
 (rdvertex-attribute+set-offset :class 'rdvertex-attribute :bind "set_offset"
  :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdvertex-attribute+get-offset :class 'rdvertex-attribute :bind "get_offset"
  :hash 3905245786)
 int)

(defgmethod
 (rdvertex-attribute+set-format :class 'rdvertex-attribute :bind "set_format"
  :hash 565531219)
 :void (p-member rendering-device+data-format))

(defgmethod
 (rdvertex-attribute+get-format :class 'rdvertex-attribute :bind "get_format"
  :hash 2235804183)
 rendering-device+data-format)

(defgmethod
 (rdvertex-attribute+set-stride :class 'rdvertex-attribute :bind "set_stride"
  :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdvertex-attribute+get-stride :class 'rdvertex-attribute :bind "get_stride"
  :hash 3905245786)
 int)

(defgmethod
 (rdvertex-attribute+set-frequency :class 'rdvertex-attribute :bind
  "set_frequency" :hash 522141836)
 :void (p-member rendering-device+vertex-frequency))

(defgmethod
 (rdvertex-attribute+get-frequency :class 'rdvertex-attribute :bind
  "get_frequency" :hash 4154106413)
 rendering-device+vertex-frequency)