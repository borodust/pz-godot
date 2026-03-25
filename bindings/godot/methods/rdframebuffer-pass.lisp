(common-lisp:in-package :%godot)


(defgmethod
 (rdframebuffer-pass+set-color-attachments :class 'rdframebuffer-pass :bind
  "set_color_attachments" :hash 3614634198)
 :void (p-member packed-int-32array))

(defgmethod
 (rdframebuffer-pass+get-color-attachments :class 'rdframebuffer-pass :bind
  "get_color_attachments" :hash 1930428628)
 packed-int-32array)

(defgmethod
 (rdframebuffer-pass+set-input-attachments :class 'rdframebuffer-pass :bind
  "set_input_attachments" :hash 3614634198)
 :void (p-member packed-int-32array))

(defgmethod
 (rdframebuffer-pass+get-input-attachments :class 'rdframebuffer-pass :bind
  "get_input_attachments" :hash 1930428628)
 packed-int-32array)

(defgmethod
 (rdframebuffer-pass+set-resolve-attachments :class 'rdframebuffer-pass :bind
  "set_resolve_attachments" :hash 3614634198)
 :void (p-member packed-int-32array))

(defgmethod
 (rdframebuffer-pass+get-resolve-attachments :class 'rdframebuffer-pass :bind
  "get_resolve_attachments" :hash 1930428628)
 packed-int-32array)

(defgmethod
 (rdframebuffer-pass+set-preserve-attachments :class 'rdframebuffer-pass :bind
  "set_preserve_attachments" :hash 3614634198)
 :void (p-member packed-int-32array))

(defgmethod
 (rdframebuffer-pass+get-preserve-attachments :class 'rdframebuffer-pass :bind
  "get_preserve_attachments" :hash 1930428628)
 packed-int-32array)

(defgmethod
 (rdframebuffer-pass+set-depth-attachment :class 'rdframebuffer-pass :bind
  "set_depth_attachment" :hash 1286410249)
 :void (p-member int))

(defgmethod
 (rdframebuffer-pass+get-depth-attachment :class 'rdframebuffer-pass :bind
  "get_depth_attachment" :hash 3905245786)
 int)