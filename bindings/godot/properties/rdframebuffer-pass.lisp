(common-lisp:in-package :%godot)


(defgproperty rdframebuffer-pass+color-attachments 'rdframebuffer-pass :get
 'rdframebuffer-pass+get-color-attachments :set
 'rdframebuffer-pass+set-color-attachments)

(defgproperty rdframebuffer-pass+input-attachments 'rdframebuffer-pass :get
 'rdframebuffer-pass+get-input-attachments :set
 'rdframebuffer-pass+set-input-attachments)

(defgproperty rdframebuffer-pass+resolve-attachments 'rdframebuffer-pass :get
 'rdframebuffer-pass+get-resolve-attachments :set
 'rdframebuffer-pass+set-resolve-attachments)

(defgproperty rdframebuffer-pass+preserve-attachments 'rdframebuffer-pass :get
 'rdframebuffer-pass+get-preserve-attachments :set
 'rdframebuffer-pass+set-preserve-attachments)

(defgproperty rdframebuffer-pass+depth-attachment 'rdframebuffer-pass :get
 'rdframebuffer-pass+get-depth-attachment :set
 'rdframebuffer-pass+set-depth-attachment)