(common-lisp:in-package :%godot)


(defgproperty gltfbuffer-view+buffer 'gltfbuffer-view :get
 'gltfbuffer-view+get-buffer :set 'gltfbuffer-view+set-buffer)

(defgproperty gltfbuffer-view+byte-offset 'gltfbuffer-view :get
 'gltfbuffer-view+get-byte-offset :set 'gltfbuffer-view+set-byte-offset)

(defgproperty gltfbuffer-view+byte-length 'gltfbuffer-view :get
 'gltfbuffer-view+get-byte-length :set 'gltfbuffer-view+set-byte-length)

(defgproperty gltfbuffer-view+byte-stride 'gltfbuffer-view :get
 'gltfbuffer-view+get-byte-stride :set 'gltfbuffer-view+set-byte-stride)

(defgproperty gltfbuffer-view+indices 'gltfbuffer-view :get
 'gltfbuffer-view+get-indices :set 'gltfbuffer-view+set-indices)

(defgproperty gltfbuffer-view+vertex-attributes 'gltfbuffer-view :get
 'gltfbuffer-view+get-vertex-attributes :set
 'gltfbuffer-view+set-vertex-attributes)