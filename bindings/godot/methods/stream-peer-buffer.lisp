(common-lisp:in-package :%godot)


(defgmethod
 (stream-peer-buffer+seek :class 'stream-peer-buffer :bind "seek" :hash
  1286410249)
 :void (position int))

(defgmethod
 (stream-peer-buffer+get-size :class 'stream-peer-buffer :bind "get_size" :hash
  3905245786)
 int)

(defgmethod
 (stream-peer-buffer+get-position :class 'stream-peer-buffer :bind
  "get_position" :hash 3905245786)
 int)

(defgmethod
 (stream-peer-buffer+resize :class 'stream-peer-buffer :bind "resize" :hash
  1286410249)
 :void (size int))

(defgmethod
 (stream-peer-buffer+set-data-array :class 'stream-peer-buffer :bind
  "set_data_array" :hash 2971499966)
 :void (data packed-byte-array))

(defgmethod
 (stream-peer-buffer+get-data-array :class 'stream-peer-buffer :bind
  "get_data_array" :hash 2362200018)
 packed-byte-array)

(defgmethod
 (stream-peer-buffer+clear :class 'stream-peer-buffer :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (stream-peer-buffer+duplicate :class 'stream-peer-buffer :bind "duplicate"
  :hash 2474064677)
 stream-peer-buffer)