(common-lisp:in-package :%godot)


(defgclass (nil :bind "Nil" :api :builtin :size 0))

(defgclass (bool :bind "bool" :api :builtin :size 1))

(defgclass (int :bind "int" :api :builtin :size 8))

(defgclass (float :bind "float" :api :builtin :size 8))

(defgclass (string :bind "String" :api :builtin :size 8))

(defgclass (vector-2 :bind "Vector2" :api :builtin :size 8)
 (:fields (x float :offset 0) (y float :offset 4)))


(defgenum (vector-2+axis :class 'vector-2) (:x 0) (:y 1))

(defgclass (vector-2i :bind "Vector2i" :api :builtin :size 8)
 (:fields (x :int32 :offset 0) (y :int32 :offset 4)))


(defgenum (vector-2i+axis :class 'vector-2i) (:x 0) (:y 1))

(defgclass (rect-2 :bind "Rect2" :api :builtin :size 16)
 (:fields (position vector-2 :offset 0) (size vector-2 :offset 8)))

(defgclass (rect-2i :bind "Rect2i" :api :builtin :size 16)
 (:fields (position vector-2i :offset 0) (size vector-2i :offset 8)))

(defgclass (vector-3 :bind "Vector3" :api :builtin :size 12)
 (:fields (x float :offset 0) (y float :offset 4) (z float :offset 8)))


(defgenum (vector-3+axis :class 'vector-3) (:x 0) (:y 1) (:z 2))

(defgclass (vector-3i :bind "Vector3i" :api :builtin :size 12)
 (:fields (x :int32 :offset 0) (y :int32 :offset 4) (z :int32 :offset 8)))


(defgenum (vector-3i+axis :class 'vector-3i) (:x 0) (:y 1) (:z 2))

(defgclass (transform-2d :bind "Transform2D" :api :builtin :size 24)
 (:fields (x vector-2 :offset 0) (y vector-2 :offset 8)
  (origin vector-2 :offset 16)))

(defgclass (vector-4 :bind "Vector4" :api :builtin :size 16)
 (:fields (x float :offset 0) (y float :offset 4) (z float :offset 8)
  (w float :offset 12)))


(defgenum (vector-4+axis :class 'vector-4) (:x 0) (:y 1) (:z 2) (:w 3))

(defgclass (vector-4i :bind "Vector4i" :api :builtin :size 16)
 (:fields (x :int32 :offset 0) (y :int32 :offset 4) (z :int32 :offset 8)
  (w :int32 :offset 12)))


(defgenum (vector-4i+axis :class 'vector-4i) (:x 0) (:y 1) (:z 2) (:w 3))

(defgclass (plane :bind "Plane" :api :builtin :size 16)
 (:fields (normal vector-3 :offset 0) (d float :offset 12)))

(defgclass (quaternion :bind "Quaternion" :api :builtin :size 16)
 (:fields (x float :offset 0) (y float :offset 4) (z float :offset 8)
  (w float :offset 12)))

(defgclass (aabb :bind "AABB" :api :builtin :size 24)
 (:fields (position vector-3 :offset 0) (size vector-3 :offset 12)))

(defgclass (basis :bind "Basis" :api :builtin :size 36)
 (:fields (x vector-3 :offset 0) (y vector-3 :offset 12)
  (z vector-3 :offset 24)))

(defgclass (transform-3d :bind "Transform3D" :api :builtin :size 48)
 (:fields (basis basis :offset 0) (origin vector-3 :offset 36)))

(defgclass (projection :bind "Projection" :api :builtin :size 64)
 (:fields (x vector-4 :offset 0) (y vector-4 :offset 16)
  (z vector-4 :offset 32) (w vector-4 :offset 48)))


(defgenum (projection+planes :class 'projection) (:near 0) (:far 1) (:left 2)
 (:top 3) (:right 4) (:bottom 5))

(defgclass (color :bind "Color" :api :builtin :size 16)
 (:fields (r float :offset 0) (g float :offset 4) (b float :offset 8)
  (a float :offset 12)))

(defgclass (string-name :bind "StringName" :api :builtin :size 8))

(defgclass (node-path :bind "NodePath" :api :builtin :size 8))

(defgclass (rid :bind "RID" :api :builtin :size 8))

(defgclass (callable :bind "Callable" :api :builtin :size 16))

(defgclass (signal :bind "Signal" :api :builtin :size 16))

(defgclass (dictionary :bind "Dictionary" :api :builtin :size 8))

(defgclass (array :bind "Array" :api :builtin :size 8))

(defgclass (packed-byte-array :bind "PackedByteArray" :api :builtin :size 16))

(defgclass (packed-int-32array :bind "PackedInt32Array" :api :builtin :size 16))

(defgclass (packed-int-64array :bind "PackedInt64Array" :api :builtin :size 16))

(defgclass
 (packed-float-32array :bind "PackedFloat32Array" :api :builtin :size 16))

(defgclass
 (packed-float-64array :bind "PackedFloat64Array" :api :builtin :size 16))

(defgclass
 (packed-string-array :bind "PackedStringArray" :api :builtin :size 16))

(defgclass
 (packed-vector-2array :bind "PackedVector2Array" :api :builtin :size 16))

(defgclass
 (packed-vector-3array :bind "PackedVector3Array" :api :builtin :size 16))

(defgclass (packed-color-array :bind "PackedColorArray" :api :builtin :size 16))

(defgclass
 (packed-vector-4array :bind "PackedVector4Array" :api :builtin :size 16))

(defgclass (variant :bind "Variant" :api :builtin :size 24))