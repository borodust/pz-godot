(common-lisp:in-package :%godot)


(defgconstructor (make-nil :class 'nil :index 0))

(defgconstructor (make-nil@1 :class 'nil :index 1) (from variant))

(defgconstructor (make-bool :class 'bool :index 0))

(defgconstructor (make-bool@1 :class 'bool :index 1) (from bool))

(defgconstructor (make-bool@2 :class 'bool :index 2) (from int))

(defgconstructor (make-bool@3 :class 'bool :index 3) (from float))

(defgconstructor (make-int :class 'int :index 0))

(defgconstructor (make-int@1 :class 'int :index 1) (from int))

(defgconstructor (make-int@2 :class 'int :index 2) (from float))

(defgconstructor (make-int@3 :class 'int :index 3) (from bool))

(defgconstructor (make-int@4 :class 'int :index 4) (from string))

(defgconstructor (make-float :class 'float :index 0))

(defgconstructor (make-float@1 :class 'float :index 1) (from float))

(defgconstructor (make-float@2 :class 'float :index 2) (from int))

(defgconstructor (make-float@3 :class 'float :index 3) (from bool))

(defgconstructor (make-float@4 :class 'float :index 4) (from string))

(defgconstructor (make-string :class 'string :index 0))

(defgconstructor (make-string@1 :class 'string :index 1) (from string))

(defgconstructor (make-string@2 :class 'string :index 2) (from string-name))

(defgconstructor (make-string@3 :class 'string :index 3) (from node-path))

(defgconstructor (make-vector-2 :class 'vector-2 :index 0))

(defgconstructor (make-vector-2@1 :class 'vector-2 :index 1) (from vector-2))

(defgconstructor (make-vector-2@2 :class 'vector-2 :index 2) (from vector-2i))

(defgconstructor (make-vector-2@3 :class 'vector-2 :index 3) (x float)
 (y float))

(defgconstructor (make-vector-2i :class 'vector-2i :index 0))

(defgconstructor (make-vector-2i@1 :class 'vector-2i :index 1) (from vector-2i))

(defgconstructor (make-vector-2i@2 :class 'vector-2i :index 2) (from vector-2))

(defgconstructor (make-vector-2i@3 :class 'vector-2i :index 3) (x int) (y int))

(defgconstructor (make-rect-2 :class 'rect-2 :index 0))

(defgconstructor (make-rect-2@1 :class 'rect-2 :index 1) (from rect-2))

(defgconstructor (make-rect-2@2 :class 'rect-2 :index 2) (from rect-2i))

(defgconstructor (make-rect-2@3 :class 'rect-2 :index 3) (position vector-2)
 (size vector-2))

(defgconstructor (make-rect-2@4 :class 'rect-2 :index 4) (x float) (y float)
 (width float) (height float))

(defgconstructor (make-rect-2i :class 'rect-2i :index 0))

(defgconstructor (make-rect-2i@1 :class 'rect-2i :index 1) (from rect-2i))

(defgconstructor (make-rect-2i@2 :class 'rect-2i :index 2) (from rect-2))

(defgconstructor (make-rect-2i@3 :class 'rect-2i :index 3) (position vector-2i)
 (size vector-2i))

(defgconstructor (make-rect-2i@4 :class 'rect-2i :index 4) (x int) (y int)
 (width int) (height int))

(defgconstructor (make-vector-3 :class 'vector-3 :index 0))

(defgconstructor (make-vector-3@1 :class 'vector-3 :index 1) (from vector-3))

(defgconstructor (make-vector-3@2 :class 'vector-3 :index 2) (from vector-3i))

(defgconstructor (make-vector-3@3 :class 'vector-3 :index 3) (x float)
 (y float) (z float))

(defgconstructor (make-vector-3i :class 'vector-3i :index 0))

(defgconstructor (make-vector-3i@1 :class 'vector-3i :index 1) (from vector-3i))

(defgconstructor (make-vector-3i@2 :class 'vector-3i :index 2) (from vector-3))

(defgconstructor (make-vector-3i@3 :class 'vector-3i :index 3) (x int) (y int)
 (z int))

(defgconstructor (make-transform-2d :class 'transform-2d :index 0))

(defgconstructor (make-transform-2d@1 :class 'transform-2d :index 1)
 (from transform-2d))

(defgconstructor (make-transform-2d@2 :class 'transform-2d :index 2)
 (rotation float) (position vector-2))

(defgconstructor (make-transform-2d@3 :class 'transform-2d :index 3)
 (rotation float) (scale vector-2) (skew float) (position vector-2))

(defgconstructor (make-transform-2d@4 :class 'transform-2d :index 4)
 (x-axis vector-2) (y-axis vector-2) (origin vector-2))

(defgconstructor (make-vector-4 :class 'vector-4 :index 0))

(defgconstructor (make-vector-4@1 :class 'vector-4 :index 1) (from vector-4))

(defgconstructor (make-vector-4@2 :class 'vector-4 :index 2) (from vector-4i))

(defgconstructor (make-vector-4@3 :class 'vector-4 :index 3) (x float)
 (y float) (z float) (w float))

(defgconstructor (make-vector-4i :class 'vector-4i :index 0))

(defgconstructor (make-vector-4i@1 :class 'vector-4i :index 1) (from vector-4i))

(defgconstructor (make-vector-4i@2 :class 'vector-4i :index 2) (from vector-4))

(defgconstructor (make-vector-4i@3 :class 'vector-4i :index 3) (x int) (y int)
 (z int) (w int))

(defgconstructor (make-plane :class 'plane :index 0))

(defgconstructor (make-plane@1 :class 'plane :index 1) (from plane))

(defgconstructor (make-plane@2 :class 'plane :index 2) (normal vector-3))

(defgconstructor (make-plane@3 :class 'plane :index 3) (normal vector-3)
 (d float))

(defgconstructor (make-plane@4 :class 'plane :index 4) (normal vector-3)
 (point vector-3))

(defgconstructor (make-plane@5 :class 'plane :index 5) (point1 vector-3)
 (point2 vector-3) (point3 vector-3))

(defgconstructor (make-plane@6 :class 'plane :index 6) (a float) (b float)
 (c float) (d float))

(defgconstructor (make-quaternion :class 'quaternion :index 0))

(defgconstructor (make-quaternion@1 :class 'quaternion :index 1)
 (from quaternion))

(defgconstructor (make-quaternion@2 :class 'quaternion :index 2) (from basis))

(defgconstructor (make-quaternion@3 :class 'quaternion :index 3)
 (axis vector-3) (angle float))

(defgconstructor (make-quaternion@4 :class 'quaternion :index 4)
 (arc-from vector-3) (arc-to vector-3))

(defgconstructor (make-quaternion@5 :class 'quaternion :index 5) (x float)
 (y float) (z float) (w float))

(defgconstructor (make-aabb :class 'aabb :index 0))

(defgconstructor (make-aabb@1 :class 'aabb :index 1) (from aabb))

(defgconstructor (make-aabb@2 :class 'aabb :index 2) (position vector-3)
 (size vector-3))

(defgconstructor (make-basis :class 'basis :index 0))

(defgconstructor (make-basis@1 :class 'basis :index 1) (from basis))

(defgconstructor (make-basis@2 :class 'basis :index 2) (from quaternion))

(defgconstructor (make-basis@3 :class 'basis :index 3) (axis vector-3)
 (angle float))

(defgconstructor (make-basis@4 :class 'basis :index 4) (x-axis vector-3)
 (y-axis vector-3) (z-axis vector-3))

(defgconstructor (make-transform-3d :class 'transform-3d :index 0))

(defgconstructor (make-transform-3d@1 :class 'transform-3d :index 1)
 (from transform-3d))

(defgconstructor (make-transform-3d@2 :class 'transform-3d :index 2)
 (basis basis) (origin vector-3))

(defgconstructor (make-transform-3d@3 :class 'transform-3d :index 3)
 (x-axis vector-3) (y-axis vector-3) (z-axis vector-3) (origin vector-3))

(defgconstructor (make-transform-3d@4 :class 'transform-3d :index 4)
 (from projection))

(defgconstructor (make-projection :class 'projection :index 0))

(defgconstructor (make-projection@1 :class 'projection :index 1)
 (from projection))

(defgconstructor (make-projection@2 :class 'projection :index 2)
 (from transform-3d))

(defgconstructor (make-projection@3 :class 'projection :index 3)
 (x-axis vector-4) (y-axis vector-4) (z-axis vector-4) (w-axis vector-4))

(defgconstructor (make-color :class 'color :index 0))

(defgconstructor (make-color@1 :class 'color :index 1) (from color))

(defgconstructor (make-color@2 :class 'color :index 2) (from color)
 (alpha float))

(defgconstructor (make-color@3 :class 'color :index 3) (r float) (g float)
 (b float))

(defgconstructor (make-color@4 :class 'color :index 4) (r float) (g float)
 (b float) (a float))

(defgconstructor (make-color@5 :class 'color :index 5) (code string))

(defgconstructor (make-color@6 :class 'color :index 6) (code string)
 (alpha float))

(defgconstructor (make-string-name :class 'string-name :index 0))

(defgconstructor (make-string-name@1 :class 'string-name :index 1)
 (from string-name))

(defgconstructor (make-string-name@2 :class 'string-name :index 2)
 (from string))

(defgconstructor (make-node-path :class 'node-path :index 0))

(defgconstructor (make-node-path@1 :class 'node-path :index 1) (from node-path))

(defgconstructor (make-node-path@2 :class 'node-path :index 2) (from string))

(defgconstructor (make-rid :class 'rid :index 0))

(defgconstructor (make-rid@1 :class 'rid :index 1) (from rid))

(defgconstructor (make-callable :class 'callable :index 0))

(defgconstructor (make-callable@1 :class 'callable :index 1) (from callable))

(defgconstructor (make-callable@2 :class 'callable :index 2) (object object)
 (method string-name))

(defgconstructor (make-signal :class 'signal :index 0))

(defgconstructor (make-signal@1 :class 'signal :index 1) (from signal))

(defgconstructor (make-signal@2 :class 'signal :index 2) (object object)
 (signal string-name))

(defgconstructor (make-dictionary :class 'dictionary :index 0))

(defgconstructor (make-dictionary@1 :class 'dictionary :index 1)
 (from dictionary))

(defgconstructor (make-dictionary@2 :class 'dictionary :index 2)
 (base dictionary) (key-type int) (key-class-name string-name)
 (key-script variant) (value-type int) (value-class-name string-name)
 (value-script variant))

(defgconstructor (make-array :class 'array :index 0))

(defgconstructor (make-array@1 :class 'array :index 1) (from array))

(defgconstructor (make-array@2 :class 'array :index 2) (base array) (type int)
 (class-name string-name) (script variant))

(defgconstructor (make-array@3 :class 'array :index 3) (from packed-byte-array))

(defgconstructor (make-array@4 :class 'array :index 4)
 (from packed-int-32array))

(defgconstructor (make-array@5 :class 'array :index 5)
 (from packed-int-64array))

(defgconstructor (make-array@6 :class 'array :index 6)
 (from packed-float-32array))

(defgconstructor (make-array@7 :class 'array :index 7)
 (from packed-float-64array))

(defgconstructor (make-array@8 :class 'array :index 8)
 (from packed-string-array))

(defgconstructor (make-array@9 :class 'array :index 9)
 (from packed-vector-2array))

(defgconstructor (make-array@10 :class 'array :index 10)
 (from packed-vector-3array))

(defgconstructor (make-array@11 :class 'array :index 11)
 (from packed-color-array))

(defgconstructor (make-array@12 :class 'array :index 12)
 (from packed-vector-4array))

(defgconstructor (make-packed-byte-array :class 'packed-byte-array :index 0))

(defgconstructor (make-packed-byte-array@1 :class 'packed-byte-array :index 1)
 (from packed-byte-array))

(defgconstructor (make-packed-byte-array@2 :class 'packed-byte-array :index 2)
 (from array))

(defgconstructor (make-packed-int-32array :class 'packed-int-32array :index 0))

(defgconstructor
 (make-packed-int-32array@1 :class 'packed-int-32array :index 1)
 (from packed-int-32array))

(defgconstructor
 (make-packed-int-32array@2 :class 'packed-int-32array :index 2) (from array))

(defgconstructor (make-packed-int-64array :class 'packed-int-64array :index 0))

(defgconstructor
 (make-packed-int-64array@1 :class 'packed-int-64array :index 1)
 (from packed-int-64array))

(defgconstructor
 (make-packed-int-64array@2 :class 'packed-int-64array :index 2) (from array))

(defgconstructor
 (make-packed-float-32array :class 'packed-float-32array :index 0))

(defgconstructor
 (make-packed-float-32array@1 :class 'packed-float-32array :index 1)
 (from packed-float-32array))

(defgconstructor
 (make-packed-float-32array@2 :class 'packed-float-32array :index 2)
 (from array))

(defgconstructor
 (make-packed-float-64array :class 'packed-float-64array :index 0))

(defgconstructor
 (make-packed-float-64array@1 :class 'packed-float-64array :index 1)
 (from packed-float-64array))

(defgconstructor
 (make-packed-float-64array@2 :class 'packed-float-64array :index 2)
 (from array))

(defgconstructor
 (make-packed-string-array :class 'packed-string-array :index 0))

(defgconstructor
 (make-packed-string-array@1 :class 'packed-string-array :index 1)
 (from packed-string-array))

(defgconstructor
 (make-packed-string-array@2 :class 'packed-string-array :index 2) (from array))

(defgconstructor
 (make-packed-vector-2array :class 'packed-vector-2array :index 0))

(defgconstructor
 (make-packed-vector-2array@1 :class 'packed-vector-2array :index 1)
 (from packed-vector-2array))

(defgconstructor
 (make-packed-vector-2array@2 :class 'packed-vector-2array :index 2)
 (from array))

(defgconstructor
 (make-packed-vector-3array :class 'packed-vector-3array :index 0))

(defgconstructor
 (make-packed-vector-3array@1 :class 'packed-vector-3array :index 1)
 (from packed-vector-3array))

(defgconstructor
 (make-packed-vector-3array@2 :class 'packed-vector-3array :index 2)
 (from array))

(defgconstructor (make-packed-color-array :class 'packed-color-array :index 0))

(defgconstructor
 (make-packed-color-array@1 :class 'packed-color-array :index 1)
 (from packed-color-array))

(defgconstructor
 (make-packed-color-array@2 :class 'packed-color-array :index 2) (from array))

(defgconstructor
 (make-packed-vector-4array :class 'packed-vector-4array :index 0))

(defgconstructor
 (make-packed-vector-4array@1 :class 'packed-vector-4array :index 1)
 (from packed-vector-4array))

(defgconstructor
 (make-packed-vector-4array@2 :class 'packed-vector-4array :index 2)
 (from array))