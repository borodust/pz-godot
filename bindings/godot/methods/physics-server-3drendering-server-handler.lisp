(common-lisp:in-package :%godot)


(defgmethod
 (physics-server-3drendering-server-handler+%set-vertex :class
  'physics-server-3drendering-server-handler :bind "_set_vertex" :hash
  1530502735 :virtual common-lisp:t)
 :void (vertex-id int) (vertex vector-3))

(defgmethod
 (physics-server-3drendering-server-handler+%set-normal :class
  'physics-server-3drendering-server-handler :bind "_set_normal" :hash
  1530502735 :virtual common-lisp:t)
 :void (vertex-id int) (normal vector-3))

(defgmethod
 (physics-server-3drendering-server-handler+%set-aabb :class
  'physics-server-3drendering-server-handler :bind "_set_aabb" :hash 259215842
  :virtual common-lisp:t)
 :void (aabb aabb))

(defgmethod
 (physics-server-3drendering-server-handler+set-vertex :class
  'physics-server-3drendering-server-handler :bind "set_vertex" :hash
  1530502735)
 :void (vertex-id int) (vertex vector-3))

(defgmethod
 (physics-server-3drendering-server-handler+set-normal :class
  'physics-server-3drendering-server-handler :bind "set_normal" :hash
  1530502735)
 :void (vertex-id int) (normal vector-3))

(defgmethod
 (physics-server-3drendering-server-handler+set-aabb :class
  'physics-server-3drendering-server-handler :bind "set_aabb" :hash 259215842)
 :void (aabb aabb))