(common-lisp:in-package :%godot)


(defgmethod
 (csgmesh-3d+set-mesh :class 'csgmesh-3d :bind "set_mesh" :hash 194775623)
 :void (mesh mesh))

(defgmethod
 (csgmesh-3d+get-mesh :class 'csgmesh-3d :bind "get_mesh" :hash 4081188045)
 mesh)

(defgmethod
 (csgmesh-3d+set-material :class 'csgmesh-3d :bind "set_material" :hash
  2757459619)
 :void (material material))

(defgmethod
 (csgmesh-3d+get-material :class 'csgmesh-3d :bind "get_material" :hash
  5934680)
 material)