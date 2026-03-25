(common-lisp:in-package :%godot)


(defgproperty iterate-ik3d+max-iterations 'iterate-ik3d :get
 'iterate-ik3d+get-max-iterations :set 'iterate-ik3d+set-max-iterations)

(defgproperty iterate-ik3d+min-distance 'iterate-ik3d :get
 'iterate-ik3d+get-min-distance :set 'iterate-ik3d+set-min-distance)

(defgproperty iterate-ik3d+angular-delta-limit 'iterate-ik3d :get
 'iterate-ik3d+get-angular-delta-limit :set
 'iterate-ik3d+set-angular-delta-limit)

(defgproperty iterate-ik3d+deterministic 'iterate-ik3d :get
 'iterate-ik3d+is-deterministic :set 'iterate-ik3d+set-deterministic)

(defgproperty iterate-ik3d+setting-count 'iterate-ik3d)