(common-lisp:in-package :%godot)


(defgproperty astar-grid-2d+region 'astar-grid-2d :get
 'astar-grid-2d+get-region :set 'astar-grid-2d+set-region)

(defgproperty astar-grid-2d+size 'astar-grid-2d :get 'astar-grid-2d+get-size
 :set 'astar-grid-2d+set-size)

(defgproperty astar-grid-2d+offset 'astar-grid-2d :get
 'astar-grid-2d+get-offset :set 'astar-grid-2d+set-offset)

(defgproperty astar-grid-2d+cell-size 'astar-grid-2d :get
 'astar-grid-2d+get-cell-size :set 'astar-grid-2d+set-cell-size)

(defgproperty astar-grid-2d+cell-shape 'astar-grid-2d :get
 'astar-grid-2d+get-cell-shape :set 'astar-grid-2d+set-cell-shape)

(defgproperty astar-grid-2d+jumping-enabled 'astar-grid-2d :get
 'astar-grid-2d+is-jumping-enabled :set 'astar-grid-2d+set-jumping-enabled)

(defgproperty astar-grid-2d+default-compute-heuristic 'astar-grid-2d :get
 'astar-grid-2d+get-default-compute-heuristic :set
 'astar-grid-2d+set-default-compute-heuristic)

(defgproperty astar-grid-2d+default-estimate-heuristic 'astar-grid-2d :get
 'astar-grid-2d+get-default-estimate-heuristic :set
 'astar-grid-2d+set-default-estimate-heuristic)

(defgproperty astar-grid-2d+diagonal-mode 'astar-grid-2d :get
 'astar-grid-2d+get-diagonal-mode :set 'astar-grid-2d+set-diagonal-mode)