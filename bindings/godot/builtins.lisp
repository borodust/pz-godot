(common-lisp:in-package :%godot)


(defgclass
 (variant :bind "Variant" :api :builtin :size 24 :instantiable common-lisp:nil))

(defgclass
 (nil :bind "Nil" :api :builtin :size 0 :instantiable common-lisp:nil))

(defgclass
 (bool :bind "bool" :api :builtin :size 1 :instantiable common-lisp:nil))

(defgclass
 (int :bind "int" :api :builtin :size 8 :instantiable common-lisp:nil))

(defgclass
 (float :bind "float" :api :builtin :size 8 :instantiable common-lisp:nil))

(defgclass
 (string :bind "String" :api :builtin :size 8 :instantiable common-lisp:nil))

(defgclass
 (vector-2 :bind "Vector2" :api :builtin :size 8 :instantiable common-lisp:nil)
 (:fields (x float :offset 0) (y float :offset 4)))


(defgconstant +vector-2+zero+ :bind "ZERO" :class 'vector-2 :documentation
 "Vector2(0, 0)")


(defgconstant +vector-2+one+ :bind "ONE" :class 'vector-2 :documentation
 "Vector2(1, 1)")


(defgconstant +vector-2+inf+ :bind "INF" :class 'vector-2 :documentation
 "Vector2(inf, inf)")


(defgconstant +vector-2+left+ :bind "LEFT" :class 'vector-2 :documentation
 "Vector2(-1, 0)")


(defgconstant +vector-2+right+ :bind "RIGHT" :class 'vector-2 :documentation
 "Vector2(1, 0)")


(defgconstant +vector-2+up+ :bind "UP" :class 'vector-2 :documentation
 "Vector2(0, -1)")


(defgconstant +vector-2+down+ :bind "DOWN" :class 'vector-2 :documentation
 "Vector2(0, 1)")


(defgenum (vector-2+axis :class 'vector-2) (:x 0) (:y 1))

(defgclass
 (vector-2i :bind "Vector2i" :api :builtin :size 8 :instantiable
  common-lisp:nil)
 (:fields (x :int32 :offset 0) (y :int32 :offset 4)))


(defgconstant +vector-2i+zero+ :bind "ZERO" :class 'vector-2i :documentation
 "Vector2i(0, 0)")


(defgconstant +vector-2i+one+ :bind "ONE" :class 'vector-2i :documentation
 "Vector2i(1, 1)")


(defgconstant +vector-2i+min+ :bind "MIN" :class 'vector-2i :documentation
 "Vector2i(-2147483648, -2147483648)")


(defgconstant +vector-2i+max+ :bind "MAX" :class 'vector-2i :documentation
 "Vector2i(2147483647, 2147483647)")


(defgconstant +vector-2i+left+ :bind "LEFT" :class 'vector-2i :documentation
 "Vector2i(-1, 0)")


(defgconstant +vector-2i+right+ :bind "RIGHT" :class 'vector-2i :documentation
 "Vector2i(1, 0)")


(defgconstant +vector-2i+up+ :bind "UP" :class 'vector-2i :documentation
 "Vector2i(0, -1)")


(defgconstant +vector-2i+down+ :bind "DOWN" :class 'vector-2i :documentation
 "Vector2i(0, 1)")


(defgenum (vector-2i+axis :class 'vector-2i) (:x 0) (:y 1))

(defgclass
 (rect-2 :bind "Rect2" :api :builtin :size 16 :instantiable common-lisp:nil)
 (:fields (position vector-2 :offset 0) (size vector-2 :offset 8)))

(defgclass
 (rect-2i :bind "Rect2i" :api :builtin :size 16 :instantiable common-lisp:nil)
 (:fields (position vector-2i :offset 0) (size vector-2i :offset 8)))

(defgclass
 (vector-3 :bind "Vector3" :api :builtin :size 12 :instantiable
  common-lisp:nil)
 (:fields (x float :offset 0) (y float :offset 4) (z float :offset 8)))


(defgconstant +vector-3+zero+ :bind "ZERO" :class 'vector-3 :documentation
 "Vector3(0, 0, 0)")


(defgconstant +vector-3+one+ :bind "ONE" :class 'vector-3 :documentation
 "Vector3(1, 1, 1)")


(defgconstant +vector-3+inf+ :bind "INF" :class 'vector-3 :documentation
 "Vector3(inf, inf, inf)")


(defgconstant +vector-3+left+ :bind "LEFT" :class 'vector-3 :documentation
 "Vector3(-1, 0, 0)")


(defgconstant +vector-3+right+ :bind "RIGHT" :class 'vector-3 :documentation
 "Vector3(1, 0, 0)")


(defgconstant +vector-3+up+ :bind "UP" :class 'vector-3 :documentation
 "Vector3(0, 1, 0)")


(defgconstant +vector-3+down+ :bind "DOWN" :class 'vector-3 :documentation
 "Vector3(0, -1, 0)")


(defgconstant +vector-3+forward+ :bind "FORWARD" :class 'vector-3
 :documentation "Vector3(0, 0, -1)")


(defgconstant +vector-3+back+ :bind "BACK" :class 'vector-3 :documentation
 "Vector3(0, 0, 1)")


(defgconstant +vector-3+model-left+ :bind "MODEL_LEFT" :class 'vector-3
 :documentation "Vector3(1, 0, 0)")


(defgconstant +vector-3+model-right+ :bind "MODEL_RIGHT" :class 'vector-3
 :documentation "Vector3(-1, 0, 0)")


(defgconstant +vector-3+model-top+ :bind "MODEL_TOP" :class 'vector-3
 :documentation "Vector3(0, 1, 0)")


(defgconstant +vector-3+model-bottom+ :bind "MODEL_BOTTOM" :class 'vector-3
 :documentation "Vector3(0, -1, 0)")


(defgconstant +vector-3+model-front+ :bind "MODEL_FRONT" :class 'vector-3
 :documentation "Vector3(0, 0, 1)")


(defgconstant +vector-3+model-rear+ :bind "MODEL_REAR" :class 'vector-3
 :documentation "Vector3(0, 0, -1)")


(defgenum (vector-3+axis :class 'vector-3) (:x 0) (:y 1) (:z 2))

(defgclass
 (vector-3i :bind "Vector3i" :api :builtin :size 12 :instantiable
  common-lisp:nil)
 (:fields (x :int32 :offset 0) (y :int32 :offset 4) (z :int32 :offset 8)))


(defgconstant +vector-3i+zero+ :bind "ZERO" :class 'vector-3i :documentation
 "Vector3i(0, 0, 0)")


(defgconstant +vector-3i+one+ :bind "ONE" :class 'vector-3i :documentation
 "Vector3i(1, 1, 1)")


(defgconstant +vector-3i+min+ :bind "MIN" :class 'vector-3i :documentation
 "Vector3i(-2147483648, -2147483648, -2147483648)")


(defgconstant +vector-3i+max+ :bind "MAX" :class 'vector-3i :documentation
 "Vector3i(2147483647, 2147483647, 2147483647)")


(defgconstant +vector-3i+left+ :bind "LEFT" :class 'vector-3i :documentation
 "Vector3i(-1, 0, 0)")


(defgconstant +vector-3i+right+ :bind "RIGHT" :class 'vector-3i :documentation
 "Vector3i(1, 0, 0)")


(defgconstant +vector-3i+up+ :bind "UP" :class 'vector-3i :documentation
 "Vector3i(0, 1, 0)")


(defgconstant +vector-3i+down+ :bind "DOWN" :class 'vector-3i :documentation
 "Vector3i(0, -1, 0)")


(defgconstant +vector-3i+forward+ :bind "FORWARD" :class 'vector-3i
 :documentation "Vector3i(0, 0, -1)")


(defgconstant +vector-3i+back+ :bind "BACK" :class 'vector-3i :documentation
 "Vector3i(0, 0, 1)")


(defgenum (vector-3i+axis :class 'vector-3i) (:x 0) (:y 1) (:z 2))

(defgclass
 (transform-2d :bind "Transform2D" :api :builtin :size 24 :instantiable
  common-lisp:nil)
 (:fields (x vector-2 :offset 0) (y vector-2 :offset 8)
  (origin vector-2 :offset 16)))


(defgconstant +transform-2d+identity+ :bind "IDENTITY" :class 'transform-2d
 :documentation "Transform2D(1, 0, 0, 1, 0, 0)")


(defgconstant +transform-2d+flip-x+ :bind "FLIP_X" :class 'transform-2d
 :documentation "Transform2D(-1, 0, 0, 1, 0, 0)")


(defgconstant +transform-2d+flip-y+ :bind "FLIP_Y" :class 'transform-2d
 :documentation "Transform2D(1, 0, 0, -1, 0, 0)")

(defgclass
 (vector-4 :bind "Vector4" :api :builtin :size 16 :instantiable
  common-lisp:nil)
 (:fields (x float :offset 0) (y float :offset 4) (z float :offset 8)
  (w float :offset 12)))


(defgconstant +vector-4+zero+ :bind "ZERO" :class 'vector-4 :documentation
 "Vector4(0, 0, 0, 0)")


(defgconstant +vector-4+one+ :bind "ONE" :class 'vector-4 :documentation
 "Vector4(1, 1, 1, 1)")


(defgconstant +vector-4+inf+ :bind "INF" :class 'vector-4 :documentation
 "Vector4(inf, inf, inf, inf)")


(defgenum (vector-4+axis :class 'vector-4) (:x 0) (:y 1) (:z 2) (:w 3))

(defgclass
 (vector-4i :bind "Vector4i" :api :builtin :size 16 :instantiable
  common-lisp:nil)
 (:fields (x :int32 :offset 0) (y :int32 :offset 4) (z :int32 :offset 8)
  (w :int32 :offset 12)))


(defgconstant +vector-4i+zero+ :bind "ZERO" :class 'vector-4i :documentation
 "Vector4i(0, 0, 0, 0)")


(defgconstant +vector-4i+one+ :bind "ONE" :class 'vector-4i :documentation
 "Vector4i(1, 1, 1, 1)")


(defgconstant +vector-4i+min+ :bind "MIN" :class 'vector-4i :documentation
 "Vector4i(-2147483648, -2147483648, -2147483648, -2147483648)")


(defgconstant +vector-4i+max+ :bind "MAX" :class 'vector-4i :documentation
 "Vector4i(2147483647, 2147483647, 2147483647, 2147483647)")


(defgenum (vector-4i+axis :class 'vector-4i) (:x 0) (:y 1) (:z 2) (:w 3))

(defgclass
 (plane :bind "Plane" :api :builtin :size 16 :instantiable common-lisp:nil)
 (:fields (normal vector-3 :offset 0) (d float :offset 12)))


(defgconstant +plane+plane-yz+ :bind "PLANE_YZ" :class 'plane :documentation
 "Plane(1, 0, 0, 0)")


(defgconstant +plane+plane-xz+ :bind "PLANE_XZ" :class 'plane :documentation
 "Plane(0, 1, 0, 0)")


(defgconstant +plane+plane-xy+ :bind "PLANE_XY" :class 'plane :documentation
 "Plane(0, 0, 1, 0)")

(defgclass
 (quaternion :bind "Quaternion" :api :builtin :size 16 :instantiable
  common-lisp:nil)
 (:fields (x float :offset 0) (y float :offset 4) (z float :offset 8)
  (w float :offset 12)))


(defgconstant +quaternion+identity+ :bind "IDENTITY" :class 'quaternion
 :documentation "Quaternion(0, 0, 0, 1)")

(defgclass
 (aabb :bind "AABB" :api :builtin :size 24 :instantiable common-lisp:nil)
 (:fields (position vector-3 :offset 0) (size vector-3 :offset 12)))

(defgclass
 (basis :bind "Basis" :api :builtin :size 36 :instantiable common-lisp:nil)
 (:fields (x vector-3 :offset 0) (y vector-3 :offset 12)
  (z vector-3 :offset 24)))


(defgconstant +basis+identity+ :bind "IDENTITY" :class 'basis :documentation
 "Basis(1, 0, 0, 0, 1, 0, 0, 0, 1)")


(defgconstant +basis+flip-x+ :bind "FLIP_X" :class 'basis :documentation
 "Basis(-1, 0, 0, 0, 1, 0, 0, 0, 1)")


(defgconstant +basis+flip-y+ :bind "FLIP_Y" :class 'basis :documentation
 "Basis(1, 0, 0, 0, -1, 0, 0, 0, 1)")


(defgconstant +basis+flip-z+ :bind "FLIP_Z" :class 'basis :documentation
 "Basis(1, 0, 0, 0, 1, 0, 0, 0, -1)")

(defgclass
 (transform-3d :bind "Transform3D" :api :builtin :size 48 :instantiable
  common-lisp:nil)
 (:fields (basis basis :offset 0) (origin vector-3 :offset 36)))


(defgconstant +transform-3d+identity+ :bind "IDENTITY" :class 'transform-3d
 :documentation "Transform3D(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0)")


(defgconstant +transform-3d+flip-x+ :bind "FLIP_X" :class 'transform-3d
 :documentation "Transform3D(-1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0)")


(defgconstant +transform-3d+flip-y+ :bind "FLIP_Y" :class 'transform-3d
 :documentation "Transform3D(1, 0, 0, 0, -1, 0, 0, 0, 1, 0, 0, 0)")


(defgconstant +transform-3d+flip-z+ :bind "FLIP_Z" :class 'transform-3d
 :documentation "Transform3D(1, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0)")

(defgclass
 (projection :bind "Projection" :api :builtin :size 64 :instantiable
  common-lisp:nil)
 (:fields (x vector-4 :offset 0) (y vector-4 :offset 16)
  (z vector-4 :offset 32) (w vector-4 :offset 48)))


(defgconstant +projection+identity+ :bind "IDENTITY" :class 'projection
 :documentation "Projection(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)")


(defgconstant +projection+zero+ :bind "ZERO" :class 'projection :documentation
 "Projection(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)")


(defgenum (projection+planes :class 'projection) (:near 0) (:far 1) (:left 2)
 (:top 3) (:right 4) (:bottom 5))

(defgclass
 (color :bind "Color" :api :builtin :size 16 :instantiable common-lisp:nil)
 (:fields (r float :offset 0) (g float :offset 4) (b float :offset 8)
  (a float :offset 12)))


(defgconstant +color+alice-blue+ :bind "ALICE_BLUE" :class 'color
 :documentation "Color(0.9411765, 0.972549, 1, 1)")


(defgconstant +color+antique-white+ :bind "ANTIQUE_WHITE" :class 'color
 :documentation "Color(0.98039216, 0.92156863, 0.84313726, 1)")


(defgconstant +color+aqua+ :bind "AQUA" :class 'color :documentation
 "Color(0, 1, 1, 1)")


(defgconstant +color+aquamarine+ :bind "AQUAMARINE" :class 'color
 :documentation "Color(0.49803922, 1, 0.83137256, 1)")


(defgconstant +color+azure+ :bind "AZURE" :class 'color :documentation
 "Color(0.9411765, 1, 1, 1)")


(defgconstant +color+beige+ :bind "BEIGE" :class 'color :documentation
 "Color(0.9607843, 0.9607843, 0.8627451, 1)")


(defgconstant +color+bisque+ :bind "BISQUE" :class 'color :documentation
 "Color(1, 0.89411765, 0.76862746, 1)")


(defgconstant +color+black+ :bind "BLACK" :class 'color :documentation
 "Color(0, 0, 0, 1)")


(defgconstant +color+blanched-almond+ :bind "BLANCHED_ALMOND" :class 'color
 :documentation "Color(1, 0.92156863, 0.8039216, 1)")


(defgconstant +color+blue+ :bind "BLUE" :class 'color :documentation
 "Color(0, 0, 1, 1)")


(defgconstant +color+blue-violet+ :bind "BLUE_VIOLET" :class 'color
 :documentation "Color(0.5411765, 0.16862746, 0.8862745, 1)")


(defgconstant +color+brown+ :bind "BROWN" :class 'color :documentation
 "Color(0.64705884, 0.16470589, 0.16470589, 1)")


(defgconstant +color+burlywood+ :bind "BURLYWOOD" :class 'color :documentation
 "Color(0.87058824, 0.72156864, 0.5294118, 1)")


(defgconstant +color+cadet-blue+ :bind "CADET_BLUE" :class 'color
 :documentation "Color(0.37254903, 0.61960787, 0.627451, 1)")


(defgconstant +color+chartreuse+ :bind "CHARTREUSE" :class 'color
 :documentation "Color(0.49803922, 1, 0, 1)")


(defgconstant +color+chocolate+ :bind "CHOCOLATE" :class 'color :documentation
 "Color(0.8235294, 0.4117647, 0.11764706, 1)")


(defgconstant +color+coral+ :bind "CORAL" :class 'color :documentation
 "Color(1, 0.49803922, 0.3137255, 1)")


(defgconstant +color+cornflower-blue+ :bind "CORNFLOWER_BLUE" :class 'color
 :documentation "Color(0.39215687, 0.58431375, 0.92941177, 1)")


(defgconstant +color+cornsilk+ :bind "CORNSILK" :class 'color :documentation
 "Color(1, 0.972549, 0.8627451, 1)")


(defgconstant +color+crimson+ :bind "CRIMSON" :class 'color :documentation
 "Color(0.8627451, 0.078431375, 0.23529412, 1)")


(defgconstant +color+cyan+ :bind "CYAN" :class 'color :documentation
 "Color(0, 1, 1, 1)")


(defgconstant +color+dark-blue+ :bind "DARK_BLUE" :class 'color :documentation
 "Color(0, 0, 0.54509807, 1)")


(defgconstant +color+dark-cyan+ :bind "DARK_CYAN" :class 'color :documentation
 "Color(0, 0.54509807, 0.54509807, 1)")


(defgconstant +color+dark-goldenrod+ :bind "DARK_GOLDENROD" :class 'color
 :documentation "Color(0.72156864, 0.5254902, 0.043137256, 1)")


(defgconstant +color+dark-gray+ :bind "DARK_GRAY" :class 'color :documentation
 "Color(0.6627451, 0.6627451, 0.6627451, 1)")


(defgconstant +color+dark-green+ :bind "DARK_GREEN" :class 'color
 :documentation "Color(0, 0.39215687, 0, 1)")


(defgconstant +color+dark-khaki+ :bind "DARK_KHAKI" :class 'color
 :documentation "Color(0.7411765, 0.7176471, 0.41960785, 1)")


(defgconstant +color+dark-magenta+ :bind "DARK_MAGENTA" :class 'color
 :documentation "Color(0.54509807, 0, 0.54509807, 1)")


(defgconstant +color+dark-olive-green+ :bind "DARK_OLIVE_GREEN" :class 'color
 :documentation "Color(0.33333334, 0.41960785, 0.18431373, 1)")


(defgconstant +color+dark-orange+ :bind "DARK_ORANGE" :class 'color
 :documentation "Color(1, 0.54901963, 0, 1)")


(defgconstant +color+dark-orchid+ :bind "DARK_ORCHID" :class 'color
 :documentation "Color(0.6, 0.19607843, 0.8, 1)")


(defgconstant +color+dark-red+ :bind "DARK_RED" :class 'color :documentation
 "Color(0.54509807, 0, 0, 1)")


(defgconstant +color+dark-salmon+ :bind "DARK_SALMON" :class 'color
 :documentation "Color(0.9137255, 0.5882353, 0.47843137, 1)")


(defgconstant +color+dark-sea-green+ :bind "DARK_SEA_GREEN" :class 'color
 :documentation "Color(0.56078434, 0.7372549, 0.56078434, 1)")


(defgconstant +color+dark-slate-blue+ :bind "DARK_SLATE_BLUE" :class 'color
 :documentation "Color(0.28235295, 0.23921569, 0.54509807, 1)")


(defgconstant +color+dark-slate-gray+ :bind "DARK_SLATE_GRAY" :class 'color
 :documentation "Color(0.18431373, 0.30980393, 0.30980393, 1)")


(defgconstant +color+dark-turquoise+ :bind "DARK_TURQUOISE" :class 'color
 :documentation "Color(0, 0.80784315, 0.81960785, 1)")


(defgconstant +color+dark-violet+ :bind "DARK_VIOLET" :class 'color
 :documentation "Color(0.5803922, 0, 0.827451, 1)")


(defgconstant +color+deep-pink+ :bind "DEEP_PINK" :class 'color :documentation
 "Color(1, 0.078431375, 0.5764706, 1)")


(defgconstant +color+deep-sky-blue+ :bind "DEEP_SKY_BLUE" :class 'color
 :documentation "Color(0, 0.7490196, 1, 1)")


(defgconstant +color+dim-gray+ :bind "DIM_GRAY" :class 'color :documentation
 "Color(0.4117647, 0.4117647, 0.4117647, 1)")


(defgconstant +color+dodger-blue+ :bind "DODGER_BLUE" :class 'color
 :documentation "Color(0.11764706, 0.5647059, 1, 1)")


(defgconstant +color+firebrick+ :bind "FIREBRICK" :class 'color :documentation
 "Color(0.69803923, 0.13333334, 0.13333334, 1)")


(defgconstant +color+floral-white+ :bind "FLORAL_WHITE" :class 'color
 :documentation "Color(1, 0.98039216, 0.9411765, 1)")


(defgconstant +color+forest-green+ :bind "FOREST_GREEN" :class 'color
 :documentation "Color(0.13333334, 0.54509807, 0.13333334, 1)")


(defgconstant +color+fuchsia+ :bind "FUCHSIA" :class 'color :documentation
 "Color(1, 0, 1, 1)")


(defgconstant +color+gainsboro+ :bind "GAINSBORO" :class 'color :documentation
 "Color(0.8627451, 0.8627451, 0.8627451, 1)")


(defgconstant +color+ghost-white+ :bind "GHOST_WHITE" :class 'color
 :documentation "Color(0.972549, 0.972549, 1, 1)")


(defgconstant +color+gold+ :bind "GOLD" :class 'color :documentation
 "Color(1, 0.84313726, 0, 1)")


(defgconstant +color+goldenrod+ :bind "GOLDENROD" :class 'color :documentation
 "Color(0.85490197, 0.64705884, 0.1254902, 1)")


(defgconstant +color+gray+ :bind "GRAY" :class 'color :documentation
 "Color(0.74509805, 0.74509805, 0.74509805, 1)")


(defgconstant +color+green+ :bind "GREEN" :class 'color :documentation
 "Color(0, 1, 0, 1)")


(defgconstant +color+green-yellow+ :bind "GREEN_YELLOW" :class 'color
 :documentation "Color(0.6784314, 1, 0.18431373, 1)")


(defgconstant +color+honeydew+ :bind "HONEYDEW" :class 'color :documentation
 "Color(0.9411765, 1, 0.9411765, 1)")


(defgconstant +color+hot-pink+ :bind "HOT_PINK" :class 'color :documentation
 "Color(1, 0.4117647, 0.7058824, 1)")


(defgconstant +color+indian-red+ :bind "INDIAN_RED" :class 'color
 :documentation "Color(0.8039216, 0.36078432, 0.36078432, 1)")


(defgconstant +color+indigo+ :bind "INDIGO" :class 'color :documentation
 "Color(0.29411766, 0, 0.50980395, 1)")


(defgconstant +color+ivory+ :bind "IVORY" :class 'color :documentation
 "Color(1, 1, 0.9411765, 1)")


(defgconstant +color+khaki+ :bind "KHAKI" :class 'color :documentation
 "Color(0.9411765, 0.9019608, 0.54901963, 1)")


(defgconstant +color+lavender+ :bind "LAVENDER" :class 'color :documentation
 "Color(0.9019608, 0.9019608, 0.98039216, 1)")


(defgconstant +color+lavender-blush+ :bind "LAVENDER_BLUSH" :class 'color
 :documentation "Color(1, 0.9411765, 0.9607843, 1)")


(defgconstant +color+lawn-green+ :bind "LAWN_GREEN" :class 'color
 :documentation "Color(0.4862745, 0.9882353, 0, 1)")


(defgconstant +color+lemon-chiffon+ :bind "LEMON_CHIFFON" :class 'color
 :documentation "Color(1, 0.98039216, 0.8039216, 1)")


(defgconstant +color+light-blue+ :bind "LIGHT_BLUE" :class 'color
 :documentation "Color(0.6784314, 0.84705883, 0.9019608, 1)")


(defgconstant +color+light-coral+ :bind "LIGHT_CORAL" :class 'color
 :documentation "Color(0.9411765, 0.5019608, 0.5019608, 1)")


(defgconstant +color+light-cyan+ :bind "LIGHT_CYAN" :class 'color
 :documentation "Color(0.8784314, 1, 1, 1)")


(defgconstant +color+light-goldenrod+ :bind "LIGHT_GOLDENROD" :class 'color
 :documentation "Color(0.98039216, 0.98039216, 0.8235294, 1)")


(defgconstant +color+light-gray+ :bind "LIGHT_GRAY" :class 'color
 :documentation "Color(0.827451, 0.827451, 0.827451, 1)")


(defgconstant +color+light-green+ :bind "LIGHT_GREEN" :class 'color
 :documentation "Color(0.5647059, 0.93333334, 0.5647059, 1)")


(defgconstant +color+light-pink+ :bind "LIGHT_PINK" :class 'color
 :documentation "Color(1, 0.7137255, 0.75686276, 1)")


(defgconstant +color+light-salmon+ :bind "LIGHT_SALMON" :class 'color
 :documentation "Color(1, 0.627451, 0.47843137, 1)")


(defgconstant +color+light-sea-green+ :bind "LIGHT_SEA_GREEN" :class 'color
 :documentation "Color(0.1254902, 0.69803923, 0.6666667, 1)")


(defgconstant +color+light-sky-blue+ :bind "LIGHT_SKY_BLUE" :class 'color
 :documentation "Color(0.5294118, 0.80784315, 0.98039216, 1)")


(defgconstant +color+light-slate-gray+ :bind "LIGHT_SLATE_GRAY" :class 'color
 :documentation "Color(0.46666667, 0.53333336, 0.6, 1)")


(defgconstant +color+light-steel-blue+ :bind "LIGHT_STEEL_BLUE" :class 'color
 :documentation "Color(0.6901961, 0.76862746, 0.87058824, 1)")


(defgconstant +color+light-yellow+ :bind "LIGHT_YELLOW" :class 'color
 :documentation "Color(1, 1, 0.8784314, 1)")


(defgconstant +color+lime+ :bind "LIME" :class 'color :documentation
 "Color(0, 1, 0, 1)")


(defgconstant +color+lime-green+ :bind "LIME_GREEN" :class 'color
 :documentation "Color(0.19607843, 0.8039216, 0.19607843, 1)")


(defgconstant +color+linen+ :bind "LINEN" :class 'color :documentation
 "Color(0.98039216, 0.9411765, 0.9019608, 1)")


(defgconstant +color+magenta+ :bind "MAGENTA" :class 'color :documentation
 "Color(1, 0, 1, 1)")


(defgconstant +color+maroon+ :bind "MAROON" :class 'color :documentation
 "Color(0.6901961, 0.1882353, 0.3764706, 1)")


(defgconstant +color+medium-aquamarine+ :bind "MEDIUM_AQUAMARINE" :class 'color
 :documentation "Color(0.4, 0.8039216, 0.6666667, 1)")


(defgconstant +color+medium-blue+ :bind "MEDIUM_BLUE" :class 'color
 :documentation "Color(0, 0, 0.8039216, 1)")


(defgconstant +color+medium-orchid+ :bind "MEDIUM_ORCHID" :class 'color
 :documentation "Color(0.7294118, 0.33333334, 0.827451, 1)")


(defgconstant +color+medium-purple+ :bind "MEDIUM_PURPLE" :class 'color
 :documentation "Color(0.5764706, 0.4392157, 0.85882354, 1)")


(defgconstant +color+medium-sea-green+ :bind "MEDIUM_SEA_GREEN" :class 'color
 :documentation "Color(0.23529412, 0.7019608, 0.44313726, 1)")


(defgconstant +color+medium-slate-blue+ :bind "MEDIUM_SLATE_BLUE" :class 'color
 :documentation "Color(0.48235294, 0.40784314, 0.93333334, 1)")


(defgconstant +color+medium-spring-green+ :bind "MEDIUM_SPRING_GREEN" :class
 'color :documentation "Color(0, 0.98039216, 0.6039216, 1)")


(defgconstant +color+medium-turquoise+ :bind "MEDIUM_TURQUOISE" :class 'color
 :documentation "Color(0.28235295, 0.81960785, 0.8, 1)")


(defgconstant +color+medium-violet-red+ :bind "MEDIUM_VIOLET_RED" :class 'color
 :documentation "Color(0.78039217, 0.08235294, 0.52156866, 1)")


(defgconstant +color+midnight-blue+ :bind "MIDNIGHT_BLUE" :class 'color
 :documentation "Color(0.09803922, 0.09803922, 0.4392157, 1)")


(defgconstant +color+mint-cream+ :bind "MINT_CREAM" :class 'color
 :documentation "Color(0.9607843, 1, 0.98039216, 1)")


(defgconstant +color+misty-rose+ :bind "MISTY_ROSE" :class 'color
 :documentation "Color(1, 0.89411765, 0.88235295, 1)")


(defgconstant +color+moccasin+ :bind "MOCCASIN" :class 'color :documentation
 "Color(1, 0.89411765, 0.70980394, 1)")


(defgconstant +color+navajo-white+ :bind "NAVAJO_WHITE" :class 'color
 :documentation "Color(1, 0.87058824, 0.6784314, 1)")


(defgconstant +color+navy-blue+ :bind "NAVY_BLUE" :class 'color :documentation
 "Color(0, 0, 0.5019608, 1)")


(defgconstant +color+old-lace+ :bind "OLD_LACE" :class 'color :documentation
 "Color(0.99215686, 0.9607843, 0.9019608, 1)")


(defgconstant +color+olive+ :bind "OLIVE" :class 'color :documentation
 "Color(0.5019608, 0.5019608, 0, 1)")


(defgconstant +color+olive-drab+ :bind "OLIVE_DRAB" :class 'color
 :documentation "Color(0.41960785, 0.5568628, 0.13725491, 1)")


(defgconstant +color+orange+ :bind "ORANGE" :class 'color :documentation
 "Color(1, 0.64705884, 0, 1)")


(defgconstant +color+orange-red+ :bind "ORANGE_RED" :class 'color
 :documentation "Color(1, 0.27058825, 0, 1)")


(defgconstant +color+orchid+ :bind "ORCHID" :class 'color :documentation
 "Color(0.85490197, 0.4392157, 0.8392157, 1)")


(defgconstant +color+pale-goldenrod+ :bind "PALE_GOLDENROD" :class 'color
 :documentation "Color(0.93333334, 0.9098039, 0.6666667, 1)")


(defgconstant +color+pale-green+ :bind "PALE_GREEN" :class 'color
 :documentation "Color(0.59607846, 0.9843137, 0.59607846, 1)")


(defgconstant +color+pale-turquoise+ :bind "PALE_TURQUOISE" :class 'color
 :documentation "Color(0.6862745, 0.93333334, 0.93333334, 1)")


(defgconstant +color+pale-violet-red+ :bind "PALE_VIOLET_RED" :class 'color
 :documentation "Color(0.85882354, 0.4392157, 0.5764706, 1)")


(defgconstant +color+papaya-whip+ :bind "PAPAYA_WHIP" :class 'color
 :documentation "Color(1, 0.9372549, 0.8352941, 1)")


(defgconstant +color+peach-puff+ :bind "PEACH_PUFF" :class 'color
 :documentation "Color(1, 0.85490197, 0.7254902, 1)")


(defgconstant +color+peru+ :bind "PERU" :class 'color :documentation
 "Color(0.8039216, 0.52156866, 0.24705882, 1)")


(defgconstant +color+pink+ :bind "PINK" :class 'color :documentation
 "Color(1, 0.7529412, 0.79607844, 1)")


(defgconstant +color+plum+ :bind "PLUM" :class 'color :documentation
 "Color(0.8666667, 0.627451, 0.8666667, 1)")


(defgconstant +color+powder-blue+ :bind "POWDER_BLUE" :class 'color
 :documentation "Color(0.6901961, 0.8784314, 0.9019608, 1)")


(defgconstant +color+purple+ :bind "PURPLE" :class 'color :documentation
 "Color(0.627451, 0.1254902, 0.9411765, 1)")


(defgconstant +color+rebecca-purple+ :bind "REBECCA_PURPLE" :class 'color
 :documentation "Color(0.4, 0.2, 0.6, 1)")


(defgconstant +color+red+ :bind "RED" :class 'color :documentation
 "Color(1, 0, 0, 1)")


(defgconstant +color+rosy-brown+ :bind "ROSY_BROWN" :class 'color
 :documentation "Color(0.7372549, 0.56078434, 0.56078434, 1)")


(defgconstant +color+royal-blue+ :bind "ROYAL_BLUE" :class 'color
 :documentation "Color(0.25490198, 0.4117647, 0.88235295, 1)")


(defgconstant +color+saddle-brown+ :bind "SADDLE_BROWN" :class 'color
 :documentation "Color(0.54509807, 0.27058825, 0.07450981, 1)")


(defgconstant +color+salmon+ :bind "SALMON" :class 'color :documentation
 "Color(0.98039216, 0.5019608, 0.44705883, 1)")


(defgconstant +color+sandy-brown+ :bind "SANDY_BROWN" :class 'color
 :documentation "Color(0.95686275, 0.6431373, 0.3764706, 1)")


(defgconstant +color+sea-green+ :bind "SEA_GREEN" :class 'color :documentation
 "Color(0.18039216, 0.54509807, 0.34117648, 1)")


(defgconstant +color+seashell+ :bind "SEASHELL" :class 'color :documentation
 "Color(1, 0.9607843, 0.93333334, 1)")


(defgconstant +color+sienna+ :bind "SIENNA" :class 'color :documentation
 "Color(0.627451, 0.32156864, 0.1764706, 1)")


(defgconstant +color+silver+ :bind "SILVER" :class 'color :documentation
 "Color(0.7529412, 0.7529412, 0.7529412, 1)")


(defgconstant +color+sky-blue+ :bind "SKY_BLUE" :class 'color :documentation
 "Color(0.5294118, 0.80784315, 0.92156863, 1)")


(defgconstant +color+slate-blue+ :bind "SLATE_BLUE" :class 'color
 :documentation "Color(0.41568628, 0.3529412, 0.8039216, 1)")


(defgconstant +color+slate-gray+ :bind "SLATE_GRAY" :class 'color
 :documentation "Color(0.4392157, 0.5019608, 0.5647059, 1)")


(defgconstant +color+snow+ :bind "SNOW" :class 'color :documentation
 "Color(1, 0.98039216, 0.98039216, 1)")


(defgconstant +color+spring-green+ :bind "SPRING_GREEN" :class 'color
 :documentation "Color(0, 1, 0.49803922, 1)")


(defgconstant +color+steel-blue+ :bind "STEEL_BLUE" :class 'color
 :documentation "Color(0.27450982, 0.50980395, 0.7058824, 1)")


(defgconstant +color+tan+ :bind "TAN" :class 'color :documentation
 "Color(0.8235294, 0.7058824, 0.54901963, 1)")


(defgconstant +color+teal+ :bind "TEAL" :class 'color :documentation
 "Color(0, 0.5019608, 0.5019608, 1)")


(defgconstant +color+thistle+ :bind "THISTLE" :class 'color :documentation
 "Color(0.84705883, 0.7490196, 0.84705883, 1)")


(defgconstant +color+tomato+ :bind "TOMATO" :class 'color :documentation
 "Color(1, 0.3882353, 0.2784314, 1)")


(defgconstant +color+transparent+ :bind "TRANSPARENT" :class 'color
 :documentation "Color(1, 1, 1, 0)")


(defgconstant +color+turquoise+ :bind "TURQUOISE" :class 'color :documentation
 "Color(0.2509804, 0.8784314, 0.8156863, 1)")


(defgconstant +color+violet+ :bind "VIOLET" :class 'color :documentation
 "Color(0.93333334, 0.50980395, 0.93333334, 1)")


(defgconstant +color+web-gray+ :bind "WEB_GRAY" :class 'color :documentation
 "Color(0.5019608, 0.5019608, 0.5019608, 1)")


(defgconstant +color+web-green+ :bind "WEB_GREEN" :class 'color :documentation
 "Color(0, 0.5019608, 0, 1)")


(defgconstant +color+web-maroon+ :bind "WEB_MAROON" :class 'color
 :documentation "Color(0.5019608, 0, 0, 1)")


(defgconstant +color+web-purple+ :bind "WEB_PURPLE" :class 'color
 :documentation "Color(0.5019608, 0, 0.5019608, 1)")


(defgconstant +color+wheat+ :bind "WHEAT" :class 'color :documentation
 "Color(0.9607843, 0.87058824, 0.7019608, 1)")


(defgconstant +color+white+ :bind "WHITE" :class 'color :documentation
 "Color(1, 1, 1, 1)")


(defgconstant +color+white-smoke+ :bind "WHITE_SMOKE" :class 'color
 :documentation "Color(0.9607843, 0.9607843, 0.9607843, 1)")


(defgconstant +color+yellow+ :bind "YELLOW" :class 'color :documentation
 "Color(1, 1, 0, 1)")


(defgconstant +color+yellow-green+ :bind "YELLOW_GREEN" :class 'color
 :documentation "Color(0.6039216, 0.8039216, 0.19607843, 1)")

(defgclass
 (string-name :bind "StringName" :api :builtin :size 8 :instantiable
  common-lisp:nil))

(defgclass
 (node-path :bind "NodePath" :api :builtin :size 8 :instantiable
  common-lisp:nil))

(defgclass
 (rid :bind "RID" :api :builtin :size 8 :instantiable common-lisp:nil))

(defgclass
 (callable :bind "Callable" :api :builtin :size 16 :instantiable
  common-lisp:nil))

(defgclass
 (signal :bind "Signal" :api :builtin :size 16 :instantiable common-lisp:nil))

(defgclass
 (dictionary :bind "Dictionary" :api :builtin :size 8 :instantiable
  common-lisp:nil))

(defgclass
 (array :bind "Array" :api :builtin :size 8 :instantiable common-lisp:nil))

(defgclass
 (packed-byte-array :bind "PackedByteArray" :api :builtin :size 16
  :instantiable common-lisp:nil))

(defgclass
 (packed-int-32array :bind "PackedInt32Array" :api :builtin :size 16
  :instantiable common-lisp:nil))

(defgclass
 (packed-int-64array :bind "PackedInt64Array" :api :builtin :size 16
  :instantiable common-lisp:nil))

(defgclass
 (packed-float-32array :bind "PackedFloat32Array" :api :builtin :size 16
  :instantiable common-lisp:nil))

(defgclass
 (packed-float-64array :bind "PackedFloat64Array" :api :builtin :size 16
  :instantiable common-lisp:nil))

(defgclass
 (packed-string-array :bind "PackedStringArray" :api :builtin :size 16
  :instantiable common-lisp:nil))

(defgclass
 (packed-vector-2array :bind "PackedVector2Array" :api :builtin :size 16
  :instantiable common-lisp:nil))

(defgclass
 (packed-vector-3array :bind "PackedVector3Array" :api :builtin :size 16
  :instantiable common-lisp:nil))

(defgclass
 (packed-color-array :bind "PackedColorArray" :api :builtin :size 16
  :instantiable common-lisp:nil))

(defgclass
 (packed-vector-4array :bind "PackedVector4Array" :api :builtin :size 16
  :instantiable common-lisp:nil))