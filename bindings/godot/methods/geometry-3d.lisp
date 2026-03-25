(common-lisp:in-package :%godot)


(defgmethod
 (geometry-3d+compute-convex-mesh-points :class 'geometry-3d :bind
  "compute_convex_mesh_points" :hash 1936902142)
 packed-vector-3array (planes array))

(defgmethod
 (geometry-3d+build-box-planes :class 'geometry-3d :bind "build_box_planes"
  :hash 3622277145)
 array (extents vector-3))

(defgmethod
 (geometry-3d+build-cylinder-planes :class 'geometry-3d :bind
  "build_cylinder_planes" :hash 449920067)
 array (radius float) (height float) (sides int) (axis vector-3+axis))

(defgmethod
 (geometry-3d+build-capsule-planes :class 'geometry-3d :bind
  "build_capsule_planes" :hash 2113592876)
 array (radius float) (height float) (sides int) (lats int)
 (axis vector-3+axis))

(defgmethod
 (geometry-3d+get-closest-points-between-segments :class 'geometry-3d :bind
  "get_closest_points_between_segments" :hash 1056373962)
 packed-vector-3array (p1 vector-3) (p2 vector-3) (q1 vector-3) (q2 vector-3))

(defgmethod
 (geometry-3d+get-closest-point-to-segment :class 'geometry-3d :bind
  "get_closest_point_to_segment" :hash 2168193209)
 vector-3 (point vector-3) (s1 vector-3) (s2 vector-3))

(defgmethod
 (geometry-3d+get-closest-point-to-segment-uncapped :class 'geometry-3d :bind
  "get_closest_point_to_segment_uncapped" :hash 2168193209)
 vector-3 (point vector-3) (s1 vector-3) (s2 vector-3))

(defgmethod
 (geometry-3d+get-triangle-barycentric-coords :class 'geometry-3d :bind
  "get_triangle_barycentric_coords" :hash 1362048029)
 vector-3 (point vector-3) (a vector-3) (b vector-3) (c vector-3))

(defgmethod
 (geometry-3d+ray-intersects-triangle :class 'geometry-3d :bind
  "ray_intersects_triangle" :hash 1718655448)
 variant (from vector-3) (dir vector-3) (a vector-3) (b vector-3) (c vector-3))

(defgmethod
 (geometry-3d+segment-intersects-triangle :class 'geometry-3d :bind
  "segment_intersects_triangle" :hash 1718655448)
 variant (from vector-3) (to vector-3) (a vector-3) (b vector-3) (c vector-3))

(defgmethod
 (geometry-3d+segment-intersects-sphere :class 'geometry-3d :bind
  "segment_intersects_sphere" :hash 4080141172)
 packed-vector-3array (from vector-3) (to vector-3) (sphere-position vector-3)
 (sphere-radius float))

(defgmethod
 (geometry-3d+segment-intersects-cylinder :class 'geometry-3d :bind
  "segment_intersects_cylinder" :hash 2361316491)
 packed-vector-3array (from vector-3) (to vector-3) (height float)
 (radius float))

(defgmethod
 (geometry-3d+segment-intersects-convex :class 'geometry-3d :bind
  "segment_intersects_convex" :hash 537425332)
 packed-vector-3array (from vector-3) (to vector-3) (planes array))

(defgmethod
 (geometry-3d+clip-polygon :class 'geometry-3d :bind "clip_polygon" :hash
  2603188319)
 packed-vector-3array (points packed-vector-3array) (plane plane))

(defgmethod
 (geometry-3d+tetrahedralize-delaunay :class 'geometry-3d :bind
  "tetrahedralize_delaunay" :hash 1230191221)
 packed-int-32array (points packed-vector-3array))