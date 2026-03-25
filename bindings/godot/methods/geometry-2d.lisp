(common-lisp:in-package :%godot)


(defgmethod
 (geometry-2d+is-point-in-circle :class 'geometry-2d :bind "is_point_in_circle"
  :hash 2929491703)
 bool (point vector-2) (circle-position vector-2) (circle-radius float))

(defgmethod
 (geometry-2d+segment-intersects-circle :class 'geometry-2d :bind
  "segment_intersects_circle" :hash 1356928167)
 float (segment-from vector-2) (segment-to vector-2) (circle-position vector-2)
 (circle-radius float))

(defgmethod
 (geometry-2d+segment-intersects-segment :class 'geometry-2d :bind
  "segment_intersects_segment" :hash 2058025344)
 variant (from-a vector-2) (to-a vector-2) (from-b vector-2) (to-b vector-2))

(defgmethod
 (geometry-2d+line-intersects-line :class 'geometry-2d :bind
  "line_intersects_line" :hash 2058025344)
 variant (from-a vector-2) (dir-a vector-2) (from-b vector-2) (dir-b vector-2))

(defgmethod
 (geometry-2d+get-closest-points-between-segments :class 'geometry-2d :bind
  "get_closest_points_between_segments" :hash 3344690961)
 packed-vector-2array (p1 vector-2) (q1 vector-2) (p2 vector-2) (q2 vector-2))

(defgmethod
 (geometry-2d+get-closest-point-to-segment :class 'geometry-2d :bind
  "get_closest_point_to_segment" :hash 4172901909)
 vector-2 (point vector-2) (s1 vector-2) (s2 vector-2))

(defgmethod
 (geometry-2d+get-closest-point-to-segment-uncapped :class 'geometry-2d :bind
  "get_closest_point_to_segment_uncapped" :hash 4172901909)
 vector-2 (point vector-2) (s1 vector-2) (s2 vector-2))

(defgmethod
 (geometry-2d+point-is-inside-triangle :class 'geometry-2d :bind
  "point_is_inside_triangle" :hash 1025948137)
 bool (point vector-2) (a vector-2) (b vector-2) (c vector-2))

(defgmethod
 (geometry-2d+is-polygon-clockwise :class 'geometry-2d :bind
  "is_polygon_clockwise" :hash 1361156557)
 bool (polygon packed-vector-2array))

(defgmethod
 (geometry-2d+is-point-in-polygon :class 'geometry-2d :bind
  "is_point_in_polygon" :hash 738277916)
 bool (point vector-2) (polygon packed-vector-2array))

(defgmethod
 (geometry-2d+triangulate-polygon :class 'geometry-2d :bind
  "triangulate_polygon" :hash 1389921771)
 packed-int-32array (polygon packed-vector-2array))

(defgmethod
 (geometry-2d+triangulate-delaunay :class 'geometry-2d :bind
  "triangulate_delaunay" :hash 1389921771)
 packed-int-32array (points packed-vector-2array))

(defgmethod
 (geometry-2d+convex-hull :class 'geometry-2d :bind "convex_hull" :hash
  2004331998)
 packed-vector-2array (points packed-vector-2array))

(defgmethod
 (geometry-2d+decompose-polygon-in-convex :class 'geometry-2d :bind
  "decompose_polygon_in_convex" :hash 3982393695)
 array (polygon packed-vector-2array))

(defgmethod
 (geometry-2d+merge-polygons :class 'geometry-2d :bind "merge_polygons" :hash
  3637387053)
 array (polygon-a packed-vector-2array) (polygon-b packed-vector-2array))

(defgmethod
 (geometry-2d+clip-polygons :class 'geometry-2d :bind "clip_polygons" :hash
  3637387053)
 array (polygon-a packed-vector-2array) (polygon-b packed-vector-2array))

(defgmethod
 (geometry-2d+intersect-polygons :class 'geometry-2d :bind "intersect_polygons"
  :hash 3637387053)
 array (polygon-a packed-vector-2array) (polygon-b packed-vector-2array))

(defgmethod
 (geometry-2d+exclude-polygons :class 'geometry-2d :bind "exclude_polygons"
  :hash 3637387053)
 array (polygon-a packed-vector-2array) (polygon-b packed-vector-2array))

(defgmethod
 (geometry-2d+clip-polyline-with-polygon :class 'geometry-2d :bind
  "clip_polyline_with_polygon" :hash 3637387053)
 array (polyline packed-vector-2array) (polygon packed-vector-2array))

(defgmethod
 (geometry-2d+intersect-polyline-with-polygon :class 'geometry-2d :bind
  "intersect_polyline_with_polygon" :hash 3637387053)
 array (polyline packed-vector-2array) (polygon packed-vector-2array))

(defgmethod
 (geometry-2d+offset-polygon :class 'geometry-2d :bind "offset_polygon" :hash
  1275354010)
 array (polygon packed-vector-2array) (delta float)
 (join-type geometry-2d+poly-join-type))

(defgmethod
 (geometry-2d+offset-polyline :class 'geometry-2d :bind "offset_polyline" :hash
  2328231778)
 array (polyline packed-vector-2array) (delta float)
 (join-type geometry-2d+poly-join-type) (end-type geometry-2d+poly-end-type))

(defgmethod
 (geometry-2d+make-atlas :class 'geometry-2d :bind "make_atlas" :hash
  1337682371)
 dictionary (sizes packed-vector-2array))

(defgmethod
 (geometry-2d+bresenham-line :class 'geometry-2d :bind "bresenham_line" :hash
  1989391000)
 array (from vector-2i) (to vector-2i))