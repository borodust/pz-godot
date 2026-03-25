(common-lisp:in-package :%godot)


(defgmethod
 (area-2d+set-gravity-space-override-mode :class 'area-2d :bind
  "set_gravity_space_override_mode" :hash 2879900038)
 :void (space-override-mode area-2d+space-override))

(defgmethod
 (area-2d+get-gravity-space-override-mode :class 'area-2d :bind
  "get_gravity_space_override_mode" :hash 3990256304)
 area-2d+space-override)

(defgmethod
 (area-2d+set-gravity-is-point :class 'area-2d :bind "set_gravity_is_point"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (area-2d+is-gravity-a-point :class 'area-2d :bind "is_gravity_a_point" :hash
  36873697)
 bool)

(defgmethod
 (area-2d+set-gravity-point-unit-distance :class 'area-2d :bind
  "set_gravity_point_unit_distance" :hash 373806689)
 :void (distance-scale float))

(defgmethod
 (area-2d+get-gravity-point-unit-distance :class 'area-2d :bind
  "get_gravity_point_unit_distance" :hash 1740695150)
 float)

(defgmethod
 (area-2d+set-gravity-point-center :class 'area-2d :bind
  "set_gravity_point_center" :hash 743155724)
 :void (center vector-2))

(defgmethod
 (area-2d+get-gravity-point-center :class 'area-2d :bind
  "get_gravity_point_center" :hash 3341600327)
 vector-2)

(defgmethod
 (area-2d+set-gravity-direction :class 'area-2d :bind "set_gravity_direction"
  :hash 743155724)
 :void (direction vector-2))

(defgmethod
 (area-2d+get-gravity-direction :class 'area-2d :bind "get_gravity_direction"
  :hash 3341600327)
 vector-2)

(defgmethod
 (area-2d+set-gravity :class 'area-2d :bind "set_gravity" :hash 373806689)
 :void (gravity float))

(defgmethod
 (area-2d+get-gravity :class 'area-2d :bind "get_gravity" :hash 1740695150)
 float)

(defgmethod
 (area-2d+set-linear-damp-space-override-mode :class 'area-2d :bind
  "set_linear_damp_space_override_mode" :hash 2879900038)
 :void (space-override-mode area-2d+space-override))

(defgmethod
 (area-2d+get-linear-damp-space-override-mode :class 'area-2d :bind
  "get_linear_damp_space_override_mode" :hash 3990256304)
 area-2d+space-override)

(defgmethod
 (area-2d+set-angular-damp-space-override-mode :class 'area-2d :bind
  "set_angular_damp_space_override_mode" :hash 2879900038)
 :void (space-override-mode area-2d+space-override))

(defgmethod
 (area-2d+get-angular-damp-space-override-mode :class 'area-2d :bind
  "get_angular_damp_space_override_mode" :hash 3990256304)
 area-2d+space-override)

(defgmethod
 (area-2d+set-linear-damp :class 'area-2d :bind "set_linear_damp" :hash
  373806689)
 :void (linear-damp float))

(defgmethod
 (area-2d+get-linear-damp :class 'area-2d :bind "get_linear_damp" :hash
  1740695150)
 float)

(defgmethod
 (area-2d+set-angular-damp :class 'area-2d :bind "set_angular_damp" :hash
  373806689)
 :void (angular-damp float))

(defgmethod
 (area-2d+get-angular-damp :class 'area-2d :bind "get_angular_damp" :hash
  1740695150)
 float)

(defgmethod
 (area-2d+set-priority :class 'area-2d :bind "set_priority" :hash 1286410249)
 :void (priority int))

(defgmethod
 (area-2d+get-priority :class 'area-2d :bind "get_priority" :hash 3905245786)
 int)

(defgmethod
 (area-2d+set-monitoring :class 'area-2d :bind "set_monitoring" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (area-2d+is-monitoring :class 'area-2d :bind "is_monitoring" :hash 36873697)
 bool)

(defgmethod
 (area-2d+set-monitorable :class 'area-2d :bind "set_monitorable" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (area-2d+is-monitorable :class 'area-2d :bind "is_monitorable" :hash 36873697)
 bool)

(defgmethod
 (area-2d+get-overlapping-bodies :class 'area-2d :bind "get_overlapping_bodies"
  :hash 3995934104)
 array)

(defgmethod
 (area-2d+get-overlapping-areas :class 'area-2d :bind "get_overlapping_areas"
  :hash 3995934104)
 array)

(defgmethod
 (area-2d+has-overlapping-bodies :class 'area-2d :bind "has_overlapping_bodies"
  :hash 36873697)
 bool)

(defgmethod
 (area-2d+has-overlapping-areas :class 'area-2d :bind "has_overlapping_areas"
  :hash 36873697)
 bool)

(defgmethod
 (area-2d+overlaps-body :class 'area-2d :bind "overlaps_body" :hash 3093956946)
 bool (body node))

(defgmethod
 (area-2d+overlaps-area :class 'area-2d :bind "overlaps_area" :hash 3093956946)
 bool (area node))

(defgmethod
 (area-2d+set-audio-bus-name :class 'area-2d :bind "set_audio_bus_name" :hash
  3304788590)
 :void (name string-name))

(defgmethod
 (area-2d+get-audio-bus-name :class 'area-2d :bind "get_audio_bus_name" :hash
  2002593661)
 string-name)

(defgmethod
 (area-2d+set-audio-bus-override :class 'area-2d :bind "set_audio_bus_override"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (area-2d+is-overriding-audio-bus :class 'area-2d :bind
  "is_overriding_audio_bus" :hash 36873697)
 bool)