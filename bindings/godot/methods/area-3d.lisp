(common-lisp:in-package :%godot)


(defgmethod
 (area-3d+set-gravity-space-override-mode :class 'area-3d :bind
  "set_gravity_space_override_mode" :hash 2311433571)
 :void (space-override-mode area-3d+space-override))

(defgmethod
 (area-3d+get-gravity-space-override-mode :class 'area-3d :bind
  "get_gravity_space_override_mode" :hash 958191869)
 area-3d+space-override)

(defgmethod
 (area-3d+set-gravity-is-point :class 'area-3d :bind "set_gravity_is_point"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (area-3d+is-gravity-a-point :class 'area-3d :bind "is_gravity_a_point" :hash
  36873697)
 bool)

(defgmethod
 (area-3d+set-gravity-point-unit-distance :class 'area-3d :bind
  "set_gravity_point_unit_distance" :hash 373806689)
 :void (distance-scale float))

(defgmethod
 (area-3d+get-gravity-point-unit-distance :class 'area-3d :bind
  "get_gravity_point_unit_distance" :hash 1740695150)
 float)

(defgmethod
 (area-3d+set-gravity-point-center :class 'area-3d :bind
  "set_gravity_point_center" :hash 3460891852)
 :void (center vector-3))

(defgmethod
 (area-3d+get-gravity-point-center :class 'area-3d :bind
  "get_gravity_point_center" :hash 3360562783)
 vector-3)

(defgmethod
 (area-3d+set-gravity-direction :class 'area-3d :bind "set_gravity_direction"
  :hash 3460891852)
 :void (direction vector-3))

(defgmethod
 (area-3d+get-gravity-direction :class 'area-3d :bind "get_gravity_direction"
  :hash 3360562783)
 vector-3)

(defgmethod
 (area-3d+set-gravity :class 'area-3d :bind "set_gravity" :hash 373806689)
 :void (gravity float))

(defgmethod
 (area-3d+get-gravity :class 'area-3d :bind "get_gravity" :hash 1740695150)
 float)

(defgmethod
 (area-3d+set-linear-damp-space-override-mode :class 'area-3d :bind
  "set_linear_damp_space_override_mode" :hash 2311433571)
 :void (space-override-mode area-3d+space-override))

(defgmethod
 (area-3d+get-linear-damp-space-override-mode :class 'area-3d :bind
  "get_linear_damp_space_override_mode" :hash 958191869)
 area-3d+space-override)

(defgmethod
 (area-3d+set-angular-damp-space-override-mode :class 'area-3d :bind
  "set_angular_damp_space_override_mode" :hash 2311433571)
 :void (space-override-mode area-3d+space-override))

(defgmethod
 (area-3d+get-angular-damp-space-override-mode :class 'area-3d :bind
  "get_angular_damp_space_override_mode" :hash 958191869)
 area-3d+space-override)

(defgmethod
 (area-3d+set-angular-damp :class 'area-3d :bind "set_angular_damp" :hash
  373806689)
 :void (angular-damp float))

(defgmethod
 (area-3d+get-angular-damp :class 'area-3d :bind "get_angular_damp" :hash
  1740695150)
 float)

(defgmethod
 (area-3d+set-linear-damp :class 'area-3d :bind "set_linear_damp" :hash
  373806689)
 :void (linear-damp float))

(defgmethod
 (area-3d+get-linear-damp :class 'area-3d :bind "get_linear_damp" :hash
  1740695150)
 float)

(defgmethod
 (area-3d+set-priority :class 'area-3d :bind "set_priority" :hash 1286410249)
 :void (priority int))

(defgmethod
 (area-3d+get-priority :class 'area-3d :bind "get_priority" :hash 3905245786)
 int)

(defgmethod
 (area-3d+set-wind-force-magnitude :class 'area-3d :bind
  "set_wind_force_magnitude" :hash 373806689)
 :void (wind-force-magnitude float))

(defgmethod
 (area-3d+get-wind-force-magnitude :class 'area-3d :bind
  "get_wind_force_magnitude" :hash 1740695150)
 float)

(defgmethod
 (area-3d+set-wind-attenuation-factor :class 'area-3d :bind
  "set_wind_attenuation_factor" :hash 373806689)
 :void (wind-attenuation-factor float))

(defgmethod
 (area-3d+get-wind-attenuation-factor :class 'area-3d :bind
  "get_wind_attenuation_factor" :hash 1740695150)
 float)

(defgmethod
 (area-3d+set-wind-source-path :class 'area-3d :bind "set_wind_source_path"
  :hash 1348162250)
 :void (wind-source-path node-path))

(defgmethod
 (area-3d+get-wind-source-path :class 'area-3d :bind "get_wind_source_path"
  :hash 4075236667)
 node-path)

(defgmethod
 (area-3d+set-monitorable :class 'area-3d :bind "set_monitorable" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (area-3d+is-monitorable :class 'area-3d :bind "is_monitorable" :hash 36873697)
 bool)

(defgmethod
 (area-3d+set-monitoring :class 'area-3d :bind "set_monitoring" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (area-3d+is-monitoring :class 'area-3d :bind "is_monitoring" :hash 36873697)
 bool)

(defgmethod
 (area-3d+get-overlapping-bodies :class 'area-3d :bind "get_overlapping_bodies"
  :hash 3995934104)
 array)

(defgmethod
 (area-3d+get-overlapping-areas :class 'area-3d :bind "get_overlapping_areas"
  :hash 3995934104)
 array)

(defgmethod
 (area-3d+has-overlapping-bodies :class 'area-3d :bind "has_overlapping_bodies"
  :hash 36873697)
 bool)

(defgmethod
 (area-3d+has-overlapping-areas :class 'area-3d :bind "has_overlapping_areas"
  :hash 36873697)
 bool)

(defgmethod
 (area-3d+overlaps-body :class 'area-3d :bind "overlaps_body" :hash 3093956946)
 bool (body node))

(defgmethod
 (area-3d+overlaps-area :class 'area-3d :bind "overlaps_area" :hash 3093956946)
 bool (area node))

(defgmethod
 (area-3d+set-audio-bus-override :class 'area-3d :bind "set_audio_bus_override"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (area-3d+is-overriding-audio-bus :class 'area-3d :bind
  "is_overriding_audio_bus" :hash 36873697)
 bool)

(defgmethod
 (area-3d+set-audio-bus-name :class 'area-3d :bind "set_audio_bus_name" :hash
  3304788590)
 :void (name string-name))

(defgmethod
 (area-3d+get-audio-bus-name :class 'area-3d :bind "get_audio_bus_name" :hash
  2002593661)
 string-name)

(defgmethod
 (area-3d+set-use-reverb-bus :class 'area-3d :bind "set_use_reverb_bus" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (area-3d+is-using-reverb-bus :class 'area-3d :bind "is_using_reverb_bus" :hash
  36873697)
 bool)

(defgmethod
 (area-3d+set-reverb-bus-name :class 'area-3d :bind "set_reverb_bus_name" :hash
  3304788590)
 :void (name string-name))

(defgmethod
 (area-3d+get-reverb-bus-name :class 'area-3d :bind "get_reverb_bus_name" :hash
  2002593661)
 string-name)

(defgmethod
 (area-3d+set-reverb-amount :class 'area-3d :bind "set_reverb_amount" :hash
  373806689)
 :void (amount float))

(defgmethod
 (area-3d+get-reverb-amount :class 'area-3d :bind "get_reverb_amount" :hash
  1740695150)
 float)

(defgmethod
 (area-3d+set-reverb-uniformity :class 'area-3d :bind "set_reverb_uniformity"
  :hash 373806689)
 :void (amount float))

(defgmethod
 (area-3d+get-reverb-uniformity :class 'area-3d :bind "get_reverb_uniformity"
  :hash 1740695150)
 float)