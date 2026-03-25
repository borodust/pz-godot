(common-lisp:in-package :%godot)


(defgmethod
 (lightmap-gidata+set-lightmap-textures :class 'lightmap-gidata :bind
  "set_lightmap_textures" :hash 381264803)
 :void (light-textures array))

(defgmethod
 (lightmap-gidata+get-lightmap-textures :class 'lightmap-gidata :bind
  "get_lightmap_textures" :hash 3995934104)
 array)

(defgmethod
 (lightmap-gidata+set-shadowmask-textures :class 'lightmap-gidata :bind
  "set_shadowmask_textures" :hash 381264803)
 :void (shadowmask-textures array))

(defgmethod
 (lightmap-gidata+get-shadowmask-textures :class 'lightmap-gidata :bind
  "get_shadowmask_textures" :hash 3995934104)
 array)

(defgmethod
 (lightmap-gidata+set-uses-spherical-harmonics :class 'lightmap-gidata :bind
  "set_uses_spherical_harmonics" :hash 2586408642)
 :void (uses-spherical-harmonics bool))

(defgmethod
 (lightmap-gidata+is-using-spherical-harmonics :class 'lightmap-gidata :bind
  "is_using_spherical_harmonics" :hash 36873697)
 bool)

(defgmethod
 (lightmap-gidata+add-user :class 'lightmap-gidata :bind "add_user" :hash
  4272570515)
 :void (path node-path) (uv-scale rect-2) (slice-index int) (sub-instance int))

(defgmethod
 (lightmap-gidata+get-user-count :class 'lightmap-gidata :bind "get_user_count"
  :hash 3905245786)
 int)

(defgmethod
 (lightmap-gidata+get-user-path :class 'lightmap-gidata :bind "get_user_path"
  :hash 408788394)
 node-path (user-idx int))

(defgmethod
 (lightmap-gidata+clear-users :class 'lightmap-gidata :bind "clear_users" :hash
  3218959716)
 :void)

(defgmethod
 (lightmap-gidata+set-light-texture :class 'lightmap-gidata :bind
  "set_light_texture" :hash 1278366092)
 :void (light-texture texture-layered))

(defgmethod
 (lightmap-gidata+get-light-texture :class 'lightmap-gidata :bind
  "get_light_texture" :hash 3984243839)
 texture-layered)