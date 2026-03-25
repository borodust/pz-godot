(common-lisp:in-package :%godot)


(defgproperty lightmap-gidata+lightmap-textures 'lightmap-gidata :get
 'lightmap-gidata+get-lightmap-textures :set
 'lightmap-gidata+set-lightmap-textures)

(defgproperty lightmap-gidata+shadowmask-textures 'lightmap-gidata :get
 'lightmap-gidata+get-shadowmask-textures :set
 'lightmap-gidata+set-shadowmask-textures)

(defgproperty lightmap-gidata+uses-spherical-harmonics 'lightmap-gidata :get
 'lightmap-gidata+is-using-spherical-harmonics :set
 'lightmap-gidata+set-uses-spherical-harmonics)

(defgproperty lightmap-gidata+user-data 'lightmap-gidata)

(defgproperty lightmap-gidata+probe-data 'lightmap-gidata)

(defgproperty lightmap-gidata+light-texture 'lightmap-gidata :get
 'lightmap-gidata+get-light-texture :set 'lightmap-gidata+set-light-texture)

(defgproperty lightmap-gidata+light-textures 'lightmap-gidata)