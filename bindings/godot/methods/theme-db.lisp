(common-lisp:in-package :%godot)


(defgmethod
 (theme-db+get-default-theme :class 'theme-db :bind "get_default_theme" :hash
  754276358)
 theme)

(defgmethod
 (theme-db+get-project-theme :class 'theme-db :bind "get_project_theme" :hash
  754276358)
 theme)

(defgmethod
 (theme-db+set-fallback-base-scale :class 'theme-db :bind
  "set_fallback_base_scale" :hash 373806689)
 :void (base-scale float))

(defgmethod
 (theme-db+get-fallback-base-scale :class 'theme-db :bind
  "get_fallback_base_scale" :hash 191475506)
 float)

(defgmethod
 (theme-db+set-fallback-font :class 'theme-db :bind "set_fallback_font" :hash
  1262170328)
 :void (font font))

(defgmethod
 (theme-db+get-fallback-font :class 'theme-db :bind "get_fallback_font" :hash
  3656929885)
 font)

(defgmethod
 (theme-db+set-fallback-font-size :class 'theme-db :bind
  "set_fallback_font_size" :hash 1286410249)
 :void (font-size int))

(defgmethod
 (theme-db+get-fallback-font-size :class 'theme-db :bind
  "get_fallback_font_size" :hash 2455072627)
 int)

(defgmethod
 (theme-db+set-fallback-icon :class 'theme-db :bind "set_fallback_icon" :hash
  4051416890)
 :void (icon texture-2d))

(defgmethod
 (theme-db+get-fallback-icon :class 'theme-db :bind "get_fallback_icon" :hash
  255860311)
 texture-2d)

(defgmethod
 (theme-db+set-fallback-stylebox :class 'theme-db :bind "set_fallback_stylebox"
  :hash 2797200388)
 :void (stylebox style-box))

(defgmethod
 (theme-db+get-fallback-stylebox :class 'theme-db :bind "get_fallback_stylebox"
  :hash 496040854)
 style-box)