(common-lisp:in-package :%godot)


(defgmethod (theme+set-icon :class 'theme :bind "set_icon" :hash 2188371082)
 :void (name string-name) (theme-type string-name) (texture texture-2d))

(defgmethod (theme+get-icon :class 'theme :bind "get_icon" :hash 934555193)
 texture-2d (name string-name) (theme-type string-name))

(defgmethod (theme+has-icon :class 'theme :bind "has_icon" :hash 471820014)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (theme+rename-icon :class 'theme :bind "rename_icon" :hash 642128662) :void
 (old-name string-name) (name string-name) (theme-type string-name))

(defgmethod
 (theme+clear-icon :class 'theme :bind "clear_icon" :hash 3740211285) :void
 (name string-name) (theme-type string-name))

(defgmethod
 (theme+get-icon-list :class 'theme :bind "get_icon_list" :hash 4291131558)
 packed-string-array (theme-type string))

(defgmethod
 (theme+get-icon-type-list :class 'theme :bind "get_icon_type_list" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (theme+set-stylebox :class 'theme :bind "set_stylebox" :hash 2075907568) :void
 (name string-name) (theme-type string-name) (texture style-box))

(defgmethod
 (theme+get-stylebox :class 'theme :bind "get_stylebox" :hash 3405608165)
 style-box (name string-name) (theme-type string-name))

(defgmethod
 (theme+has-stylebox :class 'theme :bind "has_stylebox" :hash 471820014) bool
 (name string-name) (theme-type string-name))

(defgmethod
 (theme+rename-stylebox :class 'theme :bind "rename_stylebox" :hash 642128662)
 :void (old-name string-name) (name string-name) (theme-type string-name))

(defgmethod
 (theme+clear-stylebox :class 'theme :bind "clear_stylebox" :hash 3740211285)
 :void (name string-name) (theme-type string-name))

(defgmethod
 (theme+get-stylebox-list :class 'theme :bind "get_stylebox_list" :hash
  4291131558)
 packed-string-array (theme-type string))

(defgmethod
 (theme+get-stylebox-type-list :class 'theme :bind "get_stylebox_type_list"
  :hash 1139954409)
 packed-string-array)

(defgmethod (theme+set-font :class 'theme :bind "set_font" :hash 177292320)
 :void (name string-name) (theme-type string-name) (font font))

(defgmethod (theme+get-font :class 'theme :bind "get_font" :hash 3445063586)
 font (name string-name) (theme-type string-name))

(defgmethod (theme+has-font :class 'theme :bind "has_font" :hash 471820014)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (theme+rename-font :class 'theme :bind "rename_font" :hash 642128662) :void
 (old-name string-name) (name string-name) (theme-type string-name))

(defgmethod
 (theme+clear-font :class 'theme :bind "clear_font" :hash 3740211285) :void
 (name string-name) (theme-type string-name))

(defgmethod
 (theme+get-font-list :class 'theme :bind "get_font_list" :hash 4291131558)
 packed-string-array (theme-type string))

(defgmethod
 (theme+get-font-type-list :class 'theme :bind "get_font_type_list" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (theme+set-font-size :class 'theme :bind "set_font_size" :hash 281601298)
 :void (name string-name) (theme-type string-name) (font-size int))

(defgmethod
 (theme+get-font-size :class 'theme :bind "get_font_size" :hash 2419549490) int
 (name string-name) (theme-type string-name))

(defgmethod
 (theme+has-font-size :class 'theme :bind "has_font_size" :hash 471820014) bool
 (name string-name) (theme-type string-name))

(defgmethod
 (theme+rename-font-size :class 'theme :bind "rename_font_size" :hash
  642128662)
 :void (old-name string-name) (name string-name) (theme-type string-name))

(defgmethod
 (theme+clear-font-size :class 'theme :bind "clear_font_size" :hash 3740211285)
 :void (name string-name) (theme-type string-name))

(defgmethod
 (theme+get-font-size-list :class 'theme :bind "get_font_size_list" :hash
  4291131558)
 packed-string-array (theme-type string))

(defgmethod
 (theme+get-font-size-type-list :class 'theme :bind "get_font_size_type_list"
  :hash 1139954409)
 packed-string-array)

(defgmethod (theme+set-color :class 'theme :bind "set_color" :hash 4111215154)
 :void (name string-name) (theme-type string-name) (color color))

(defgmethod (theme+get-color :class 'theme :bind "get_color" :hash 2015923404)
 color (name string-name) (theme-type string-name))

(defgmethod (theme+has-color :class 'theme :bind "has_color" :hash 471820014)
 bool (name string-name) (theme-type string-name))

(defgmethod
 (theme+rename-color :class 'theme :bind "rename_color" :hash 642128662) :void
 (old-name string-name) (name string-name) (theme-type string-name))

(defgmethod
 (theme+clear-color :class 'theme :bind "clear_color" :hash 3740211285) :void
 (name string-name) (theme-type string-name))

(defgmethod
 (theme+get-color-list :class 'theme :bind "get_color_list" :hash 4291131558)
 packed-string-array (theme-type string))

(defgmethod
 (theme+get-color-type-list :class 'theme :bind "get_color_type_list" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (theme+set-constant :class 'theme :bind "set_constant" :hash 281601298) :void
 (name string-name) (theme-type string-name) (constant int))

(defgmethod
 (theme+get-constant :class 'theme :bind "get_constant" :hash 2419549490) int
 (name string-name) (theme-type string-name))

(defgmethod
 (theme+has-constant :class 'theme :bind "has_constant" :hash 471820014) bool
 (name string-name) (theme-type string-name))

(defgmethod
 (theme+rename-constant :class 'theme :bind "rename_constant" :hash 642128662)
 :void (old-name string-name) (name string-name) (theme-type string-name))

(defgmethod
 (theme+clear-constant :class 'theme :bind "clear_constant" :hash 3740211285)
 :void (name string-name) (theme-type string-name))

(defgmethod
 (theme+get-constant-list :class 'theme :bind "get_constant_list" :hash
  4291131558)
 packed-string-array (theme-type string))

(defgmethod
 (theme+get-constant-type-list :class 'theme :bind "get_constant_type_list"
  :hash 1139954409)
 packed-string-array)

(defgmethod
 (theme+set-default-base-scale :class 'theme :bind "set_default_base_scale"
  :hash 373806689)
 :void (base-scale float))

(defgmethod
 (theme+get-default-base-scale :class 'theme :bind "get_default_base_scale"
  :hash 1740695150)
 float)

(defgmethod
 (theme+has-default-base-scale :class 'theme :bind "has_default_base_scale"
  :hash 36873697)
 bool)

(defgmethod
 (theme+set-default-font :class 'theme :bind "set_default_font" :hash
  1262170328)
 :void (font font))

(defgmethod
 (theme+get-default-font :class 'theme :bind "get_default_font" :hash
  3229501585)
 font)

(defgmethod
 (theme+has-default-font :class 'theme :bind "has_default_font" :hash 36873697)
 bool)

(defgmethod
 (theme+set-default-font-size :class 'theme :bind "set_default_font_size" :hash
  1286410249)
 :void (font-size int))

(defgmethod
 (theme+get-default-font-size :class 'theme :bind "get_default_font_size" :hash
  3905245786)
 int)

(defgmethod
 (theme+has-default-font-size :class 'theme :bind "has_default_font_size" :hash
  36873697)
 bool)

(defgmethod
 (theme+set-theme-item :class 'theme :bind "set_theme_item" :hash 2492983623)
 :void (data-type theme+data-type) (name string-name) (theme-type string-name)
 (value variant))

(defgmethod
 (theme+get-theme-item :class 'theme :bind "get_theme_item" :hash 2191024021)
 variant (data-type theme+data-type) (name string-name)
 (theme-type string-name))

(defgmethod
 (theme+has-theme-item :class 'theme :bind "has_theme_item" :hash 1739311056)
 bool (data-type theme+data-type) (name string-name) (theme-type string-name))

(defgmethod
 (theme+rename-theme-item :class 'theme :bind "rename_theme_item" :hash
  3900867553)
 :void (data-type theme+data-type) (old-name string-name) (name string-name)
 (theme-type string-name))

(defgmethod
 (theme+clear-theme-item :class 'theme :bind "clear_theme_item" :hash
  2965505587)
 :void (data-type theme+data-type) (name string-name) (theme-type string-name))

(defgmethod
 (theme+get-theme-item-list :class 'theme :bind "get_theme_item_list" :hash
  3726716710)
 packed-string-array (data-type theme+data-type) (theme-type string))

(defgmethod
 (theme+get-theme-item-type-list :class 'theme :bind "get_theme_item_type_list"
  :hash 1316004935)
 packed-string-array (data-type theme+data-type))

(defgmethod
 (theme+set-type-variation :class 'theme :bind "set_type_variation" :hash
  3740211285)
 :void (theme-type string-name) (base-type string-name))

(defgmethod
 (theme+is-type-variation :class 'theme :bind "is_type_variation" :hash
  471820014)
 bool (theme-type string-name) (base-type string-name))

(defgmethod
 (theme+clear-type-variation :class 'theme :bind "clear_type_variation" :hash
  3304788590)
 :void (theme-type string-name))

(defgmethod
 (theme+get-type-variation-base :class 'theme :bind "get_type_variation_base"
  :hash 1965194235)
 string-name (theme-type string-name))

(defgmethod
 (theme+get-type-variation-list :class 'theme :bind "get_type_variation_list"
  :hash 1761182771)
 packed-string-array (base-type string-name))

(defgmethod (theme+add-type :class 'theme :bind "add_type" :hash 3304788590)
 :void (theme-type string-name))

(defgmethod
 (theme+remove-type :class 'theme :bind "remove_type" :hash 3304788590) :void
 (theme-type string-name))

(defgmethod
 (theme+rename-type :class 'theme :bind "rename_type" :hash 3740211285) :void
 (old-theme-type string-name) (theme-type string-name))

(defgmethod
 (theme+get-type-list :class 'theme :bind "get_type_list" :hash 1139954409)
 packed-string-array)

(defgmethod
 (theme+merge-with :class 'theme :bind "merge_with" :hash 2326690814) :void
 (other theme))

(defgmethod (theme+clear :class 'theme :bind "clear" :hash 3218959716) :void)