(common-lisp:in-package :%godot)


(defgmethod
 (foldable-container+fold :class 'foldable-container :bind "fold" :hash
  3218959716)
 :void)

(defgmethod
 (foldable-container+expand :class 'foldable-container :bind "expand" :hash
  3218959716)
 :void)

(defgmethod
 (foldable-container+set-folded :class 'foldable-container :bind "set_folded"
  :hash 2586408642)
 :void (folded bool))

(defgmethod
 (foldable-container+is-folded :class 'foldable-container :bind "is_folded"
  :hash 36873697)
 bool)

(defgmethod
 (foldable-container+set-foldable-group :class 'foldable-container :bind
  "set_foldable_group" :hash 3001390597)
 :void (button-group foldable-group))

(defgmethod
 (foldable-container+get-foldable-group :class 'foldable-container :bind
  "get_foldable_group" :hash 66499518)
 foldable-group)

(defgmethod
 (foldable-container+set-title :class 'foldable-container :bind "set_title"
  :hash 83702148)
 :void (text string))

(defgmethod
 (foldable-container+get-title :class 'foldable-container :bind "get_title"
  :hash 201670096)
 string)

(defgmethod
 (foldable-container+set-title-alignment :class 'foldable-container :bind
  "set_title_alignment" :hash 2312603777)
 :void (alignment horizontal-alignment))

(defgmethod
 (foldable-container+get-title-alignment :class 'foldable-container :bind
  "get_title_alignment" :hash 341400642)
 horizontal-alignment)

(defgmethod
 (foldable-container+set-language :class 'foldable-container :bind
  "set_language" :hash 83702148)
 :void (language string))

(defgmethod
 (foldable-container+get-language :class 'foldable-container :bind
  "get_language" :hash 201670096)
 string)

(defgmethod
 (foldable-container+set-title-text-direction :class 'foldable-container :bind
  "set_title_text_direction" :hash 119160795)
 :void (text-direction control+text-direction))

(defgmethod
 (foldable-container+get-title-text-direction :class 'foldable-container :bind
  "get_title_text_direction" :hash 797257663)
 control+text-direction)

(defgmethod
 (foldable-container+set-title-text-overrun-behavior :class 'foldable-container
  :bind "set_title_text_overrun_behavior" :hash 1008890932)
 :void (overrun-behavior text-server+overrun-behavior))

(defgmethod
 (foldable-container+get-title-text-overrun-behavior :class 'foldable-container
  :bind "get_title_text_overrun_behavior" :hash 3779142101)
 text-server+overrun-behavior)

(defgmethod
 (foldable-container+set-title-position :class 'foldable-container :bind
  "set_title_position" :hash 2276829442)
 :void (title-position foldable-container+title-position))

(defgmethod
 (foldable-container+get-title-position :class 'foldable-container :bind
  "get_title_position" :hash 3028840207)
 foldable-container+title-position)

(defgmethod
 (foldable-container+add-title-bar-control :class 'foldable-container :bind
  "add_title_bar_control" :hash 1496901182)
 :void (control control))

(defgmethod
 (foldable-container+remove-title-bar-control :class 'foldable-container :bind
  "remove_title_bar_control" :hash 1496901182)
 :void (control control))