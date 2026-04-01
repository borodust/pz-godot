(common-lisp:in-package :%godot)


(defgmethod
 (range+%value-changed :class 'range :bind "_value_changed" :hash 373806689
  :virtual common-lisp:t)
 :void (new-value float))

(defgmethod (range+get-value :class 'range :bind "get_value" :hash 1740695150)
 float)

(defgmethod (range+get-min :class 'range :bind "get_min" :hash 1740695150)
 float)

(defgmethod (range+get-max :class 'range :bind "get_max" :hash 1740695150)
 float)

(defgmethod (range+get-step :class 'range :bind "get_step" :hash 1740695150)
 float)

(defgmethod (range+get-page :class 'range :bind "get_page" :hash 1740695150)
 float)

(defgmethod
 (range+get-as-ratio :class 'range :bind "get_as_ratio" :hash 1740695150) float)

(defgmethod (range+set-value :class 'range :bind "set_value" :hash 373806689)
 :void (value float))

(defgmethod
 (range+set-value-no-signal :class 'range :bind "set_value_no_signal" :hash
  373806689)
 :void (value float))

(defgmethod (range+set-min :class 'range :bind "set_min" :hash 373806689) :void
 (minimum float))

(defgmethod (range+set-max :class 'range :bind "set_max" :hash 373806689) :void
 (maximum float))

(defgmethod (range+set-step :class 'range :bind "set_step" :hash 373806689)
 :void (step float))

(defgmethod (range+set-page :class 'range :bind "set_page" :hash 373806689)
 :void (pagesize float))

(defgmethod
 (range+set-as-ratio :class 'range :bind "set_as_ratio" :hash 373806689) :void
 (value float))

(defgmethod
 (range+set-use-rounded-values :class 'range :bind "set_use_rounded_values"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (range+is-using-rounded-values :class 'range :bind "is_using_rounded_values"
  :hash 36873697)
 bool)

(defgmethod
 (range+set-exp-ratio :class 'range :bind "set_exp_ratio" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (range+is-ratio-exp :class 'range :bind "is_ratio_exp" :hash 36873697) bool)

(defgmethod
 (range+set-allow-greater :class 'range :bind "set_allow_greater" :hash
  2586408642)
 :void (allow bool))

(defgmethod
 (range+is-greater-allowed :class 'range :bind "is_greater_allowed" :hash
  36873697)
 bool)

(defgmethod
 (range+set-allow-lesser :class 'range :bind "set_allow_lesser" :hash
  2586408642)
 :void (allow bool))

(defgmethod
 (range+is-lesser-allowed :class 'range :bind "is_lesser_allowed" :hash
  36873697)
 bool)

(defgmethod (range+share :class 'range :bind "share" :hash 1078189570) :void
 (with node))

(defgmethod (range+unshare :class 'range :bind "unshare" :hash 3218959716)
 :void)