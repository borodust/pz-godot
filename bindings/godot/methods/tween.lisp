(common-lisp:in-package :%godot)


(defgmethod
 (tween+tween-property :class 'tween :bind "tween_property" :hash 4049770449)
 property-tweener (object object) (property node-path) (final-val variant)
 (duration float))

(defgmethod
 (tween+tween-interval :class 'tween :bind "tween_interval" :hash 413360199)
 interval-tweener (time float))

(defgmethod
 (tween+tween-callback :class 'tween :bind "tween_callback" :hash 1540176488)
 callback-tweener (callback callable))

(defgmethod
 (tween+tween-method :class 'tween :bind "tween_method" :hash 2337877153)
 method-tweener (method callable) (from variant) (to variant) (duration float))

(defgmethod
 (tween+tween-subtween :class 'tween :bind "tween_subtween" :hash 1567358477)
 subtween-tweener (subtween tween))

(defgmethod
 (tween+custom-step :class 'tween :bind "custom_step" :hash 330693286) bool
 (delta float))

(defgmethod (tween+stop :class 'tween :bind "stop" :hash 3218959716) :void)

(defgmethod (tween+pause :class 'tween :bind "pause" :hash 3218959716) :void)

(defgmethod (tween+play :class 'tween :bind "play" :hash 3218959716) :void)

(defgmethod (tween+kill :class 'tween :bind "kill" :hash 3218959716) :void)

(defgmethod
 (tween+get-total-elapsed-time :class 'tween :bind "get_total_elapsed_time"
  :hash 1740695150)
 float)

(defgmethod
 (tween+is-running :class 'tween :bind "is_running" :hash 2240911060) bool)

(defgmethod (tween+is-valid :class 'tween :bind "is_valid" :hash 2240911060)
 bool)

(defgmethod (tween+bind-node :class 'tween :bind "bind_node" :hash 2946786331)
 tween (node node))

(defgmethod
 (tween+set-process-mode :class 'tween :bind "set_process_mode" :hash
  855258840)
 tween (mode tween+tween-process-mode))

(defgmethod
 (tween+set-pause-mode :class 'tween :bind "set_pause_mode" :hash 3363368837)
 tween (mode tween+tween-pause-mode))

(defgmethod
 (tween+set-ignore-time-scale :class 'tween :bind "set_ignore_time_scale" :hash
  1942052223)
 tween (ignore bool))

(defgmethod
 (tween+set-parallel :class 'tween :bind "set_parallel" :hash 1942052223) tween
 (parallel bool))

(defgmethod (tween+set-loops :class 'tween :bind "set_loops" :hash 2670836414)
 tween (loops int))

(defgmethod
 (tween+get-loops-left :class 'tween :bind "get_loops_left" :hash 3905245786)
 int)

(defgmethod
 (tween+set-speed-scale :class 'tween :bind "set_speed_scale" :hash 3961971106)
 tween (speed float))

(defgmethod (tween+set-trans :class 'tween :bind "set_trans" :hash 3965963875)
 tween (trans tween+transition-type))

(defgmethod (tween+set-ease :class 'tween :bind "set_ease" :hash 1208117252)
 tween (ease tween+ease-type))

(defgmethod (tween+parallel :class 'tween :bind "parallel" :hash 3426978995)
 tween)

(defgmethod (tween+chain :class 'tween :bind "chain" :hash 3426978995) tween)

(defgmethod
 (tween+interpolate-value :class 'tween :bind "interpolate_value" :hash
  3452526450 :static common-lisp:t)
 variant (initial-value variant) (delta-value variant) (elapsed-time float)
 (duration float) (trans-type tween+transition-type)
 (ease-type tween+ease-type))