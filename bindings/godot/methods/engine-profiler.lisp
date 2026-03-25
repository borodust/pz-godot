(common-lisp:in-package :%godot)


(defgmethod
 (engine-profiler+-toggle :class 'engine-profiler :bind "_toggle" :hash
  1157301103 :virtual common-lisp:t)
 :void (enable bool) (options array))

(defgmethod
 (engine-profiler+-add-frame :class 'engine-profiler :bind "_add_frame" :hash
  381264803 :virtual common-lisp:t)
 :void (data array))

(defgmethod
 (engine-profiler+-tick :class 'engine-profiler :bind "_tick" :hash 3948312143
  :virtual common-lisp:t)
 :void (frame-time float) (process-time float) (physics-time float)
 (physics-frame-time float))