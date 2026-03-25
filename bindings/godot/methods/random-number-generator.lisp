(common-lisp:in-package :%godot)


(defgmethod
 (random-number-generator+set-seed :class 'random-number-generator :bind
  "set_seed" :hash 1286410249)
 :void (seed int))

(defgmethod
 (random-number-generator+get-seed :class 'random-number-generator :bind
  "get_seed" :hash 2455072627)
 int)

(defgmethod
 (random-number-generator+set-state :class 'random-number-generator :bind
  "set_state" :hash 1286410249)
 :void (state int))

(defgmethod
 (random-number-generator+get-state :class 'random-number-generator :bind
  "get_state" :hash 3905245786)
 int)

(defgmethod
 (random-number-generator+randi :class 'random-number-generator :bind "randi"
  :hash 2455072627)
 int)

(defgmethod
 (random-number-generator+randf :class 'random-number-generator :bind "randf"
  :hash 191475506)
 float)

(defgmethod
 (random-number-generator+randfn :class 'random-number-generator :bind "randfn"
  :hash 837325100)
 float (mean float) (deviation float))

(defgmethod
 (random-number-generator+randf-range :class 'random-number-generator :bind
  "randf_range" :hash 4269894367)
 float (from float) (to float))

(defgmethod
 (random-number-generator+randi-range :class 'random-number-generator :bind
  "randi_range" :hash 50157827)
 int (from int) (to int))

(defgmethod
 (random-number-generator+rand-weighted :class 'random-number-generator :bind
  "rand_weighted" :hash 4189642986)
 int (weights packed-float-32array))

(defgmethod
 (random-number-generator+randomize :class 'random-number-generator :bind
  "randomize" :hash 3218959716)
 :void)