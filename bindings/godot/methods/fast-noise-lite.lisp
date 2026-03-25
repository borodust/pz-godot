(common-lisp:in-package :%godot)


(defgmethod
 (fast-noise-lite+set-noise-type :class 'fast-noise-lite :bind "set_noise_type"
  :hash 2624461392)
 :void (type fast-noise-lite+noise-type))

(defgmethod
 (fast-noise-lite+get-noise-type :class 'fast-noise-lite :bind "get_noise_type"
  :hash 1458108610)
 fast-noise-lite+noise-type)

(defgmethod
 (fast-noise-lite+set-seed :class 'fast-noise-lite :bind "set_seed" :hash
  1286410249)
 :void (seed int))

(defgmethod
 (fast-noise-lite+get-seed :class 'fast-noise-lite :bind "get_seed" :hash
  3905245786)
 int)

(defgmethod
 (fast-noise-lite+set-frequency :class 'fast-noise-lite :bind "set_frequency"
  :hash 373806689)
 :void (freq float))

(defgmethod
 (fast-noise-lite+get-frequency :class 'fast-noise-lite :bind "get_frequency"
  :hash 1740695150)
 float)

(defgmethod
 (fast-noise-lite+set-offset :class 'fast-noise-lite :bind "set_offset" :hash
  3460891852)
 :void (offset vector-3))

(defgmethod
 (fast-noise-lite+get-offset :class 'fast-noise-lite :bind "get_offset" :hash
  3360562783)
 vector-3)

(defgmethod
 (fast-noise-lite+set-fractal-type :class 'fast-noise-lite :bind
  "set_fractal_type" :hash 4132731174)
 :void (type fast-noise-lite+fractal-type))

(defgmethod
 (fast-noise-lite+get-fractal-type :class 'fast-noise-lite :bind
  "get_fractal_type" :hash 1036889279)
 fast-noise-lite+fractal-type)

(defgmethod
 (fast-noise-lite+set-fractal-octaves :class 'fast-noise-lite :bind
  "set_fractal_octaves" :hash 1286410249)
 :void (octave-count int))

(defgmethod
 (fast-noise-lite+get-fractal-octaves :class 'fast-noise-lite :bind
  "get_fractal_octaves" :hash 3905245786)
 int)

(defgmethod
 (fast-noise-lite+set-fractal-lacunarity :class 'fast-noise-lite :bind
  "set_fractal_lacunarity" :hash 373806689)
 :void (lacunarity float))

(defgmethod
 (fast-noise-lite+get-fractal-lacunarity :class 'fast-noise-lite :bind
  "get_fractal_lacunarity" :hash 1740695150)
 float)

(defgmethod
 (fast-noise-lite+set-fractal-gain :class 'fast-noise-lite :bind
  "set_fractal_gain" :hash 373806689)
 :void (gain float))

(defgmethod
 (fast-noise-lite+get-fractal-gain :class 'fast-noise-lite :bind
  "get_fractal_gain" :hash 1740695150)
 float)

(defgmethod
 (fast-noise-lite+set-fractal-weighted-strength :class 'fast-noise-lite :bind
  "set_fractal_weighted_strength" :hash 373806689)
 :void (weighted-strength float))

(defgmethod
 (fast-noise-lite+get-fractal-weighted-strength :class 'fast-noise-lite :bind
  "get_fractal_weighted_strength" :hash 1740695150)
 float)

(defgmethod
 (fast-noise-lite+set-fractal-ping-pong-strength :class 'fast-noise-lite :bind
  "set_fractal_ping_pong_strength" :hash 373806689)
 :void (ping-pong-strength float))

(defgmethod
 (fast-noise-lite+get-fractal-ping-pong-strength :class 'fast-noise-lite :bind
  "get_fractal_ping_pong_strength" :hash 1740695150)
 float)

(defgmethod
 (fast-noise-lite+set-cellular-distance-function :class 'fast-noise-lite :bind
  "set_cellular_distance_function" :hash 1006013267)
 :void (func fast-noise-lite+cellular-distance-function))

(defgmethod
 (fast-noise-lite+get-cellular-distance-function :class 'fast-noise-lite :bind
  "get_cellular_distance_function" :hash 2021274088)
 fast-noise-lite+cellular-distance-function)

(defgmethod
 (fast-noise-lite+set-cellular-jitter :class 'fast-noise-lite :bind
  "set_cellular_jitter" :hash 373806689)
 :void (jitter float))

(defgmethod
 (fast-noise-lite+get-cellular-jitter :class 'fast-noise-lite :bind
  "get_cellular_jitter" :hash 1740695150)
 float)

(defgmethod
 (fast-noise-lite+set-cellular-return-type :class 'fast-noise-lite :bind
  "set_cellular_return_type" :hash 2654169698)
 :void (ret fast-noise-lite+cellular-return-type))

(defgmethod
 (fast-noise-lite+get-cellular-return-type :class 'fast-noise-lite :bind
  "get_cellular_return_type" :hash 3699796343)
 fast-noise-lite+cellular-return-type)

(defgmethod
 (fast-noise-lite+set-domain-warp-enabled :class 'fast-noise-lite :bind
  "set_domain_warp_enabled" :hash 2586408642)
 :void (domain-warp-enabled bool))

(defgmethod
 (fast-noise-lite+is-domain-warp-enabled :class 'fast-noise-lite :bind
  "is_domain_warp_enabled" :hash 36873697)
 bool)

(defgmethod
 (fast-noise-lite+set-domain-warp-type :class 'fast-noise-lite :bind
  "set_domain_warp_type" :hash 3629692980)
 :void (domain-warp-type fast-noise-lite+domain-warp-type))

(defgmethod
 (fast-noise-lite+get-domain-warp-type :class 'fast-noise-lite :bind
  "get_domain_warp_type" :hash 2980162020)
 fast-noise-lite+domain-warp-type)

(defgmethod
 (fast-noise-lite+set-domain-warp-amplitude :class 'fast-noise-lite :bind
  "set_domain_warp_amplitude" :hash 373806689)
 :void (domain-warp-amplitude float))

(defgmethod
 (fast-noise-lite+get-domain-warp-amplitude :class 'fast-noise-lite :bind
  "get_domain_warp_amplitude" :hash 1740695150)
 float)

(defgmethod
 (fast-noise-lite+set-domain-warp-frequency :class 'fast-noise-lite :bind
  "set_domain_warp_frequency" :hash 373806689)
 :void (domain-warp-frequency float))

(defgmethod
 (fast-noise-lite+get-domain-warp-frequency :class 'fast-noise-lite :bind
  "get_domain_warp_frequency" :hash 1740695150)
 float)

(defgmethod
 (fast-noise-lite+set-domain-warp-fractal-type :class 'fast-noise-lite :bind
  "set_domain_warp_fractal_type" :hash 3999408287)
 :void (domain-warp-fractal-type fast-noise-lite+domain-warp-fractal-type))

(defgmethod
 (fast-noise-lite+get-domain-warp-fractal-type :class 'fast-noise-lite :bind
  "get_domain_warp_fractal_type" :hash 407716934)
 fast-noise-lite+domain-warp-fractal-type)

(defgmethod
 (fast-noise-lite+set-domain-warp-fractal-octaves :class 'fast-noise-lite :bind
  "set_domain_warp_fractal_octaves" :hash 1286410249)
 :void (domain-warp-octave-count int))

(defgmethod
 (fast-noise-lite+get-domain-warp-fractal-octaves :class 'fast-noise-lite :bind
  "get_domain_warp_fractal_octaves" :hash 3905245786)
 int)

(defgmethod
 (fast-noise-lite+set-domain-warp-fractal-lacunarity :class 'fast-noise-lite
  :bind "set_domain_warp_fractal_lacunarity" :hash 373806689)
 :void (domain-warp-lacunarity float))

(defgmethod
 (fast-noise-lite+get-domain-warp-fractal-lacunarity :class 'fast-noise-lite
  :bind "get_domain_warp_fractal_lacunarity" :hash 1740695150)
 float)

(defgmethod
 (fast-noise-lite+set-domain-warp-fractal-gain :class 'fast-noise-lite :bind
  "set_domain_warp_fractal_gain" :hash 373806689)
 :void (domain-warp-gain float))

(defgmethod
 (fast-noise-lite+get-domain-warp-fractal-gain :class 'fast-noise-lite :bind
  "get_domain_warp_fractal_gain" :hash 1740695150)
 float)