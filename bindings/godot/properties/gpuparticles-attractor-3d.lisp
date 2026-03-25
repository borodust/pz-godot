(common-lisp:in-package :%godot)


(defgproperty gpuparticles-attractor-3d+strength 'gpuparticles-attractor-3d
 :get 'gpuparticles-attractor-3d+get-strength :set
 'gpuparticles-attractor-3d+set-strength)

(defgproperty gpuparticles-attractor-3d+attenuation 'gpuparticles-attractor-3d
 :get 'gpuparticles-attractor-3d+get-attenuation :set
 'gpuparticles-attractor-3d+set-attenuation)

(defgproperty gpuparticles-attractor-3d+directionality
 'gpuparticles-attractor-3d :get 'gpuparticles-attractor-3d+get-directionality
 :set 'gpuparticles-attractor-3d+set-directionality)

(defgproperty gpuparticles-attractor-3d+cull-mask 'gpuparticles-attractor-3d
 :get 'gpuparticles-attractor-3d+get-cull-mask :set
 'gpuparticles-attractor-3d+set-cull-mask)