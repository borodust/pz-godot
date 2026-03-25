(common-lisp:in-package :%godot)


(defgproperty compositor-effect+enabled 'compositor-effect :get
 'compositor-effect+get-enabled :set 'compositor-effect+set-enabled)

(defgproperty compositor-effect+effect-callback-type 'compositor-effect :get
 'compositor-effect+get-effect-callback-type :set
 'compositor-effect+set-effect-callback-type)

(defgproperty compositor-effect+access-resolved-color 'compositor-effect :get
 'compositor-effect+get-access-resolved-color :set
 'compositor-effect+set-access-resolved-color)

(defgproperty compositor-effect+access-resolved-depth 'compositor-effect :get
 'compositor-effect+get-access-resolved-depth :set
 'compositor-effect+set-access-resolved-depth)

(defgproperty compositor-effect+needs-motion-vectors 'compositor-effect :get
 'compositor-effect+get-needs-motion-vectors :set
 'compositor-effect+set-needs-motion-vectors)

(defgproperty compositor-effect+needs-normal-roughness 'compositor-effect :get
 'compositor-effect+get-needs-normal-roughness :set
 'compositor-effect+set-needs-normal-roughness)

(defgproperty compositor-effect+needs-separate-specular 'compositor-effect :get
 'compositor-effect+get-needs-separate-specular :set
 'compositor-effect+set-needs-separate-specular)