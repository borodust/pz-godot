(common-lisp:in-package :%godot)


(defgproperty animation-mixer+active 'animation-mixer :get
 'animation-mixer+is-active :set 'animation-mixer+set-active)

(defgproperty animation-mixer+deterministic 'animation-mixer :get
 'animation-mixer+is-deterministic :set 'animation-mixer+set-deterministic)

(defgproperty animation-mixer+reset-on-save 'animation-mixer :get
 'animation-mixer+is-reset-on-save-enabled :set
 'animation-mixer+set-reset-on-save-enabled)

(defgproperty animation-mixer+root-node 'animation-mixer :get
 'animation-mixer+get-root-node :set 'animation-mixer+set-root-node)

(defgproperty animation-mixer+root-motion-track 'animation-mixer :get
 'animation-mixer+get-root-motion-track :set
 'animation-mixer+set-root-motion-track)

(defgproperty animation-mixer+root-motion-local 'animation-mixer :get
 'animation-mixer+is-root-motion-local :set
 'animation-mixer+set-root-motion-local)

(defgproperty animation-mixer+audio-max-polyphony 'animation-mixer :get
 'animation-mixer+get-audio-max-polyphony :set
 'animation-mixer+set-audio-max-polyphony)

(defgproperty animation-mixer+callback-mode-process 'animation-mixer :get
 'animation-mixer+get-callback-mode-process :set
 'animation-mixer+set-callback-mode-process)

(defgproperty animation-mixer+callback-mode-method 'animation-mixer :get
 'animation-mixer+get-callback-mode-method :set
 'animation-mixer+set-callback-mode-method)

(defgproperty animation-mixer+callback-mode-discrete 'animation-mixer :get
 'animation-mixer+get-callback-mode-discrete :set
 'animation-mixer+set-callback-mode-discrete)