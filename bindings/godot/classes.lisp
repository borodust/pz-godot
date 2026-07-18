(common-lisp:in-package :%godot)


(defgclass (aescontext :bind "AESContext" :api :core :refcounted common-lisp:t))


(defgenum (aescontext+mode :class 'aescontext) (:ecb-encrypt 0)
 (:ecb-decrypt 1) (:cbc-encrypt 2) (:cbc-decrypt 3) (:max 4))

(defgclass (astar-2d :bind "AStar2D" :api :core :refcounted common-lisp:t))

(defgclass (astar-3d :bind "AStar3D" :api :core :refcounted common-lisp:t))

(defgclass
 (astar-grid-2d :bind "AStarGrid2D" :api :core :refcounted common-lisp:t))


(defgenum (astar-grid-2d+heuristic :class 'astar-grid-2d) (:euclidean 0)
 (:manhattan 1) (:octile 2) (:chebyshev 3) (:max 4))


(defgenum (astar-grid-2d+diagonal-mode :class 'astar-grid-2d) (:always 0)
 (:never 1) (:at-least-one-walkable 2) (:only-if-no-obstacles 3) (:max 4))


(defgenum (astar-grid-2d+cell-shape :class 'astar-grid-2d) (:square 0)
 (:isometric-right 1) (:isometric-down 2) (:max 3))

(defgclass (accept-dialog :bind "AcceptDialog" :api :core)
 (:signals (confirmed) (canceled) (custom-action action string-name)))

(defgclass
 (accessibility-server :bind "AccessibilityServer" :api :core :instantiable
  common-lisp:nil))


(defgenum
 (accessibility-server+accessibility-role :class 'accessibility-server)
 (:unknown 0) (:default-button 1) (:audio 2) (:video 3) (:static-text 4)
 (:container 5) (:panel 6) (:button 7) (:link 8) (:check-box 9)
 (:radio-button 10) (:check-button 11) (:scroll-bar 12) (:scroll-view 13)
 (:splitter 14) (:slider 15) (:spin-button 16) (:progress-indicator 17)
 (:text-field 18) (:multiline-text-field 19) (:color-picker 20) (:table 21)
 (:cell 22) (:row 23) (:row-group 24) (:row-header 25) (:column-header 26)
 (:tree 27) (:tree-item 28) (:list 29) (:list-item 30) (:list-box 31)
 (:list-box-option 32) (:tab-bar 33) (:tab 34) (:tab-panel 35) (:menu-bar 36)
 (:menu 37) (:menu-item 38) (:menu-item-check-box 39) (:menu-item-radio 40)
 (:image 41) (:window 42) (:title-bar 43) (:dialog 44) (:tooltip 45)
 (:region 46) (:text-run 47))


(defgenum
 (accessibility-server+accessibility-popup-type :class 'accessibility-server)
 (:menu 0) (:list 1) (:tree 2) (:dialog 3))


(defgenum
 (accessibility-server+accessibility-flags :class 'accessibility-server)
 (:hidden 0) (:multiselectable 1) (:required 2) (:visited 3) (:busy 4)
 (:modal 5) (:touch-passthrough 6) (:readonly 7) (:disabled 8)
 (:clips-children 9))


(defgenum
 (accessibility-server+accessibility-action :class 'accessibility-server)
 (:click 0) (:focus 1) (:blur 2) (:collapse 3) (:expand 4) (:decrement 5)
 (:increment 6) (:hide-tooltip 7) (:show-tooltip 8) (:set-text-selection 9)
 (:replace-selected-text 10) (:scroll-backward 11) (:scroll-down 12)
 (:scroll-forward 13) (:scroll-left 14) (:scroll-right 15) (:scroll-up 16)
 (:scroll-into-view 17) (:scroll-to-point 18) (:set-scroll-offset 19)
 (:set-value 20) (:show-context-menu 21) (:custom 22))


(defgenum
 (accessibility-server+accessibility-live-mode :class 'accessibility-server)
 (:off 0) (:polite 1) (:assertive 2))


(defgenum
 (accessibility-server+accessibility-scroll-unit :class 'accessibility-server)
 (:item 0) (:page 1))


(defgenum
 (accessibility-server+accessibility-scroll-hint :class 'accessibility-server)
 (:top-left 0) (:bottom-right 1) (:top-edge 2) (:bottom-edge 3) (:left-edge 4)
 (:right-edge 5))

(defgclass (aim-modifier-3d :bind "AimModifier3D" :api :core))

(defgclass (animatable-body-2d :bind "AnimatableBody2D" :api :core))

(defgclass (animatable-body-3d :bind "AnimatableBody3D" :api :core))

(defgclass (animated-sprite-2d :bind "AnimatedSprite2D" :api :core)
 (:signals (sprite-frames-changed) (animation-changed) (frame-changed)
  (animation-looped) (animation-finished)))

(defgclass (animated-sprite-3d :bind "AnimatedSprite3D" :api :core)
 (:signals (sprite-frames-changed) (animation-changed) (frame-changed)
  (animation-looped) (animation-finished)))

(defgclass
 (animated-texture :bind "AnimatedTexture" :api :core :refcounted
  common-lisp:t))


(defgconstant +animated-texture+max-frames+ :value 256 :bind "MAX_FRAMES"
 :class 'animated-texture)

(defgclass (animation :bind "Animation" :api :core :refcounted common-lisp:t))


(defgenum (animation+track-type :class 'animation) (:value 0) (:position-3d 1)
 (:rotation-3d 2) (:scale-3d 3) (:blend-shape 4) (:method 5) (:bezier 6)
 (:audio 7) (:animation 8))


(defgenum (animation+interpolation-type :class 'animation) (:nearest 0)
 (:linear 1) (:cubic 2) (:linear-angle 3) (:cubic-angle 4))


(defgenum (animation+update-mode :class 'animation) (:continuous 0)
 (:discrete 1) (:capture 2))


(defgenum (animation+loop-mode :class 'animation) (:none 0) (:linear 1)
 (:pingpong 2))


(defgenum (animation+looped-flag :class 'animation) (:none 0) (:end 1)
 (:start 2))


(defgenum (animation+find-mode :class 'animation) (:nearest 0) (:approx 1)
 (:exact 2))

(defgclass
 (animation-library :bind "AnimationLibrary" :api :core :refcounted
  common-lisp:t)
 (:signals (animation-added anim-name string-name)
  (animation-removed anim-name string-name)
  (animation-renamed old-name string-name new-name string-name)
  (animation-changed anim-name string-name)))

(defgclass
 (animation-mixer :bind "AnimationMixer" :api :core :instantiable
  common-lisp:nil)
 (:signals (animation-list-changed) (animation-libraries-updated)
  (animation-finished anim-name string-name)
  (animation-started anim-name string-name) (caches-cleared) (mixer-applied)
  (mixer-updated)))


(defgenum
 (animation-mixer+animation-callback-mode-process :class 'animation-mixer)
 (:physics 0) (:idle 1) (:manual 2))


(defgenum
 (animation-mixer+animation-callback-mode-method :class 'animation-mixer)
 (:deferred 0) (:immediate 1))


(defgenum
 (animation-mixer+animation-callback-mode-discrete :class 'animation-mixer)
 (:dominant 0) (:recessive 1) (:force-continuous 2))

(defgclass
 (animation-node :bind "AnimationNode" :api :core :refcounted common-lisp:t)
 (:signals (tree-changed) (node-updated object-id int)
  (animation-node-renamed object-id int old-name string new-name string)
  (animation-node-removed object-id int node-name string)))


(defgenum (animation-node+filter-action :class 'animation-node) (:ignore 0)
 (:pass 1) (:stop 2) (:blend 3))

(defgclass
 (animation-node-add-2 :bind "AnimationNodeAdd2" :api :core :refcounted
  common-lisp:t))

(defgclass
 (animation-node-add-3 :bind "AnimationNodeAdd3" :api :core :refcounted
  common-lisp:t))

(defgclass
 (animation-node-animation :bind "AnimationNodeAnimation" :api :core
  :refcounted common-lisp:t))


(defgenum (animation-node-animation+play-mode :class 'animation-node-animation)
 (:forward 0) (:backward 1))

(defgclass
 (animation-node-blend-2 :bind "AnimationNodeBlend2" :api :core :refcounted
  common-lisp:t))

(defgclass
 (animation-node-blend-3 :bind "AnimationNodeBlend3" :api :core :refcounted
  common-lisp:t))

(defgclass
 (animation-node-blend-space-1d :bind "AnimationNodeBlendSpace1D" :api :core
  :refcounted common-lisp:t))


(defgenum
 (animation-node-blend-space-1d+blend-mode :class
  'animation-node-blend-space-1d)
 (:interpolated 0) (:discrete 1) (:discrete-carry 2))


(defgenum
 (animation-node-blend-space-1d+sync-mode :class
  'animation-node-blend-space-1d)
 (:none 0) (:independent 1) (:cyclic-mutable 2) (:cyclic-constant 3))

(defgclass
 (animation-node-blend-space-2d :bind "AnimationNodeBlendSpace2D" :api :core
  :refcounted common-lisp:t)
 (:signals (triangles-updated)))


(defgenum
 (animation-node-blend-space-2d+blend-mode :class
  'animation-node-blend-space-2d)
 (:interpolated 0) (:discrete 1) (:discrete-carry 2))


(defgenum
 (animation-node-blend-space-2d+sync-mode :class
  'animation-node-blend-space-2d)
 (:none 0) (:independent 1) (:cyclic-mutable 2) (:cyclic-constant 3))

(defgclass
 (animation-node-blend-tree :bind "AnimationNodeBlendTree" :api :core
  :refcounted common-lisp:t)
 (:signals (node-changed node-name string-name)))


(defgconstant +animation-node-blend-tree+connection-ok+ :value 0 :bind
 "CONNECTION_OK" :class 'animation-node-blend-tree)


(defgconstant +animation-node-blend-tree+connection-error-no-input+ :value 1
 :bind "CONNECTION_ERROR_NO_INPUT" :class 'animation-node-blend-tree)


(defgconstant +animation-node-blend-tree+connection-error-no-input-index+
 :value 2 :bind "CONNECTION_ERROR_NO_INPUT_INDEX" :class
 'animation-node-blend-tree)


(defgconstant +animation-node-blend-tree+connection-error-no-output+ :value 3
 :bind "CONNECTION_ERROR_NO_OUTPUT" :class 'animation-node-blend-tree)


(defgconstant +animation-node-blend-tree+connection-error-same-node+ :value 4
 :bind "CONNECTION_ERROR_SAME_NODE" :class 'animation-node-blend-tree)


(defgconstant +animation-node-blend-tree+connection-error-connection-exists+
 :value 5 :bind "CONNECTION_ERROR_CONNECTION_EXISTS" :class
 'animation-node-blend-tree)

(defgclass
 (animation-node-extension :bind "AnimationNodeExtension" :api :core
  :refcounted common-lisp:t))

(defgclass
 (animation-node-one-shot :bind "AnimationNodeOneShot" :api :core :refcounted
  common-lisp:t))


(defgenum
 (animation-node-one-shot+one-shot-request :class 'animation-node-one-shot)
 (:none 0) (:fire 1) (:abort 2) (:fade-out 3))


(defgenum (animation-node-one-shot+mix-mode :class 'animation-node-one-shot)
 (:blend 0) (:add 1))

(defgclass
 (animation-node-output :bind "AnimationNodeOutput" :api :core :refcounted
  common-lisp:t))

(defgclass
 (animation-node-state-machine :bind "AnimationNodeStateMachine" :api :core
  :refcounted common-lisp:t))


(defgenum
 (animation-node-state-machine+state-machine-type :class
  'animation-node-state-machine)
 (:root 0) (:nested 1) (:grouped 2))

(defgclass
 (animation-node-state-machine-playback :bind
  "AnimationNodeStateMachinePlayback" :api :core :refcounted common-lisp:t)
 (:signals (state-started state string-name)
  (state-finished state string-name)))

(defgclass
 (animation-node-state-machine-transition :bind
  "AnimationNodeStateMachineTransition" :api :core :refcounted common-lisp:t)
 (:signals (advance-condition-changed)))


(defgenum
 (animation-node-state-machine-transition+switch-mode :class
  'animation-node-state-machine-transition)
 (:immediate 0) (:sync 1) (:at-end 2))


(defgenum
 (animation-node-state-machine-transition+advance-mode :class
  'animation-node-state-machine-transition)
 (:disabled 0) (:enabled 1) (:auto 2))

(defgclass
 (animation-node-sub-2 :bind "AnimationNodeSub2" :api :core :refcounted
  common-lisp:t))

(defgclass
 (animation-node-sync :bind "AnimationNodeSync" :api :core :refcounted
  common-lisp:t))

(defgclass
 (animation-node-time-scale :bind "AnimationNodeTimeScale" :api :core
  :refcounted common-lisp:t))

(defgclass
 (animation-node-time-seek :bind "AnimationNodeTimeSeek" :api :core :refcounted
  common-lisp:t))

(defgclass
 (animation-node-transition :bind "AnimationNodeTransition" :api :core
  :refcounted common-lisp:t))

(defgclass (animation-player :bind "AnimationPlayer" :api :core)
 (:signals (current-animation-changed anim-name string-name)
  (animation-changed old-name string-name new-name string-name)))


(defgenum
 (animation-player+animation-process-callback :class 'animation-player)
 (:physics 0) (:idle 1) (:manual 2))


(defgenum
 (animation-player+animation-method-call-mode :class 'animation-player)
 (:deferred 0) (:immediate 1))

(defgclass
 (animation-root-node :bind "AnimationRootNode" :api :core :refcounted
  common-lisp:t))

(defgclass (animation-tree :bind "AnimationTree" :api :core)
 (:signals (animation-player-changed)))


(defgenum (animation-tree+animation-process-callback :class 'animation-tree)
 (:physics 0) (:idle 1) (:manual 2))

(defgclass (area-2d :bind "Area2D" :api :core)
 (:signals
  (body-shape-entered body-rid rid body node-2d body-shape-index int
   local-shape-index int)
  (body-shape-exited body-rid rid body node-2d body-shape-index int
   local-shape-index int)
  (body-entered body node-2d) (body-exited body node-2d)
  (area-shape-entered area-rid rid area area-2d area-shape-index int
   local-shape-index int)
  (area-shape-exited area-rid rid area area-2d area-shape-index int
   local-shape-index int)
  (area-entered area area-2d) (area-exited area area-2d)))


(defgenum (area-2d+space-override :class 'area-2d) (:disabled 0) (:combine 1)
 (:combine-replace 2) (:replace 3) (:replace-combine 4))

(defgclass (area-3d :bind "Area3D" :api :core)
 (:signals
  (body-shape-entered body-rid rid body node-3d body-shape-index int
   local-shape-index int)
  (body-shape-exited body-rid rid body node-3d body-shape-index int
   local-shape-index int)
  (body-entered body node-3d) (body-exited body node-3d)
  (area-shape-entered area-rid rid area area-3d area-shape-index int
   local-shape-index int)
  (area-shape-exited area-rid rid area area-3d area-shape-index int
   local-shape-index int)
  (area-entered area area-3d) (area-exited area area-3d)))


(defgenum (area-3d+space-override :class 'area-3d) (:disabled 0) (:combine 1)
 (:combine-replace 2) (:replace 3) (:replace-combine 4))

(defgclass (area-light-3d :bind "AreaLight3D" :api :core))

(defgclass (array-mesh :bind "ArrayMesh" :api :core :refcounted common-lisp:t))

(defgclass
 (array-occluder-3d :bind "ArrayOccluder3D" :api :core :refcounted
  common-lisp:t))

(defgclass (aspect-ratio-container :bind "AspectRatioContainer" :api :core))


(defgenum (aspect-ratio-container+stretch-mode :class 'aspect-ratio-container)
 (:width-controls-height 0) (:height-controls-width 1) (:fit 2) (:cover 3))


(defgenum
 (aspect-ratio-container+alignment-mode :class 'aspect-ratio-container)
 (:begin 0) (:center 1) (:end 2))

(defgclass
 (atlas-texture :bind "AtlasTexture" :api :core :refcounted common-lisp:t))

(defgclass
 (audio-bus-layout :bind "AudioBusLayout" :api :core :refcounted common-lisp:t))

(defgclass
 (audio-effect :bind "AudioEffect" :api :core :refcounted common-lisp:t))

(defgclass
 (audio-effect-amplify :bind "AudioEffectAmplify" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-band-limit-filter :bind "AudioEffectBandLimitFilter" :api :core
  :refcounted common-lisp:t))

(defgclass
 (audio-effect-band-pass-filter :bind "AudioEffectBandPassFilter" :api :core
  :refcounted common-lisp:t))

(defgclass
 (audio-effect-capture :bind "AudioEffectCapture" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-chorus :bind "AudioEffectChorus" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-compressor :bind "AudioEffectCompressor" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-delay :bind "AudioEffectDelay" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-distortion :bind "AudioEffectDistortion" :api :core :refcounted
  common-lisp:t))


(defgenum (audio-effect-distortion+mode :class 'audio-effect-distortion)
 (:clip 0) (:atan 1) (:lofi 2) (:overdrive 3) (:waveshape 4))

(defgclass
 (audio-effect-eq :bind "AudioEffectEQ" :api :core :refcounted common-lisp:t))

(defgclass
 (audio-effect-eq10 :bind "AudioEffectEQ10" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-eq21 :bind "AudioEffectEQ21" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-eq6 :bind "AudioEffectEQ6" :api :core :refcounted common-lisp:t))

(defgclass
 (audio-effect-filter :bind "AudioEffectFilter" :api :core :refcounted
  common-lisp:t))


(defgenum (audio-effect-filter+filter-db :class 'audio-effect-filter) (:6db 0)
 (:12db 1) (:18db 2) (:24db 3))

(defgclass
 (audio-effect-hard-limiter :bind "AudioEffectHardLimiter" :api :core
  :refcounted common-lisp:t))

(defgclass
 (audio-effect-high-pass-filter :bind "AudioEffectHighPassFilter" :api :core
  :refcounted common-lisp:t))

(defgclass
 (audio-effect-high-shelf-filter :bind "AudioEffectHighShelfFilter" :api :core
  :refcounted common-lisp:t))

(defgclass
 (audio-effect-instance :bind "AudioEffectInstance" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-limiter :bind "AudioEffectLimiter" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-low-pass-filter :bind "AudioEffectLowPassFilter" :api :core
  :refcounted common-lisp:t))

(defgclass
 (audio-effect-low-shelf-filter :bind "AudioEffectLowShelfFilter" :api :core
  :refcounted common-lisp:t))

(defgclass
 (audio-effect-notch-filter :bind "AudioEffectNotchFilter" :api :core
  :refcounted common-lisp:t))

(defgclass
 (audio-effect-panner :bind "AudioEffectPanner" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-phaser :bind "AudioEffectPhaser" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-pitch-shift :bind "AudioEffectPitchShift" :api :core :refcounted
  common-lisp:t))


(defgenum (audio-effect-pitch-shift+fftsize :class 'audio-effect-pitch-shift)
 (:|256| 0) (:|512| 1) (:|1024| 2) (:|2048| 3) (:|4096| 4) (:max 5))

(defgclass
 (audio-effect-record :bind "AudioEffectRecord" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-reverb :bind "AudioEffectReverb" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-effect-spectrum-analyzer :bind "AudioEffectSpectrumAnalyzer" :api :core
  :refcounted common-lisp:t))


(defgenum
 (audio-effect-spectrum-analyzer+fftsize :class
  'audio-effect-spectrum-analyzer)
 (:|256| 0) (:|512| 1) (:|1024| 2) (:|2048| 3) (:|4096| 4) (:max 5))

(defgclass
 (audio-effect-spectrum-analyzer-instance :bind
  "AudioEffectSpectrumAnalyzerInstance" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))


(defgenum
 (audio-effect-spectrum-analyzer-instance+magnitude-mode :class
  'audio-effect-spectrum-analyzer-instance)
 (:average 0) (:max 1))

(defgclass
 (audio-effect-stereo-enhance :bind "AudioEffectStereoEnhance" :api :core
  :refcounted common-lisp:t))

(defgclass (audio-listener-2d :bind "AudioListener2D" :api :core))

(defgclass (audio-listener-3d :bind "AudioListener3D" :api :core))


(defgenum (audio-listener-3d+doppler-tracking :class 'audio-listener-3d)
 (:disabled 0) (:idle-step 1) (:physics-step 2))

(defgclass
 (audio-sample :bind "AudioSample" :api :core :refcounted common-lisp:t))

(defgclass
 (audio-sample-playback :bind "AudioSamplePlayback" :api :core :refcounted
  common-lisp:t))

(defgclass (audio-server :bind "AudioServer" :api :core)
 (:signals (bus-layout-changed)
  (bus-renamed bus-index int old-name string-name new-name string-name)))


(defgenum (audio-server+speaker-mode :class 'audio-server) (:mode-stereo 0)
 (:surround-31 1) (:surround-51 2) (:surround-71 3))


(defgenum (audio-server+playback-type :class 'audio-server) (:default 0)
 (:stream 1) (:sample 2) (:max 3))

(defgclass
 (audio-stream :bind "AudioStream" :api :core :refcounted common-lisp:t)
 (:signals (parameter-list-changed)))

(defgclass
 (audio-stream-generator :bind "AudioStreamGenerator" :api :core :refcounted
  common-lisp:t))


(defgenum
 (audio-stream-generator+audio-stream-generator-mix-rate :class
  'audio-stream-generator)
 (:output 0) (:input 1) (:custom 2) (:max 3))

(defgclass
 (audio-stream-generator-playback :bind "AudioStreamGeneratorPlayback" :api
  :core :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (audio-stream-interactive :bind "AudioStreamInteractive" :api :core
  :refcounted common-lisp:t))


(defgconstant +audio-stream-interactive+clip-any+ :value -1 :bind "CLIP_ANY"
 :class 'audio-stream-interactive)


(defgenum
 (audio-stream-interactive+transition-from-time :class
  'audio-stream-interactive)
 (:immediate 0) (:next-beat 1) (:next-bar 2) (:end 3))


(defgenum
 (audio-stream-interactive+transition-to-time :class 'audio-stream-interactive)
 (:same-position 0) (:start 1) (:previous-position 2))


(defgenum (audio-stream-interactive+fade-mode :class 'audio-stream-interactive)
 (:disabled 0) (:in 1) (:out 2) (:cross 3) (:automatic 4))


(defgenum
 (audio-stream-interactive+auto-advance-mode :class 'audio-stream-interactive)
 (:disabled 0) (:enabled 1) (:return-to-hold 2))

(defgclass
 (audio-stream-mp3 :bind "AudioStreamMP3" :api :core :refcounted common-lisp:t))

(defgclass
 (audio-stream-microphone :bind "AudioStreamMicrophone" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-stream-ogg-vorbis :bind "AudioStreamOggVorbis" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-stream-playback :bind "AudioStreamPlayback" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-stream-playback-interactive :bind "AudioStreamPlaybackInteractive" :api
  :core :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (audio-stream-playback-ogg-vorbis :bind "AudioStreamPlaybackOggVorbis" :api
  :core :refcounted common-lisp:t))

(defgclass
 (audio-stream-playback-playlist :bind "AudioStreamPlaybackPlaylist" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (audio-stream-playback-polyphonic :bind "AudioStreamPlaybackPolyphonic" :api
  :core :instantiable common-lisp:nil :refcounted common-lisp:t))


(defgconstant +audio-stream-playback-polyphonic+invalid-id+ :value -1 :bind
 "INVALID_ID" :class 'audio-stream-playback-polyphonic)

(defgclass
 (audio-stream-playback-resampled :bind "AudioStreamPlaybackResampled" :api
  :core :refcounted common-lisp:t))

(defgclass
 (audio-stream-playback-synchronized :bind "AudioStreamPlaybackSynchronized"
  :api :core :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass (audio-stream-player :bind "AudioStreamPlayer" :api :core)
 (:signals (finished)))


(defgenum (audio-stream-player+mix-target :class 'audio-stream-player)
 (:stereo 0) (:surround 1) (:center 2))

(defgclass (audio-stream-player-2d :bind "AudioStreamPlayer2D" :api :core)
 (:signals (finished)))

(defgclass (audio-stream-player-3d :bind "AudioStreamPlayer3D" :api :core)
 (:signals (finished)))


(defgenum
 (audio-stream-player-3d+attenuation-model :class 'audio-stream-player-3d)
 (:inverse-distance 0) (:inverse-square-distance 1) (:logarithmic 2)
 (:disabled 3))


(defgenum
 (audio-stream-player-3d+doppler-tracking :class 'audio-stream-player-3d)
 (:disabled 0) (:idle-step 1) (:physics-step 2))

(defgclass
 (audio-stream-playlist :bind "AudioStreamPlaylist" :api :core :refcounted
  common-lisp:t))


(defgconstant +audio-stream-playlist+max-streams+ :value 64 :bind "MAX_STREAMS"
 :class 'audio-stream-playlist)

(defgclass
 (audio-stream-polyphonic :bind "AudioStreamPolyphonic" :api :core :refcounted
  common-lisp:t))

(defgclass
 (audio-stream-randomizer :bind "AudioStreamRandomizer" :api :core :refcounted
  common-lisp:t))


(defgenum
 (audio-stream-randomizer+playback-mode :class 'audio-stream-randomizer)
 (:random-no-repeats 0) (:random 1) (:sequential 2))

(defgclass
 (audio-stream-synchronized :bind "AudioStreamSynchronized" :api :core
  :refcounted common-lisp:t))


(defgconstant +audio-stream-synchronized+max-streams+ :value 32 :bind
 "MAX_STREAMS" :class 'audio-stream-synchronized)

(defgclass
 (audio-stream-wav :bind "AudioStreamWAV" :api :core :refcounted common-lisp:t))


(defgenum (audio-stream-wav+format :class 'audio-stream-wav) (:8-bits 0)
 (:16-bits 1) (:ima-adpcm 2) (:qoa 3))


(defgenum (audio-stream-wav+loop-mode :class 'audio-stream-wav) (:disabled 0)
 (:forward 1) (:pingpong 2) (:backward 3))

(defgclass
 (await-tweener :bind "AwaitTweener" :api :core :refcounted common-lisp:t))

(defgclass (back-buffer-copy :bind "BackBufferCopy" :api :core))


(defgenum (back-buffer-copy+copy-mode :class 'back-buffer-copy) (:disabled 0)
 (:rect 1) (:viewport 2))

(defgclass (base-button :bind "BaseButton" :api :core)
 (:signals (pressed) (button-up) (button-down) (toggled toggled-on bool)))


(defgenum (base-button+draw-mode :class 'base-button) (:normal 0) (:pressed 1)
 (:hover 2) (:disabled 3) (:hover-pressed 4))


(defgenum (base-button+action-mode :class 'base-button) (:press 0) (:release 1))

(defgclass
 (base-material-3d :bind "BaseMaterial3D" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))


(defgenum (base-material-3d+texture-param :class 'base-material-3d) (:albedo 0)
 (:metallic 1) (:roughness 2) (:emission 3) (:normal 4) (:bent-normal 18)
 (:rim 5) (:clearcoat 6) (:flowmap 7) (:ambient-occlusion 8) (:heightmap 9)
 (:subsurface-scattering 10) (:subsurface-transmittance 11) (:backlight 12)
 (:refraction 13) (:detail-mask 14) (:detail-albedo 15) (:detail-normal 16)
 (:orm 17) (:max 19))


(defgenum (base-material-3d+texture-filter :class 'base-material-3d)
 (:nearest 0) (:linear 1) (:nearest-with-mipmaps 2) (:linear-with-mipmaps 3)
 (:nearest-with-mipmaps-anisotropic 4) (:linear-with-mipmaps-anisotropic 5)
 (:max 6))


(defgenum (base-material-3d+detail-uv :class 'base-material-3d) (:|1| 0)
 (:|2| 1))


(defgenum (base-material-3d+transparency :class 'base-material-3d)
 (:disabled 0) (:alpha 1) (:alpha-scissor 2) (:alpha-hash 3)
 (:alpha-depth-pre-pass 4) (:max 5))


(defgenum (base-material-3d+shading-mode :class 'base-material-3d)
 (:unshaded 0) (:per-pixel 1) (:per-vertex 2) (:max 3))


(defgenum (base-material-3d+feature :class 'base-material-3d) (:emission 0)
 (:normal-mapping 1) (:rim 2) (:clearcoat 3) (:anisotropy 4)
 (:ambient-occlusion 5) (:height-mapping 6) (:subsurface-scattering 7)
 (:subsurface-transmittance 8) (:backlight 9) (:refraction 10) (:detail 11)
 (:bent-normal-mapping 12) (:max 13))


(defgenum (base-material-3d+blend-mode :class 'base-material-3d) (:mix 0)
 (:add 1) (:sub 2) (:mul 3) (:premult-alpha 4))


(defgenum (base-material-3d+alpha-anti-aliasing :class 'base-material-3d)
 (:off 0) (:alpha-to-coverage 1) (:alpha-to-coverage-and-to-one 2))


(defgenum (base-material-3d+depth-draw-mode :class 'base-material-3d)
 (:opaque-only 0) (:always 1) (:disabled 2))


(defgenum (base-material-3d+depth-test :class 'base-material-3d) (:default 0)
 (:inverted 1))


(defgenum (base-material-3d+cull-mode :class 'base-material-3d) (:back 0)
 (:front 1) (:disabled 2))


(defgenum (base-material-3d+flags :class 'base-material-3d)
 (:disable-depth-test 0) (:albedo-from-vertex-color 1) (:srgb-vertex-color 2)
 (:use-point-size 3) (:fixed-size 4) (:billboard-keep-scale 5)
 (:uv1-use-triplanar 6) (:uv2-use-triplanar 7) (:uv1-use-world-triplanar 8)
 (:uv2-use-world-triplanar 9) (:ao-on-uv2 10) (:emission-on-uv2 11)
 (:albedo-texture-force-srgb 12) (:dont-receive-shadows 13)
 (:disable-ambient-light 14) (:use-shadow-to-opacity 15)
 (:use-texture-repeat 16) (:invert-heightmap 17) (:subsurface-mode-skin 18)
 (:particle-trails-mode 19) (:albedo-texture-msdf 20) (:disable-fog 21)
 (:disable-specular-occlusion 22) (:use-z-clip-scale 23) (:use-fov-override 24)
 (:max 25))


(defgenum (base-material-3d+diffuse-mode :class 'base-material-3d) (:burley 0)
 (:lambert 1) (:lambert-wrap 2) (:toon 3))


(defgenum (base-material-3d+specular-mode :class 'base-material-3d)
 (:schlick-ggx 0) (:toon 1) (:disabled 2))


(defgenum (base-material-3d+billboard-mode :class 'base-material-3d)
 (:disabled 0) (:enabled 1) (:fixed-y 2) (:particles 3))


(defgenum (base-material-3d+texture-channel :class 'base-material-3d) (:red 0)
 (:green 1) (:blue 2) (:alpha 3) (:grayscale 4))


(defgenum (base-material-3d+emission-operator :class 'base-material-3d)
 (:add 0) (:multiply 1))


(defgenum (base-material-3d+distance-fade-mode :class 'base-material-3d)
 (:disabled 0) (:pixel-alpha 1) (:pixel-dither 2) (:object-dither 3))


(defgenum (base-material-3d+stencil-mode :class 'base-material-3d)
 (:disabled 0) (:outline 1) (:xray 2) (:custom 3))


(defgenum (base-material-3d+stencil-flags :class 'base-material-3d) (:read 1)
 (:write 2) (:write-depth-fail 4))


(defgenum (base-material-3d+stencil-compare :class 'base-material-3d)
 (:always 0) (:less 1) (:equal 2) (:less-or-equal 3) (:greater 4)
 (:not-equal 5) (:greater-or-equal 6))

(defgclass (bit-map :bind "BitMap" :api :core :refcounted common-lisp:t))

(defgclass
 (blit-material :bind "BlitMaterial" :api :core :refcounted common-lisp:t))


(defgenum (blit-material+blend-mode :class 'blit-material) (:mix 0) (:add 1)
 (:sub 2) (:mul 3) (:disabled 4))

(defgclass (bone-2d :bind "Bone2D" :api :core))

(defgclass (bone-attachment-3d :bind "BoneAttachment3D" :api :core))

(defgclass (bone-constraint-3d :bind "BoneConstraint3D" :api :core))


(defgenum (bone-constraint-3d+reference-type :class 'bone-constraint-3d)
 (:bone 0) (:node 1))

(defgclass (bone-map :bind "BoneMap" :api :core :refcounted common-lisp:t)
 (:signals (bone-map-updated) (profile-updated)))

(defgclass (bone-twist-disperser-3d :bind "BoneTwistDisperser3D" :api :core))


(defgenum
 (bone-twist-disperser-3d+disperse-mode :class 'bone-twist-disperser-3d)
 (:even 0) (:weighted 1) (:custom 2))

(defgclass (box-container :bind "BoxContainer" :api :core))


(defgenum (box-container+alignment-mode :class 'box-container) (:begin 0)
 (:center 1) (:end 2))

(defgclass (box-mesh :bind "BoxMesh" :api :core :refcounted common-lisp:t))

(defgclass
 (box-occluder-3d :bind "BoxOccluder3D" :api :core :refcounted common-lisp:t))

(defgclass
 (box-shape-3d :bind "BoxShape3D" :api :core :refcounted common-lisp:t))

(defgclass (button :bind "Button" :api :core))

(defgclass
 (button-group :bind "ButtonGroup" :api :core :refcounted common-lisp:t)
 (:signals (pressed button base-button)))

(defgclass (ccdik3d :bind "CCDIK3D" :api :core))

(defgclass (cpuparticles-2d :bind "CPUParticles2D" :api :core)
 (:signals (finished)))


(defgenum (cpuparticles-2d+draw-order :class 'cpuparticles-2d) (:index 0)
 (:lifetime 1))


(defgenum (cpuparticles-2d+parameter :class 'cpuparticles-2d)
 (:initial-linear-velocity 0) (:angular-velocity 1) (:orbit-velocity 2)
 (:linear-accel 3) (:radial-accel 4) (:tangential-accel 5) (:damping 6)
 (:angle 7) (:scale 8) (:hue-variation 9) (:anim-speed 10) (:anim-offset 11)
 (:max 12))


(defgenum (cpuparticles-2d+particle-flags :class 'cpuparticles-2d)
 (:align-y-to-velocity 0) (:rotate-y 1) (:disable-z 2) (:max 3))


(defgenum (cpuparticles-2d+emission-shape :class 'cpuparticles-2d) (:point 0)
 (:sphere 1) (:sphere-surface 2) (:rectangle 3) (:points 4)
 (:directed-points 5) (:ring 6) (:max 7))

(defgclass (cpuparticles-3d :bind "CPUParticles3D" :api :core)
 (:signals (finished)))


(defgenum (cpuparticles-3d+draw-order :class 'cpuparticles-3d) (:index 0)
 (:lifetime 1) (:view-depth 2))


(defgenum (cpuparticles-3d+parameter :class 'cpuparticles-3d)
 (:initial-linear-velocity 0) (:angular-velocity 1) (:orbit-velocity 2)
 (:linear-accel 3) (:radial-accel 4) (:tangential-accel 5) (:damping 6)
 (:angle 7) (:scale 8) (:hue-variation 9) (:anim-speed 10) (:anim-offset 11)
 (:max 12))


(defgenum (cpuparticles-3d+particle-flags :class 'cpuparticles-3d)
 (:align-y-to-velocity 0) (:rotate-y 1) (:disable-z 2) (:max 3))


(defgenum (cpuparticles-3d+emission-shape :class 'cpuparticles-3d) (:point 0)
 (:sphere 1) (:sphere-surface 2) (:box 3) (:points 4) (:directed-points 5)
 (:ring 6) (:max 7))

(defgclass (csgbox-3d :bind "CSGBox3D" :api :core))

(defgclass (csgcombiner-3d :bind "CSGCombiner3D" :api :core))

(defgclass (csgcylinder-3d :bind "CSGCylinder3D" :api :core))

(defgclass (csgmesh-3d :bind "CSGMesh3D" :api :core))

(defgclass (csgpolygon-3d :bind "CSGPolygon3D" :api :core))


(defgenum (csgpolygon-3d+mode :class 'csgpolygon-3d) (:depth 0) (:spin 1)
 (:path 2))


(defgenum (csgpolygon-3d+path-rotation :class 'csgpolygon-3d) (:polygon 0)
 (:path 1) (:path-follow 2))


(defgenum (csgpolygon-3d+path-interval-type :class 'csgpolygon-3d)
 (:distance 0) (:subdivide 1))

(defgclass
 (csgprimitive-3d :bind "CSGPrimitive3D" :api :core :instantiable
  common-lisp:nil))

(defgclass
 (csgshape-3d :bind "CSGShape3D" :api :core :instantiable common-lisp:nil))


(defgenum (csgshape-3d+operation :class 'csgshape-3d) (:union 0)
 (:intersection 1) (:subtraction 2))

(defgclass (csgsphere-3d :bind "CSGSphere3D" :api :core))

(defgclass (csgtorus-3d :bind "CSGTorus3D" :api :core))

(defgclass
 (callback-tweener :bind "CallbackTweener" :api :core :refcounted
  common-lisp:t))

(defgclass (camera-2d :bind "Camera2D" :api :core))


(defgenum (camera-2d+anchor-mode :class 'camera-2d) (:fixed-top-left 0)
 (:drag-center 1))


(defgenum (camera-2d+camera-2dprocess-callback :class 'camera-2d) (:physics 0)
 (:idle 1))

(defgclass (camera-3d :bind "Camera3D" :api :core))


(defgenum (camera-3d+projection-type :class 'camera-3d) (:perspective 0)
 (:orthogonal 1) (:frustum 2))


(defgenum (camera-3d+keep-aspect :class 'camera-3d) (:width 0) (:height 1))


(defgenum (camera-3d+doppler-tracking :class 'camera-3d) (:disabled 0)
 (:idle-step 1) (:physics-step 2))

(defgclass
 (camera-attributes :bind "CameraAttributes" :api :core :refcounted
  common-lisp:t))

(defgclass
 (camera-attributes-physical :bind "CameraAttributesPhysical" :api :core
  :refcounted common-lisp:t))

(defgclass
 (camera-attributes-practical :bind "CameraAttributesPractical" :api :core
  :refcounted common-lisp:t))

(defgclass
 (camera-feed :bind "CameraFeed" :api :core :refcounted common-lisp:t)
 (:signals (frame-changed) (format-changed)))


(defgenum (camera-feed+feed-data-type :class 'camera-feed) (:noimage 0)
 (:rgb 1) (:ycbcr 2) (:ycbcr-sep 3) (:external 4))


(defgenum (camera-feed+feed-position :class 'camera-feed) (:unspecified 0)
 (:front 1) (:back 2))

(defgclass (camera-server :bind "CameraServer" :api :core)
 (:signals (camera-feed-added id int) (camera-feed-removed id int)
  (camera-feeds-updated)))


(defgenum (camera-server+feed-image :class 'camera-server) (:rgba-image 0)
 (:ycbcr-image 0) (:y-image 0) (:cbcr-image 1))

(defgclass
 (camera-texture :bind "CameraTexture" :api :core :refcounted common-lisp:t))

(defgclass (canvas-group :bind "CanvasGroup" :api :core))

(defgclass
 (canvas-item :bind "CanvasItem" :api :core :instantiable common-lisp:nil)
 (:signals (draw) (visibility-changed) (hidden) (item-rect-changed)))


(defgconstant +canvas-item+notification-transform-changed+ :value 2000 :bind
 "NOTIFICATION_TRANSFORM_CHANGED" :class 'canvas-item)


(defgconstant +canvas-item+notification-local-transform-changed+ :value 35
 :bind "NOTIFICATION_LOCAL_TRANSFORM_CHANGED" :class 'canvas-item)


(defgconstant +canvas-item+notification-draw+ :value 30 :bind
 "NOTIFICATION_DRAW" :class 'canvas-item)


(defgconstant +canvas-item+notification-visibility-changed+ :value 31 :bind
 "NOTIFICATION_VISIBILITY_CHANGED" :class 'canvas-item)


(defgconstant +canvas-item+notification-enter-canvas+ :value 32 :bind
 "NOTIFICATION_ENTER_CANVAS" :class 'canvas-item)


(defgconstant +canvas-item+notification-exit-canvas+ :value 33 :bind
 "NOTIFICATION_EXIT_CANVAS" :class 'canvas-item)


(defgconstant +canvas-item+notification-world-2d-changed+ :value 36 :bind
 "NOTIFICATION_WORLD_2D_CHANGED" :class 'canvas-item)


(defgenum (canvas-item+texture-filter :class 'canvas-item) (:parent-node 0)
 (:nearest 1) (:linear 2) (:nearest-with-mipmaps 3) (:linear-with-mipmaps 4)
 (:nearest-with-mipmaps-anisotropic 5) (:linear-with-mipmaps-anisotropic 6)
 (:max 7))


(defgenum (canvas-item+texture-repeat :class 'canvas-item) (:parent-node 0)
 (:disabled 1) (:enabled 2) (:mirror 3) (:max 4))


(defgenum (canvas-item+clip-children-mode :class 'canvas-item) (:disabled 0)
 (:only 1) (:and-draw 2) (:max 3))


(defgenum (canvas-item+oversampling-with-scale :class 'canvas-item)
 (:parent-node 0) (:disabled 1) (:enabled 2) (:max 3))

(defgclass
 (canvas-item-material :bind "CanvasItemMaterial" :api :core :refcounted
  common-lisp:t))


(defgenum (canvas-item-material+blend-mode :class 'canvas-item-material)
 (:mix 0) (:add 1) (:sub 2) (:mul 3) (:premult-alpha 4))


(defgenum (canvas-item-material+light-mode :class 'canvas-item-material)
 (:normal 0) (:unshaded 1) (:light-only 2))

(defgclass (canvas-layer :bind "CanvasLayer" :api :core)
 (:signals (visibility-changed)))

(defgclass (canvas-modulate :bind "CanvasModulate" :api :core))

(defgclass
 (canvas-texture :bind "CanvasTexture" :api :core :refcounted common-lisp:t))

(defgclass
 (capsule-mesh :bind "CapsuleMesh" :api :core :refcounted common-lisp:t))

(defgclass
 (capsule-shape-2d :bind "CapsuleShape2D" :api :core :refcounted common-lisp:t))

(defgclass
 (capsule-shape-3d :bind "CapsuleShape3D" :api :core :refcounted common-lisp:t))

(defgclass (center-container :bind "CenterContainer" :api :core))

(defgclass
 (chain-ik3d :bind "ChainIK3D" :api :core :instantiable common-lisp:nil))

(defgclass
 (char-fxtransform :bind "CharFXTransform" :api :core :refcounted
  common-lisp:t))

(defgclass (character-body-2d :bind "CharacterBody2D" :api :core))


(defgenum (character-body-2d+motion-mode :class 'character-body-2d)
 (:grounded 0) (:floating 1))


(defgenum (character-body-2d+platform-on-leave :class 'character-body-2d)
 (:add-velocity 0) (:add-upward-velocity 1) (:do-nothing 2))

(defgclass (character-body-3d :bind "CharacterBody3D" :api :core))


(defgenum (character-body-3d+motion-mode :class 'character-body-3d)
 (:grounded 0) (:floating 1))


(defgenum (character-body-3d+platform-on-leave :class 'character-body-3d)
 (:add-velocity 0) (:add-upward-velocity 1) (:do-nothing 2))

(defgclass (check-box :bind "CheckBox" :api :core))

(defgclass (check-button :bind "CheckButton" :api :core))

(defgclass
 (circle-shape-2d :bind "CircleShape2D" :api :core :refcounted common-lisp:t))

(defgclass (class-db :bind "ClassDB" :api :core))


(defgenum (class-db+apitype :class 'class-db) (:core 0) (:editor 1)
 (:extension 2) (:editor-extension 3) (:none 4))

(defgclass (code-edit :bind "CodeEdit" :api :core)
 (:signals (breakpoint-toggled line int) (code-completion-requested)
  (symbol-lookup symbol string line int column int)
  (symbol-validate symbol string)
  (symbol-hovered symbol string line int column int)))


(defgenum (code-edit+code-completion-kind :class 'code-edit) (:class 0)
 (:function 1) (:signal 2) (:variable 3) (:member 4) (:enum 5) (:constant 6)
 (:node-path 7) (:file-path 8) (:plain-text 9) (:keyword 10))


(defgenum (code-edit+code-completion-location :class 'code-edit) (:local 0)
 (:parent-mask 256) (:other-user-code 512) (:other 1024))

(defgclass
 (code-highlighter :bind "CodeHighlighter" :api :core :refcounted
  common-lisp:t))

(defgclass
 (collision-object-2d :bind "CollisionObject2D" :api :core :instantiable
  common-lisp:nil)
 (:signals (input-event viewport node event input-event shape-idx int)
  (mouse-entered) (mouse-exited) (mouse-shape-entered shape-idx int)
  (mouse-shape-exited shape-idx int)))


(defgenum (collision-object-2d+disable-mode :class 'collision-object-2d)
 (:remove 0) (:make-static 1) (:keep-active 2))

(defgclass
 (collision-object-3d :bind "CollisionObject3D" :api :core :instantiable
  common-lisp:nil)
 (:signals
  (input-event camera node event input-event event-position vector-3 normal
   vector-3 shape-idx int)
  (mouse-entered) (mouse-exited)))


(defgenum (collision-object-3d+disable-mode :class 'collision-object-3d)
 (:remove 0) (:make-static 1) (:keep-active 2))

(defgclass (collision-polygon-2d :bind "CollisionPolygon2D" :api :core))


(defgenum (collision-polygon-2d+build-mode :class 'collision-polygon-2d)
 (:solids 0) (:segments 1))

(defgclass (collision-polygon-3d :bind "CollisionPolygon3D" :api :core))

(defgclass (collision-shape-2d :bind "CollisionShape2D" :api :core))

(defgclass (collision-shape-3d :bind "CollisionShape3D" :api :core))

(defgclass
 (color-palette :bind "ColorPalette" :api :core :refcounted common-lisp:t))

(defgclass (color-picker :bind "ColorPicker" :api :core)
 (:signals (color-changed color color) (preset-added color color)
  (preset-removed color color)))


(defgenum (color-picker+color-mode-type :class 'color-picker) (:rgb 0) (:hsv 1)
 (:raw 2) (:linear 2) (:okhsl 3))


(defgenum (color-picker+picker-shape-type :class 'color-picker)
 (:hsv-rectangle 0) (:hsv-wheel 1) (:vhs-circle 2) (:okhsl-circle 3) (:none 4)
 (:ok-hs-rectangle 5) (:ok-hl-rectangle 6))

(defgclass (color-picker-button :bind "ColorPickerButton" :api :core)
 (:signals (color-changed color color) (popup-closed) (picker-created)))

(defgclass (color-rect :bind "ColorRect" :api :core))

(defgclass (compositor :bind "Compositor" :api :core :refcounted common-lisp:t))

(defgclass
 (compositor-effect :bind "CompositorEffect" :api :core :refcounted
  common-lisp:t))


(defgenum (compositor-effect+effect-callback-type :class 'compositor-effect)
 (:pre-opaque 0) (:post-opaque 1) (:post-sky 2) (:pre-transparent 3)
 (:post-transparent 4) (:max 5))

(defgclass
 (compressed-cubemap :bind "CompressedCubemap" :api :core :refcounted
  common-lisp:t))

(defgclass
 (compressed-cubemap-array :bind "CompressedCubemapArray" :api :core
  :refcounted common-lisp:t))

(defgclass
 (compressed-texture-2d :bind "CompressedTexture2D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (compressed-texture-2darray :bind "CompressedTexture2DArray" :api :core
  :refcounted common-lisp:t))

(defgclass
 (compressed-texture-3d :bind "CompressedTexture3D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (compressed-texture-layered :bind "CompressedTextureLayered" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (concave-polygon-shape-2d :bind "ConcavePolygonShape2D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (concave-polygon-shape-3d :bind "ConcavePolygonShape3D" :api :core :refcounted
  common-lisp:t))

(defgclass (cone-twist-joint-3d :bind "ConeTwistJoint3D" :api :core))


(defgenum (cone-twist-joint-3d+param :class 'cone-twist-joint-3d)
 (:swing-span 0) (:twist-span 1) (:bias 2) (:softness 3) (:relaxation 4)
 (:max 5))

(defgclass
 (config-file :bind "ConfigFile" :api :core :refcounted common-lisp:t))

(defgclass (confirmation-dialog :bind "ConfirmationDialog" :api :core))

(defgclass (container :bind "Container" :api :core)
 (:signals (pre-sort-children) (sort-children)))


(defgconstant +container+notification-pre-sort-children+ :value 50 :bind
 "NOTIFICATION_PRE_SORT_CHILDREN" :class 'container)


(defgconstant +container+notification-sort-children+ :value 51 :bind
 "NOTIFICATION_SORT_CHILDREN" :class 'container)

(defgclass (control :bind "Control" :api :core)
 (:signals (resized) (gui-input event input-event) (mouse-entered)
  (mouse-exited) (focus-entered) (focus-exited) (size-flags-changed)
  (maximum-size-changed) (minimum-size-changed) (theme-changed)))


(defgconstant +control+notification-resized+ :value 40 :bind
 "NOTIFICATION_RESIZED" :class 'control)


(defgconstant +control+notification-mouse-enter+ :value 41 :bind
 "NOTIFICATION_MOUSE_ENTER" :class 'control)


(defgconstant +control+notification-mouse-exit+ :value 42 :bind
 "NOTIFICATION_MOUSE_EXIT" :class 'control)


(defgconstant +control+notification-mouse-enter-self+ :value 60 :bind
 "NOTIFICATION_MOUSE_ENTER_SELF" :class 'control)


(defgconstant +control+notification-mouse-exit-self+ :value 61 :bind
 "NOTIFICATION_MOUSE_EXIT_SELF" :class 'control)


(defgconstant +control+notification-focus-enter+ :value 43 :bind
 "NOTIFICATION_FOCUS_ENTER" :class 'control)


(defgconstant +control+notification-focus-exit+ :value 44 :bind
 "NOTIFICATION_FOCUS_EXIT" :class 'control)


(defgconstant +control+notification-theme-changed+ :value 45 :bind
 "NOTIFICATION_THEME_CHANGED" :class 'control)


(defgconstant +control+notification-scroll-begin+ :value 47 :bind
 "NOTIFICATION_SCROLL_BEGIN" :class 'control)


(defgconstant +control+notification-scroll-end+ :value 48 :bind
 "NOTIFICATION_SCROLL_END" :class 'control)


(defgconstant +control+notification-layout-direction-changed+ :value 49 :bind
 "NOTIFICATION_LAYOUT_DIRECTION_CHANGED" :class 'control)


(defgenum (control+focus-mode :class 'control) (:none 0) (:click 1) (:all 2)
 (:accessibility 3))


(defgenum (control+focus-behavior-recursive :class 'control) (:inherited 0)
 (:disabled 1) (:enabled 2))


(defgenum (control+mouse-behavior-recursive :class 'control) (:inherited 0)
 (:disabled 1) (:enabled 2))


(defgenum (control+cursor-shape :class 'control) (:arrow 0) (:ibeam 1)
 (:pointing-hand 2) (:cross 3) (:wait 4) (:busy 5) (:drag 6) (:can-drop 7)
 (:forbidden 8) (:vsize 9) (:hsize 10) (:bdiagsize 11) (:fdiagsize 12)
 (:move 13) (:vsplit 14) (:hsplit 15) (:help 16))


(defgenum (control+layout-preset :class 'control) (:top-left 0) (:top-right 1)
 (:bottom-left 2) (:bottom-right 3) (:center-left 4) (:center-top 5)
 (:center-right 6) (:center-bottom 7) (:center 8) (:left-wide 9) (:top-wide 10)
 (:right-wide 11) (:bottom-wide 12) (:vcenter-wide 13) (:hcenter-wide 14)
 (:full-rect 15))


(defgenum (control+layout-preset-mode :class 'control) (:minsize 0)
 (:keep-width 1) (:keep-height 2) (:keep-size 3))


(defgenum (control+size-flags :bitfield common-lisp:t :class 'control)
 (:shrink-begin 0) (:fill 1) (:expand 2) (:expand-fill 3) (:shrink-center 4)
 (:shrink-end 8))


(defgenum (control+mouse-filter :class 'control) (:stop 0) (:pass 1)
 (:ignore 2))


(defgenum (control+grow-direction :class 'control) (:begin 0) (:end 1)
 (:both 2))


(defgenum (control+anchor :class 'control) (:begin 0) (:end 1))


(defgenum (control+layout-direction :class 'control) (:inherited 0)
 (:application-locale 1) (:ltr 2) (:rtl 3) (:system-locale 4) (:max 5)
 (:locale 1))


(defgenum (control+text-direction :class 'control) (:inherited 3) (:auto 0)
 (:ltr 1) (:rtl 2))

(defgclass
 (convert-transform-modifier-3d :bind "ConvertTransformModifier3D" :api :core))


(defgenum
 (convert-transform-modifier-3d+transform-mode :class
  'convert-transform-modifier-3d)
 (:position 0) (:rotation 1) (:scale 2))

(defgclass
 (convex-polygon-shape-2d :bind "ConvexPolygonShape2D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (convex-polygon-shape-3d :bind "ConvexPolygonShape3D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (copy-transform-modifier-3d :bind "CopyTransformModifier3D" :api :core))


(defgenum
 (copy-transform-modifier-3d+transform-flag :bitfield common-lisp:t :class
  'copy-transform-modifier-3d)
 (:position 1) (:rotation 2) (:scale 4) (:all 7))


(defgenum
 (copy-transform-modifier-3d+axis-flag :bitfield common-lisp:t :class
  'copy-transform-modifier-3d)
 (:x 1) (:y 2) (:z 4) (:all 7))

(defgclass (crypto :bind "Crypto" :api :core :refcounted common-lisp:t))

(defgclass (crypto-key :bind "CryptoKey" :api :core :refcounted common-lisp:t))

(defgclass (cubemap :bind "Cubemap" :api :core :refcounted common-lisp:t))

(defgclass
 (cubemap-array :bind "CubemapArray" :api :core :refcounted common-lisp:t))

(defgclass (curve :bind "Curve" :api :core :refcounted common-lisp:t)
 (:signals (range-changed) (domain-changed)))


(defgenum (curve+tangent-mode :class 'curve) (:free 0) (:linear 1)
 (:mode-count 2))

(defgclass (curve-2d :bind "Curve2D" :api :core :refcounted common-lisp:t))

(defgclass (curve-3d :bind "Curve3D" :api :core :refcounted common-lisp:t))

(defgclass
 (curve-texture :bind "CurveTexture" :api :core :refcounted common-lisp:t))


(defgenum (curve-texture+texture-mode :class 'curve-texture) (:rgb 0) (:red 1))

(defgclass
 (curve-xyztexture :bind "CurveXYZTexture" :api :core :refcounted
  common-lisp:t))

(defgclass
 (cylinder-mesh :bind "CylinderMesh" :api :core :refcounted common-lisp:t))

(defgclass
 (cylinder-shape-3d :bind "CylinderShape3D" :api :core :refcounted
  common-lisp:t))

(defgclass (dpitexture :bind "DPITexture" :api :core :refcounted common-lisp:t))

(defgclass (dtlsserver :bind "DTLSServer" :api :core :refcounted common-lisp:t))

(defgclass (damped-spring-joint-2d :bind "DampedSpringJoint2D" :api :core))

(defgclass (decal :bind "Decal" :api :core))


(defgenum (decal+decal-texture :class 'decal) (:albedo 0) (:normal 1) (:orm 2)
 (:emission 3) (:max 4))

(defgclass
 (dir-access :bind "DirAccess" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))

(defgclass (directional-light-2d :bind "DirectionalLight2D" :api :core))

(defgclass (directional-light-3d :bind "DirectionalLight3D" :api :core))


(defgenum (directional-light-3d+shadow-mode :class 'directional-light-3d)
 (:orthogonal 0) (:parallel-2-splits 1) (:parallel-4-splits 2))


(defgenum (directional-light-3d+sky-mode :class 'directional-light-3d)
 (:light-and-sky 0) (:light-only 1) (:sky-only 2))

(defgclass
 (display-server :bind "DisplayServer" :api :core :instantiable
  common-lisp:nil)
 (:signals (orientation-changed orientation int)))


(defgconstant +display-server+invalid-screen+ :value -1 :bind "INVALID_SCREEN"
 :class 'display-server)


(defgconstant +display-server+screen-with-mouse-focus+ :value -4 :bind
 "SCREEN_WITH_MOUSE_FOCUS" :class 'display-server)


(defgconstant +display-server+screen-with-keyboard-focus+ :value -3 :bind
 "SCREEN_WITH_KEYBOARD_FOCUS" :class 'display-server)


(defgconstant +display-server+screen-primary+ :value -2 :bind "SCREEN_PRIMARY"
 :class 'display-server)


(defgconstant +display-server+screen-of-main-window+ :value -1 :bind
 "SCREEN_OF_MAIN_WINDOW" :class 'display-server)


(defgconstant +display-server+main-window-id+ :value 0 :bind "MAIN_WINDOW_ID"
 :class 'display-server)


(defgconstant +display-server+invalid-window-id+ :value -1 :bind
 "INVALID_WINDOW_ID" :class 'display-server)


(defgconstant +display-server+invalid-indicator-id+ :value -1 :bind
 "INVALID_INDICATOR_ID" :class 'display-server)


(defgenum (display-server+feature :class 'display-server) (:global-menu 0)
 (:subwindows 1) (:touchscreen 2) (:mouse 3) (:mouse-warp 4) (:clipboard 5)
 (:virtual-keyboard 6) (:cursor-shape 7) (:custom-cursor-shape 8)
 (:native-dialog 9) (:ime 10) (:window-transparency 11) (:hidpi 12) (:icon 13)
 (:native-icon 14) (:orientation 15) (:swap-buffers 16) (:clipboard-primary 18)
 (:text-to-speech 19) (:extend-to-title 20) (:screen-capture 21)
 (:status-indicator 22) (:native-help 23) (:native-dialog-input 24)
 (:native-dialog-file 25) (:native-dialog-file-extra 26) (:window-drag 27)
 (:screen-exclude-from-capture 28) (:window-embedding 29)
 (:native-dialog-file-mime 30) (:emoji-and-symbol-picker 31)
 (:native-color-picker 32) (:self-fitting-windows 33)
 (:accessibility-screen-reader 34) (:hdr-output 35) (:pip-mode 36))


(defgenum (display-server+accessibility-role :class 'display-server)
 (:unknown 0) (:default-button 1) (:audio 2) (:video 3) (:static-text 4)
 (:container 5) (:panel 6) (:button 7) (:link 8) (:check-box 9)
 (:radio-button 10) (:check-button 11) (:scroll-bar 12) (:scroll-view 13)
 (:splitter 14) (:slider 15) (:spin-button 16) (:progress-indicator 17)
 (:text-field 18) (:multiline-text-field 19) (:color-picker 20) (:table 21)
 (:cell 22) (:row 23) (:row-group 24) (:row-header 25) (:column-header 26)
 (:tree 27) (:tree-item 28) (:list 29) (:list-item 30) (:list-box 31)
 (:list-box-option 32) (:tab-bar 33) (:tab 34) (:tab-panel 35) (:menu-bar 36)
 (:menu 37) (:menu-item 38) (:menu-item-check-box 39) (:menu-item-radio 40)
 (:image 41) (:window 42) (:title-bar 43) (:dialog 44) (:tooltip 45)
 (:region 46) (:text-run 47))


(defgenum (display-server+accessibility-popup-type :class 'display-server)
 (:menu 0) (:list 1) (:tree 2) (:dialog 3))


(defgenum (display-server+accessibility-flags :class 'display-server)
 (:hidden 0) (:multiselectable 1) (:required 2) (:visited 3) (:busy 4)
 (:modal 5) (:touch-passthrough 6) (:readonly 7) (:disabled 8)
 (:clips-children 9))


(defgenum (display-server+accessibility-action :class 'display-server)
 (:click 0) (:focus 1) (:blur 2) (:collapse 3) (:expand 4) (:decrement 5)
 (:increment 6) (:hide-tooltip 7) (:show-tooltip 8) (:set-text-selection 9)
 (:replace-selected-text 10) (:scroll-backward 11) (:scroll-down 12)
 (:scroll-forward 13) (:scroll-left 14) (:scroll-right 15) (:scroll-up 16)
 (:scroll-into-view 17) (:scroll-to-point 18) (:set-scroll-offset 19)
 (:set-value 20) (:show-context-menu 21) (:custom 22))


(defgenum (display-server+accessibility-live-mode :class 'display-server)
 (:off 0) (:polite 1) (:assertive 2))


(defgenum (display-server+accessibility-scroll-unit :class 'display-server)
 (:item 0) (:page 1))


(defgenum (display-server+accessibility-scroll-hint :class 'display-server)
 (:top-left 0) (:bottom-right 1) (:top-edge 2) (:bottom-edge 3) (:left-edge 4)
 (:right-edge 5))


(defgenum (display-server+mouse-mode :class 'display-server) (:visible 0)
 (:hidden 1) (:captured 2) (:confined 3) (:confined-hidden 4) (:max 5))


(defgenum (display-server+screen-orientation :class 'display-server)
 (:landscape 0) (:portrait 1) (:reverse-landscape 2) (:reverse-portrait 3)
 (:sensor-landscape 4) (:sensor-portrait 5) (:sensor 6))


(defgenum (display-server+virtual-keyboard-type :class 'display-server)
 (:default 0) (:multiline 1) (:number 2) (:number-decimal 3) (:phone 4)
 (:email-address 5) (:password 6) (:url 7))


(defgenum (display-server+cursor-shape :class 'display-server) (:arrow 0)
 (:ibeam 1) (:pointing-hand 2) (:cross 3) (:wait 4) (:busy 5) (:drag 6)
 (:can-drop 7) (:forbidden 8) (:vsize 9) (:hsize 10) (:bdiagsize 11)
 (:fdiagsize 12) (:move 13) (:vsplit 14) (:hsplit 15) (:help 16) (:max 17))


(defgenum (display-server+file-dialog-mode :class 'display-server)
 (:open-file 0) (:open-files 1) (:open-dir 2) (:open-any 3) (:save-file 4))


(defgenum (display-server+window-mode :class 'display-server) (:windowed 0)
 (:minimized 1) (:maximized 2) (:fullscreen 3) (:exclusive-fullscreen 4))


(defgenum (display-server+progress-state :class 'display-server)
 (:noprogress 0) (:indeterminate 1) (:normal 2) (:error 3) (:paused 4))


(defgenum (display-server+window-flags :class 'display-server)
 (:resize-disabled 0) (:borderless 1) (:always-on-top 2) (:transparent 3)
 (:no-focus 4) (:popup 5) (:extend-to-title 6) (:mouse-passthrough 7)
 (:sharp-corners 8) (:exclude-from-capture 9) (:popup-wm-hint 10)
 (:minimize-disabled 11) (:maximize-disabled 12) (:max 13))


(defgenum (display-server+window-event :class 'display-server) (:mouse-enter 0)
 (:mouse-exit 1) (:focus-in 2) (:focus-out 3) (:close-request 4)
 (:go-back-request 5) (:dpi-change 6) (:titlebar-change 7) (:force-close 8)
 (:output-max-linear-value-changed 9))


(defgenum (display-server+window-resize-edge :class 'display-server)
 (:top-left 0) (:top 1) (:top-right 2) (:left 3) (:right 4) (:bottom-left 5)
 (:bottom 6) (:bottom-right 7) (:max 8))


(defgenum (display-server+vsync-mode :class 'display-server) (:disabled 0)
 (:enabled 1) (:adaptive 2) (:mailbox 3))


(defgenum (display-server+handle-type :class 'display-server)
 (:display-handle 0) (:window-handle 1) (:window-view 2) (:opengl-context 3)
 (:egl-display 4) (:egl-config 5) (:glx-visualid 6) (:glx-fbconfig 7))


(defgenum (display-server+ttsutterance-event :class 'display-server)
 (:started 0) (:ended 1) (:canceled 2) (:boundary 3))

(defgclass
 (drawable-texture-2d :bind "DrawableTexture2D" :api :core :refcounted
  common-lisp:t))


(defgenum (drawable-texture-2d+drawable-format :class 'drawable-texture-2d)
 (:rgba8 0) (:rgba8-srgb 1) (:rgbah 2) (:rgbaf 3))

(defgclass
 (enet-connection :bind "ENetConnection" :api :core :refcounted common-lisp:t))


(defgenum (enet-connection+compression-mode :class 'enet-connection) (:none 0)
 (:range-coder 1) (:fastlz 2) (:zlib 3) (:zstd 4))


(defgenum (enet-connection+event-type :class 'enet-connection) (:error -1)
 (:none 0) (:connect 1) (:disconnect 2) (:receive 3))


(defgenum (enet-connection+host-statistic :class 'enet-connection)
 (:sent-data 0) (:sent-packets 1) (:received-data 2) (:received-packets 3))

(defgclass
 (enet-multiplayer-peer :bind "ENetMultiplayerPeer" :api :core :refcounted
  common-lisp:t))

(defgclass
 (enet-packet-peer :bind "ENetPacketPeer" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))


(defgconstant +enet-packet-peer+packet-loss-scale+ :value 65536 :bind
 "PACKET_LOSS_SCALE" :class 'enet-packet-peer)


(defgconstant +enet-packet-peer+packet-throttle-scale+ :value 32 :bind
 "PACKET_THROTTLE_SCALE" :class 'enet-packet-peer)


(defgconstant +enet-packet-peer+flag-reliable+ :value 1 :bind "FLAG_RELIABLE"
 :class 'enet-packet-peer)


(defgconstant +enet-packet-peer+flag-unsequenced+ :value 2 :bind
 "FLAG_UNSEQUENCED" :class 'enet-packet-peer)


(defgconstant +enet-packet-peer+flag-unreliable-fragment+ :value 8 :bind
 "FLAG_UNRELIABLE_FRAGMENT" :class 'enet-packet-peer)


(defgenum (enet-packet-peer+peer-state :class 'enet-packet-peer)
 (:disconnected 0) (:connecting 1) (:acknowledging-connect 2)
 (:connection-pending 3) (:connection-succeeded 4) (:connected 5)
 (:disconnect-later 6) (:disconnecting 7) (:acknowledging-disconnect 8)
 (:zombie 9))


(defgenum (enet-packet-peer+peer-statistic :class 'enet-packet-peer)
 (:packet-loss 0) (:packet-loss-variance 1) (:packet-loss-epoch 2)
 (:round-trip-time 3) (:round-trip-time-variance 4) (:last-round-trip-time 5)
 (:last-round-trip-time-variance 6) (:packet-throttle 7)
 (:packet-throttle-limit 8) (:packet-throttle-counter 9)
 (:packet-throttle-epoch 10) (:packet-throttle-acceleration 11)
 (:packet-throttle-deceleration 12) (:packet-throttle-interval 13))

(defgclass (editor-command-palette :bind "EditorCommandPalette" :api :editor))

(defgclass
 (editor-context-menu-plugin :bind "EditorContextMenuPlugin" :api :editor
  :refcounted common-lisp:t))


(defgenum
 (editor-context-menu-plugin+context-menu-slot :class
  'editor-context-menu-plugin)
 (:scene-tree 0) (:filesystem 1) (:script-editor 2) (:filesystem-create 3)
 (:script-editor-code 4) (:scene-tabs 5) (:2d-editor 6) (:inspector-property 7))

(defgclass
 (editor-debugger-plugin :bind "EditorDebuggerPlugin" :api :editor :refcounted
  common-lisp:t))

(defgclass
 (editor-debugger-session :bind "EditorDebuggerSession" :api :editor
  :instantiable common-lisp:nil :refcounted common-lisp:t)
 (:signals (started) (stopped) (breaked can-debug bool) (continued)))

(defgclass (editor-dock :bind "EditorDock" :api :editor)
 (:signals (opened) (closed)))


(defgenum (editor-dock+dock-layout :bitfield common-lisp:t :class 'editor-dock)
 (:vertical 1) (:horizontal 2) (:floating 4) (:all 7))


(defgenum (editor-dock+dock-slot :class 'editor-dock) (:none -1) (:left-ul 0)
 (:left-bl 1) (:left-ur 2) (:left-br 3) (:right-ul 4) (:right-bl 5)
 (:right-ur 6) (:right-br 7) (:bottom 8) (:bottom-l 9) (:bottom-r 10) (:max 11))

(defgclass
 (editor-export-platform :bind "EditorExportPlatform" :api :editor
  :instantiable common-lisp:nil :refcounted common-lisp:t))


(defgenum
 (editor-export-platform+export-message-type :class 'editor-export-platform)
 (:none 0) (:info 1) (:warning 2) (:error 3))


(defgenum
 (editor-export-platform+debug-flags :bitfield common-lisp:t :class
  'editor-export-platform)
 (:dumb-client 1) (:remote-debug 2) (:remote-debug-localhost 4)
 (:view-collisions 8) (:view-navigation 16))

(defgclass
 (editor-export-platform-android :bind "EditorExportPlatformAndroid" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (editor-export-platform-apple-embedded :bind
  "EditorExportPlatformAppleEmbedded" :api :editor :instantiable
  common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (editor-export-platform-extension :bind "EditorExportPlatformExtension" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (editor-export-platform-ios :bind "EditorExportPlatformIOS" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (editor-export-platform-linux-bsd :bind "EditorExportPlatformLinuxBSD" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (editor-export-platform-mac-os :bind "EditorExportPlatformMacOS" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (editor-export-platform-pc :bind "EditorExportPlatformPC" :api :editor
  :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (editor-export-platform-vision-os :bind "EditorExportPlatformVisionOS" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (editor-export-platform-web :bind "EditorExportPlatformWeb" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (editor-export-platform-windows :bind "EditorExportPlatformWindows" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (editor-export-plugin :bind "EditorExportPlugin" :api :editor :refcounted
  common-lisp:t))

(defgclass
 (editor-export-preset :bind "EditorExportPreset" :api :editor :instantiable
  common-lisp:nil :refcounted common-lisp:t))


(defgenum (editor-export-preset+export-filter :class 'editor-export-preset)
 (:export-all-resources 0) (:export-selected-scenes 1)
 (:export-selected-resources 2) (:exclude-selected-resources 3)
 (:export-customized 4))


(defgenum (editor-export-preset+file-export-mode :class 'editor-export-preset)
 (:not-customized 0) (:strip 1) (:keep 2) (:remove 3))


(defgenum
 (editor-export-preset+script-export-mode :class 'editor-export-preset)
 (:text 0) (:binary-tokens 1) (:binary-tokens-compressed 2))

(defgclass
 (editor-feature-profile :bind "EditorFeatureProfile" :api :editor :refcounted
  common-lisp:t))


(defgenum (editor-feature-profile+feature :class 'editor-feature-profile)
 (:|3D| 0) (:script 1) (:asset-lib 2) (:scene-tree 3) (:node-dock 4)
 (:filesystem-dock 5) (:import-dock 6) (:history-dock 7) (:game 8)
 (:signals-dock 9) (:groups-dock 10) (:max 11))

(defgclass (editor-file-dialog :bind "EditorFileDialog" :api :editor))

(defgclass
 (editor-file-system :bind "EditorFileSystem" :api :editor :instantiable
  common-lisp:nil)
 (:signals (filesystem-changed) (script-classes-updated)
  (sources-changed exist bool)
  (resources-reimporting resources packed-string-array)
  (resources-reimported resources packed-string-array)
  (resources-reload resources packed-string-array)))

(defgclass
 (editor-file-system-directory :bind "EditorFileSystemDirectory" :api :editor))

(defgclass
 (editor-file-system-import-format-support-query :bind
  "EditorFileSystemImportFormatSupportQuery" :api :editor :refcounted
  common-lisp:t))

(defgclass
 (editor-import-plugin :bind "EditorImportPlugin" :api :editor :refcounted
  common-lisp:t))

(defgclass (editor-inspector :bind "EditorInspector" :api :editor)
 (:signals (property-selected property string)
  (property-keyed property string value variant advance bool)
  (property-deleted property string)
  (resource-selected resource resource path string) (object-id-selected id int)
  (property-edited property string)
  (property-toggled property string checked bool) (edited-object-changed)
  (restart-requested)))

(defgclass
 (editor-inspector-plugin :bind "EditorInspectorPlugin" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (editor-interface :bind "EditorInterface" :api :editor :instantiable
  common-lisp:nil))

(defgclass
 (editor-node-3dgizmo :bind "EditorNode3DGizmo" :api :editor :refcounted
  common-lisp:t))

(defgclass
 (editor-node-3dgizmo-plugin :bind "EditorNode3DGizmoPlugin" :api :editor
  :refcounted common-lisp:t))

(defgclass (editor-paths :bind "EditorPaths" :api :editor))

(defgclass (editor-plugin :bind "EditorPlugin" :api :editor)
 (:signals (scene-changed scene-root node) (scene-closed filepath string)
  (main-screen-changed screen-name string) (resource-saved resource resource)
  (scene-saved filepath string) (project-settings-changed)))


(defgenum (editor-plugin+custom-control-container :class 'editor-plugin)
 (:toolbar 0) (:spatial-editor-menu 1) (:spatial-editor-side-left 2)
 (:spatial-editor-side-right 3) (:spatial-editor-bottom 4)
 (:canvas-editor-menu 5) (:canvas-editor-side-left 6)
 (:canvas-editor-side-right 7) (:canvas-editor-bottom 8) (:inspector-bottom 9)
 (:project-setting-tab-left 10) (:project-setting-tab-right 11))


(defgenum (editor-plugin+dock-slot :class 'editor-plugin) (:none -1)
 (:left-ul 0) (:left-bl 1) (:left-ur 2) (:left-br 3) (:right-ul 4)
 (:right-bl 5) (:right-ur 6) (:right-br 7) (:bottom 8) (:max 9))


(defgenum (editor-plugin+after-guiinput :class 'editor-plugin) (:pass 0)
 (:stop 1) (:custom 2))

(defgclass (editor-property :bind "EditorProperty" :api :editor)
 (:signals
  (property-changed property string-name value variant field string-name
   changing bool)
  (multiple-properties-changed properties packed-string-array value array)
  (property-keyed property string-name) (property-deleted property string-name)
  (property-keyed-with-value property string-name value variant)
  (property-checked property string-name checked bool) (property-overridden)
  (property-favorited property string-name favorited bool)
  (property-pinned property string-name pinned bool)
  (property-can-revert-changed property string-name can-revert bool)
  (resource-selected path string resource resource)
  (object-id-selected property string-name id int)
  (selected path string focusable-idx int)))

(defgclass
 (editor-resource-conversion-plugin :bind "EditorResourceConversionPlugin" :api
  :editor :refcounted common-lisp:t))

(defgclass (editor-resource-picker :bind "EditorResourcePicker" :api :editor)
 (:signals (resource-selected resource resource inspect bool)
  (resource-changed resource resource)))

(defgclass
 (editor-resource-preview :bind "EditorResourcePreview" :api :editor
  :instantiable common-lisp:nil)
 (:signals (preview-invalidated path string)))

(defgclass
 (editor-resource-preview-generator :bind "EditorResourcePreviewGenerator" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (editor-resource-tooltip-plugin :bind "EditorResourceTooltipPlugin" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (editor-scene-format-importer :bind "EditorSceneFormatImporter" :api :editor
  :refcounted common-lisp:t))


(defgenum
 (editor-scene-format-importer+import-flags :bitfield common-lisp:t :class
  'editor-scene-format-importer)
 (:scene 1) (:animation 2) (:fail-on-missing-dependencies 4)
 (:generate-tangent-arrays 8) (:use-named-skin-binds 16)
 (:discard-meshes-and-materials 32) (:force-disable-mesh-compression 64))

(defgclass
 (editor-scene-format-importer-blend :bind "EditorSceneFormatImporterBlend"
  :api :editor :refcounted common-lisp:t))

(defgclass
 (editor-scene-format-importer-fbx2gltf :bind
  "EditorSceneFormatImporterFBX2GLTF" :api :editor :refcounted common-lisp:t))

(defgclass
 (editor-scene-format-importer-gltf :bind "EditorSceneFormatImporterGLTF" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (editor-scene-format-importer-ufbx :bind "EditorSceneFormatImporterUFBX" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (editor-scene-post-import :bind "EditorScenePostImport" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (editor-scene-post-import-plugin :bind "EditorScenePostImportPlugin" :api
  :editor :refcounted common-lisp:t))


(defgenum
 (editor-scene-post-import-plugin+internal-import-category :class
  'editor-scene-post-import-plugin)
 (:node 0) (:mesh-3d-node 1) (:mesh 2) (:material 3) (:animation 4)
 (:animation-node 5) (:skeleton-3d-node 6) (:max 7))

(defgclass
 (editor-script :bind "EditorScript" :api :editor :refcounted common-lisp:t))

(defgclass (editor-script-picker :bind "EditorScriptPicker" :api :editor))

(defgclass (editor-selection :bind "EditorSelection" :api :editor)
 (:signals (selection-changed)))

(defgclass
 (editor-settings :bind "EditorSettings" :api :editor :refcounted
  common-lisp:t)
 (:signals (settings-changed)))


(defgconstant +editor-settings+notification-editor-settings-changed+ :value
 10000 :bind "NOTIFICATION_EDITOR_SETTINGS_CHANGED" :class 'editor-settings)

(defgclass (editor-spin-slider :bind "EditorSpinSlider" :api :editor)
 (:signals (grabbed) (ungrabbed) (updown-pressed) (value-focus-entered)
  (value-focus-exited)))


(defgenum (editor-spin-slider+control-state :class 'editor-spin-slider)
 (:default 0) (:prefer-slider 1) (:hide 2))

(defgclass
 (editor-syntax-highlighter :bind "EditorSyntaxHighlighter" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (editor-toaster :bind "EditorToaster" :api :editor :instantiable
  common-lisp:nil))


(defgenum (editor-toaster+severity :class 'editor-toaster) (:info 0)
 (:warning 1) (:error 2))

(defgclass
 (editor-translation-parser-plugin :bind "EditorTranslationParserPlugin" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (editor-undo-redo-manager :bind "EditorUndoRedoManager" :api :editor
  :instantiable common-lisp:nil)
 (:signals (history-changed) (version-changed)))


(defgenum
 (editor-undo-redo-manager+special-history :class 'editor-undo-redo-manager)
 (:global-history 0) (:remote-history -9) (:invalid-history -99))

(defgclass (editor-vcsinterface :bind "EditorVCSInterface" :api :editor))


(defgenum (editor-vcsinterface+change-type :class 'editor-vcsinterface)
 (:new 0) (:modified 1) (:renamed 2) (:deleted 3) (:typechange 4) (:unmerged 5))


(defgenum (editor-vcsinterface+tree-area :class 'editor-vcsinterface)
 (:commit 0) (:staged 1) (:unstaged 2))

(defgclass
 (encoded-object-as-id :bind "EncodedObjectAsID" :api :core :refcounted
  common-lisp:t))

(defgclass (engine :bind "Engine" :api :core))

(defgclass (engine-debugger :bind "EngineDebugger" :api :core))

(defgclass
 (engine-profiler :bind "EngineProfiler" :api :core :refcounted common-lisp:t))

(defgclass
 (environment :bind "Environment" :api :core :refcounted common-lisp:t))


(defgenum (environment+bgmode :class 'environment) (:clear-color 0) (:color 1)
 (:sky 2) (:canvas 3) (:keep 4) (:camera-feed 5) (:max 6))


(defgenum (environment+ambient-source :class 'environment) (:bg 0)
 (:disabled 1) (:color 2) (:sky 3))


(defgenum (environment+reflection-source :class 'environment) (:bg 0)
 (:disabled 1) (:sky 2))


(defgenum (environment+tone-mapper :class 'environment) (:linear 0)
 (:reinhardt 1) (:filmic 2) (:aces 3) (:agx 4))


(defgenum (environment+glow-blend-mode :class 'environment) (:additive 0)
 (:screen 1) (:softlight 2) (:replace 3) (:mix 4))


(defgenum (environment+fog-mode :class 'environment) (:exponential 0)
 (:depth 1))


(defgenum (environment+sdfgiyscale :class 'environment) (:50-percent 0)
 (:75-percent 1) (:100-percent 2))

(defgclass (expression :bind "Expression" :api :core :refcounted common-lisp:t))

(defgclass
 (external-texture :bind "ExternalTexture" :api :core :refcounted
  common-lisp:t))

(defgclass (fabrik3d :bind "FABRIK3D" :api :core))

(defgclass
 (fbxdocument :bind "FBXDocument" :api :core :refcounted common-lisp:t))

(defgclass (fbxstate :bind "FBXState" :api :core :refcounted common-lisp:t))

(defgclass
 (fast-noise-lite :bind "FastNoiseLite" :api :core :refcounted common-lisp:t))


(defgenum (fast-noise-lite+noise-type :class 'fast-noise-lite) (:value 5)
 (:value-cubic 4) (:perlin 3) (:cellular 2) (:simplex 0) (:simplex-smooth 1))


(defgenum (fast-noise-lite+fractal-type :class 'fast-noise-lite) (:none 0)
 (:fbm 1) (:ridged 2) (:ping-pong 3))


(defgenum (fast-noise-lite+cellular-distance-function :class 'fast-noise-lite)
 (:euclidean 0) (:euclidean-squared 1) (:manhattan 2) (:hybrid 3))


(defgenum (fast-noise-lite+cellular-return-type :class 'fast-noise-lite)
 (:cell-value 0) (:distance 1) (:distance2 2) (:distance2-add 3)
 (:distance2-sub 4) (:distance2-mul 5) (:distance2-div 6))


(defgenum (fast-noise-lite+domain-warp-type :class 'fast-noise-lite)
 (:simplex 0) (:simplex-reduced 1) (:basic-grid 2))


(defgenum (fast-noise-lite+domain-warp-fractal-type :class 'fast-noise-lite)
 (:none 0) (:progressive 1) (:independent 2))

(defgclass
 (file-access :bind "FileAccess" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))


(defgenum (file-access+mode-flags :class 'file-access) (:read 1) (:write 2)
 (:read-write 3) (:write-read 7))


(defgenum (file-access+compression-mode :class 'file-access) (:fastlz 0)
 (:deflate 1) (:zstd 2) (:gzip 3) (:brotli 4))


(defgenum
 (file-access+unix-permission-flags :bitfield common-lisp:t :class
  'file-access)
 (:read-owner 256) (:write-owner 128) (:execute-owner 64) (:read-group 32)
 (:write-group 16) (:execute-group 8) (:read-other 4) (:write-other 2)
 (:execute-other 1) (:set-user-id 2048) (:set-group-id 1024)
 (:restricted-delete 512))

(defgclass (file-dialog :bind "FileDialog" :api :core)
 (:signals (file-selected path string)
  (files-selected paths packed-string-array) (dir-selected dir string)
  (filename-filter-changed filter string)))


(defgenum (file-dialog+file-mode :class 'file-dialog) (:open-file 0)
 (:open-files 1) (:open-dir 2) (:open-any 3) (:save-file 4))


(defgenum (file-dialog+access :class 'file-dialog) (:resources 0) (:userdata 1)
 (:filesystem 2))


(defgenum (file-dialog+display-mode :class 'file-dialog) (:thumbnails 0)
 (:list 1))


(defgenum (file-dialog+customization :class 'file-dialog) (:hidden-files 0)
 (:create-folder 1) (:file-filter 2) (:file-sort 3) (:favorites 4) (:recent 5)
 (:layout 6) (:overwrite-warning 7) (:delete 8))

(defgclass
 (file-system-dock :bind "FileSystemDock" :api :editor :instantiable
  common-lisp:nil)
 (:signals (inherit file string) (instantiate files packed-string-array)
  (resource-removed resource resource) (file-removed file string)
  (folder-removed folder string) (files-moved old-file string new-file string)
  (folder-moved old-folder string new-folder string) (folder-color-changed)
  (selection-changed) (display-mode-changed)))

(defgclass (flow-container :bind "FlowContainer" :api :core))


(defgenum (flow-container+alignment-mode :class 'flow-container) (:begin 0)
 (:center 1) (:end 2))


(defgenum (flow-container+last-wrap-alignment-mode :class 'flow-container)
 (:inherit 0) (:begin 1) (:center 2) (:end 3))

(defgclass
 (fog-material :bind "FogMaterial" :api :core :refcounted common-lisp:t))

(defgclass (fog-volume :bind "FogVolume" :api :core))

(defgclass (foldable-container :bind "FoldableContainer" :api :core)
 (:signals (folding-changed is-folded bool)))


(defgenum (foldable-container+title-position :class 'foldable-container)
 (:top 0) (:bottom 1))

(defgclass
 (foldable-group :bind "FoldableGroup" :api :core :refcounted common-lisp:t)
 (:signals (expanded container foldable-container)))

(defgclass
 (font :bind "Font" :api :core :instantiable common-lisp:nil :refcounted
  common-lisp:t))

(defgclass (font-file :bind "FontFile" :api :core :refcounted common-lisp:t))

(defgclass
 (font-variation :bind "FontVariation" :api :core :refcounted common-lisp:t))

(defgclass (framebuffer-cache-rd :bind "FramebufferCacheRD" :api :core))

(defgclass
 (gdextension :bind "GDExtension" :api :core :refcounted common-lisp:t))


(defgenum (gdextension+initialization-level :class 'gdextension) (:core 0)
 (:servers 1) (:scene 2) (:editor 3))

(defgclass
 (gdextension-manager :bind "GDExtensionManager" :api :core :instantiable
  common-lisp:nil)
 (:signals (extensions-reloaded) (extension-loaded extension ||)
  (extension-unloading extension ||)))


(defgenum (gdextension-manager+load-status :class 'gdextension-manager) (:ok 0)
 (:failed 1) (:already-loaded 2) (:not-loaded 3) (:needs-restart 4))

(defgclass (gdscript :bind "GDScript" :api :core :refcounted common-lisp:t))

(defgclass
 (gdscript-language-protocol :bind "GDScriptLanguageProtocol" :api :editor))

(defgclass
 (gdscript-syntax-highlighter :bind "GDScriptSyntaxHighlighter" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (gdscript-text-document :bind "GDScriptTextDocument" :api :editor :refcounted
  common-lisp:t))

(defgclass
 (gdscript-workspace :bind "GDScriptWorkspace" :api :editor :refcounted
  common-lisp:t))

(defgclass
 (gltfaccessor :bind "GLTFAccessor" :api :core :refcounted common-lisp:t))


(defgenum (gltfaccessor+gltfaccessor-type :class 'gltfaccessor) (:scalar 0)
 (:vec2 1) (:vec3 2) (:vec4 3) (:mat2 4) (:mat3 5) (:mat4 6))


(defgenum (gltfaccessor+gltfcomponent-type :class 'gltfaccessor) (:none 0)
 (:signed-byte 5120) (:unsigned-byte 5121) (:signed-short 5122)
 (:unsigned-short 5123) (:signed-int 5124) (:unsigned-int 5125)
 (:single-float 5126) (:double-float 5130) (:half-float 5131)
 (:signed-long 5134) (:unsigned-long 5135))

(defgclass
 (gltfanimation :bind "GLTFAnimation" :api :core :refcounted common-lisp:t))

(defgclass
 (gltfbuffer-view :bind "GLTFBufferView" :api :core :refcounted common-lisp:t))

(defgclass (gltfcamera :bind "GLTFCamera" :api :core :refcounted common-lisp:t))

(defgclass
 (gltfdocument :bind "GLTFDocument" :api :core :refcounted common-lisp:t))


(defgenum (gltfdocument+root-node-mode :class 'gltfdocument) (:single-root 0)
 (:keep-root 1) (:multi-root 2))


(defgenum (gltfdocument+texture-map-mode :class 'gltfdocument)
 (:do-not-remap 0) (:remap-to-standard-material 1))


(defgenum (gltfdocument+visibility-mode :class 'gltfdocument)
 (:include-required 0) (:include-optional 1) (:exclude 2))


(defgenum
 (gltfdocument+import-flags :bitfield common-lisp:t :class 'gltfdocument)
 (:generate-tangent-arrays 8) (:use-named-skin-binds 16)
 (:discard-meshes-and-materials 32) (:force-disable-mesh-compression 64))

(defgclass
 (gltfdocument-extension :bind "GLTFDocumentExtension" :api :core :refcounted
  common-lisp:t))

(defgclass
 (gltfdocument-extension-convert-importer-mesh :bind
  "GLTFDocumentExtensionConvertImporterMesh" :api :core :refcounted
  common-lisp:t))

(defgclass (gltflight :bind "GLTFLight" :api :core :refcounted common-lisp:t))

(defgclass (gltfmesh :bind "GLTFMesh" :api :core :refcounted common-lisp:t))

(defgclass (gltfnode :bind "GLTFNode" :api :core :refcounted common-lisp:t))

(defgclass
 (gltfobject-model-property :bind "GLTFObjectModelProperty" :api :core
  :refcounted common-lisp:t))


(defgenum
 (gltfobject-model-property+gltfobject-model-type :class
  'gltfobject-model-property)
 (:unknown 0) (:bool 1) (:float 2) (:float-array 3) (:float2 4) (:float3 5)
 (:float4 6) (:float2x2 7) (:float3x3 8) (:float4x4 9) (:int 10))

(defgclass
 (gltfphysics-body :bind "GLTFPhysicsBody" :api :core :refcounted
  common-lisp:t))

(defgclass
 (gltfphysics-shape :bind "GLTFPhysicsShape" :api :core :refcounted
  common-lisp:t))

(defgclass
 (gltfskeleton :bind "GLTFSkeleton" :api :core :refcounted common-lisp:t))

(defgclass (gltfskin :bind "GLTFSkin" :api :core :refcounted common-lisp:t))

(defgclass
 (gltfspec-gloss :bind "GLTFSpecGloss" :api :core :refcounted common-lisp:t))

(defgclass (gltfstate :bind "GLTFState" :api :core :refcounted common-lisp:t))


(defgconstant +gltfstate+handle-binary-discard-textures+ :value 0 :bind
 "HANDLE_BINARY_DISCARD_TEXTURES" :class 'gltfstate)


(defgconstant +gltfstate+handle-binary-extract-textures+ :value 1 :bind
 "HANDLE_BINARY_EXTRACT_TEXTURES" :class 'gltfstate)


(defgconstant +gltfstate+handle-binary-embed-as-basisu+ :value 2 :bind
 "HANDLE_BINARY_EMBED_AS_BASISU" :class 'gltfstate)


(defgconstant +gltfstate+handle-binary-embed-as-uncompressed+ :value 3 :bind
 "HANDLE_BINARY_EMBED_AS_UNCOMPRESSED" :class 'gltfstate)


(defgenum (gltfstate+handle-binary-image-mode :class 'gltfstate)
 (:discard-textures 0) (:extract-textures 1) (:embed-as-basisu 2)
 (:embed-as-uncompressed 3))

(defgclass
 (gltftexture :bind "GLTFTexture" :api :core :refcounted common-lisp:t))

(defgclass
 (gltftexture-sampler :bind "GLTFTextureSampler" :api :core :refcounted
  common-lisp:t))

(defgclass (gpuparticles-2d :bind "GPUParticles2D" :api :core)
 (:signals (finished)))


(defgenum (gpuparticles-2d+draw-order :class 'gpuparticles-2d) (:index 0)
 (:lifetime 1) (:reverse-lifetime 2))


(defgenum (gpuparticles-2d+emit-flags :class 'gpuparticles-2d) (:position 1)
 (:rotation-scale 2) (:velocity 4) (:color 8) (:custom 16))

(defgclass (gpuparticles-3d :bind "GPUParticles3D" :api :core)
 (:signals (finished)))


(defgconstant +gpuparticles-3d+max-draw-passes+ :value 4 :bind
 "MAX_DRAW_PASSES" :class 'gpuparticles-3d)


(defgenum (gpuparticles-3d+draw-order :class 'gpuparticles-3d) (:index 0)
 (:lifetime 1) (:reverse-lifetime 2) (:view-depth 3))


(defgenum (gpuparticles-3d+emit-flags :class 'gpuparticles-3d) (:position 1)
 (:rotation-scale 2) (:velocity 4) (:color 8) (:custom 16))


(defgenum (gpuparticles-3d+transform-align :class 'gpuparticles-3d)
 (:disabled 0) (:z-billboard 1) (:y-to-velocity 2)
 (:z-billboard-y-to-velocity 3) (:local-billboard 4))

(defgclass
 (gpuparticles-attractor-3d :bind "GPUParticlesAttractor3D" :api :core
  :instantiable common-lisp:nil))

(defgclass
 (gpuparticles-attractor-box-3d :bind "GPUParticlesAttractorBox3D" :api :core))

(defgclass
 (gpuparticles-attractor-sphere-3d :bind "GPUParticlesAttractorSphere3D" :api
  :core))

(defgclass
 (gpuparticles-attractor-vector-field-3d :bind
  "GPUParticlesAttractorVectorField3D" :api :core))

(defgclass
 (gpuparticles-collision-3d :bind "GPUParticlesCollision3D" :api :core
  :instantiable common-lisp:nil))

(defgclass
 (gpuparticles-collision-box-3d :bind "GPUParticlesCollisionBox3D" :api :core))

(defgclass
 (gpuparticles-collision-height-field-3d :bind
  "GPUParticlesCollisionHeightField3D" :api :core))


(defgenum
 (gpuparticles-collision-height-field-3d+resolution :class
  'gpuparticles-collision-height-field-3d)
 (:|256| 0) (:|512| 1) (:|1024| 2) (:|2048| 3) (:|4096| 4) (:|8192| 5) (:max 6))


(defgenum
 (gpuparticles-collision-height-field-3d+update-mode :class
  'gpuparticles-collision-height-field-3d)
 (:when-moved 0) (:always 1))

(defgclass
 (gpuparticles-collision-sdf3d :bind "GPUParticlesCollisionSDF3D" :api :core))


(defgenum
 (gpuparticles-collision-sdf3d+resolution :class 'gpuparticles-collision-sdf3d)
 (:|16| 0) (:|32| 1) (:|64| 2) (:|128| 3) (:|256| 4) (:|512| 5) (:max 6))

(defgclass
 (gpuparticles-collision-sphere-3d :bind "GPUParticlesCollisionSphere3D" :api
  :core))

(defgclass (generic-6dofjoint-3d :bind "Generic6DOFJoint3D" :api :core))


(defgenum (generic-6dofjoint-3d+param :class 'generic-6dofjoint-3d)
 (:linear-lower-limit 0) (:linear-upper-limit 1) (:linear-limit-softness 2)
 (:linear-restitution 3) (:linear-damping 4) (:linear-motor-target-velocity 5)
 (:linear-motor-force-limit 6) (:linear-spring-stiffness 7)
 (:linear-spring-damping 8) (:linear-spring-equilibrium-point 9)
 (:angular-lower-limit 10) (:angular-upper-limit 11)
 (:angular-limit-softness 12) (:angular-damping 13) (:angular-restitution 14)
 (:angular-force-limit 15) (:angular-erp 16)
 (:angular-motor-target-velocity 17) (:angular-motor-force-limit 18)
 (:angular-spring-stiffness 19) (:angular-spring-damping 20)
 (:angular-spring-equilibrium-point 21) (:max 22))


(defgenum (generic-6dofjoint-3d+flag :class 'generic-6dofjoint-3d)
 (:enable-linear-limit 0) (:enable-angular-limit 1) (:enable-linear-spring 3)
 (:enable-angular-spring 2) (:enable-motor 4) (:enable-linear-motor 5) (:max 6))

(defgclass (geometry-2d :bind "Geometry2D" :api :core))


(defgenum (geometry-2d+poly-boolean-operation :class 'geometry-2d) (:union 0)
 (:difference 1) (:intersection 2) (:xor 3))


(defgenum (geometry-2d+poly-join-type :class 'geometry-2d) (:square 0)
 (:round 1) (:miter 2))


(defgenum (geometry-2d+poly-end-type :class 'geometry-2d) (:polygon 0)
 (:joined 1) (:butt 2) (:square 3) (:round 4))

(defgclass (geometry-3d :bind "Geometry3D" :api :core))

(defgclass (geometry-instance-3d :bind "GeometryInstance3D" :api :core))


(defgenum
 (geometry-instance-3d+shadow-casting-setting :class 'geometry-instance-3d)
 (:off 0) (:on 1) (:double-sided 2) (:shadows-only 3))


(defgenum (geometry-instance-3d+gimode :class 'geometry-instance-3d)
 (:disabled 0) (:static 1) (:dynamic 2))


(defgenum (geometry-instance-3d+lightmap-scale :class 'geometry-instance-3d)
 (:|1X| 0) (:|2X| 1) (:|4X| 2) (:|8X| 3) (:max 4))


(defgenum
 (geometry-instance-3d+visibility-range-fade-mode :class 'geometry-instance-3d)
 (:disabled 0) (:self 1) (:dependencies 2))

(defgclass
 (godot-instance :bind "GodotInstance" :api :core :instantiable
  common-lisp:nil))

(defgclass (gradient :bind "Gradient" :api :core :refcounted common-lisp:t))


(defgenum (gradient+interpolation-mode :class 'gradient) (:linear 0)
 (:constant 1) (:cubic 2))


(defgenum (gradient+color-space :class 'gradient) (:srgb 0) (:linear-srgb 1)
 (:oklab 2))

(defgclass
 (gradient-texture-1d :bind "GradientTexture1D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (gradient-texture-2d :bind "GradientTexture2D" :api :core :refcounted
  common-lisp:t))


(defgenum (gradient-texture-2d+fill :class 'gradient-texture-2d) (:linear 0)
 (:radial 1) (:square 2) (:conic 3))


(defgenum (gradient-texture-2d+repeat :class 'gradient-texture-2d)
 (:repeat-none 0) (:repeat 1) (:repeat-mirror 2))

(defgclass (graph-edit :bind "GraphEdit" :api :core)
 (:signals
  (connection-request from-node string-name from-port int to-node string-name
   to-port int)
  (disconnection-request from-node string-name from-port int to-node
   string-name to-port int)
  (connection-to-empty from-node string-name from-port int release-position
   vector-2)
  (connection-from-empty to-node string-name to-port int release-position
   vector-2)
  (connection-drag-started from-node string-name from-port int is-output bool)
  (connection-drag-ended) (copy-nodes-request) (cut-nodes-request)
  (paste-nodes-request) (duplicate-nodes-request)
  (delete-nodes-request nodes array) (node-selected node node)
  (node-deselected node node)
  (frame-rect-changed frame graph-frame new-rect rect-2)
  (popup-request at-position vector-2) (begin-node-move) (end-node-move)
  (graph-elements-linked-to-frame-request elements array frame string-name)
  (scroll-offset-changed offset vector-2)))


(defgenum (graph-edit+panning-scheme :class 'graph-edit) (:zooms 0) (:pans 1))


(defgenum (graph-edit+grid-pattern :class 'graph-edit) (:lines 0) (:dots 1))

(defgclass (graph-element :bind "GraphElement" :api :core)
 (:signals (node-selected) (node-deselected) (raise-request) (delete-request)
  (resize-request new-size vector-2) (resize-end new-size vector-2)
  (dragged from vector-2 to vector-2) (position-offset-changed)))

(defgclass (graph-frame :bind "GraphFrame" :api :core)
 (:signals (autoshrink-changed)))

(defgclass (graph-node :bind "GraphNode" :api :core)
 (:signals (slot-updated slot-index int) (slot-sizes-changed)))

(defgclass (grid-container :bind "GridContainer" :api :core))

(defgclass (grid-map :bind "GridMap" :api :core)
 (:signals (cell-size-changed cell-size vector-3) (changed)))


(defgconstant +grid-map+invalid-cell-item+ :value -1 :bind "INVALID_CELL_ITEM"
 :class 'grid-map)


(defgenum (grid-map+debug-visibility-mode :class 'grid-map) (:default 0)
 (:force-show 1) (:force-hide 2))

(defgclass (grid-map-editor-plugin :bind "GridMapEditorPlugin" :api :editor))

(defgclass (groove-joint-2d :bind "GrooveJoint2D" :api :core))

(defgclass (hbox-container :bind "HBoxContainer" :api :core))

(defgclass (hflow-container :bind "HFlowContainer" :api :core))

(defgclass
 (hmaccontext :bind "HMACContext" :api :core :refcounted common-lisp:t))

(defgclass (hscroll-bar :bind "HScrollBar" :api :core))

(defgclass (hseparator :bind "HSeparator" :api :core))

(defgclass (hslider :bind "HSlider" :api :core))

(defgclass (hsplit-container :bind "HSplitContainer" :api :core))

(defgclass (httpclient :bind "HTTPClient" :api :core :refcounted common-lisp:t))


(defgenum (httpclient+method :class 'httpclient) (:get 0) (:head 1) (:post 2)
 (:put 3) (:delete 4) (:options 5) (:trace 6) (:connect 7) (:patch 8) (:max 9))


(defgenum (httpclient+status :class 'httpclient) (:disconnected 0)
 (:resolving 1) (:cant-resolve 2) (:connecting 3) (:cant-connect 4)
 (:connected 5) (:requesting 6) (:body 7) (:connection-error 8)
 (:tls-handshake-error 9))


(defgenum (httpclient+response-code :class 'httpclient) (:continue 100)
 (:switching-protocols 101) (:processing 102) (:ok 200) (:created 201)
 (:accepted 202) (:non-authoritative-information 203) (:no-content 204)
 (:reset-content 205) (:partial-content 206) (:multi-status 207)
 (:already-reported 208) (:im-used 226) (:multiple-choices 300)
 (:moved-permanently 301) (:found 302) (:see-other 303) (:not-modified 304)
 (:use-proxy 305) (:switch-proxy 306) (:temporary-redirect 307)
 (:permanent-redirect 308) (:bad-request 400) (:unauthorized 401)
 (:payment-required 402) (:forbidden 403) (:not-found 404)
 (:method-not-allowed 405) (:not-acceptable 406)
 (:proxy-authentication-required 407) (:request-timeout 408) (:conflict 409)
 (:gone 410) (:length-required 411) (:precondition-failed 412)
 (:request-entity-too-large 413) (:request-uri-too-long 414)
 (:unsupported-media-type 415) (:requested-range-not-satisfiable 416)
 (:expectation-failed 417) (:im-a-teapot 418) (:misdirected-request 421)
 (:unprocessable-entity 422) (:locked 423) (:failed-dependency 424)
 (:upgrade-required 426) (:precondition-required 428) (:too-many-requests 429)
 (:request-header-fields-too-large 431) (:unavailable-for-legal-reasons 451)
 (:internal-server-error 500) (:not-implemented 501) (:bad-gateway 502)
 (:service-unavailable 503) (:gateway-timeout 504)
 (:http-version-not-supported 505) (:variant-also-negotiates 506)
 (:insufficient-storage 507) (:loop-detected 508) (:not-extended 510)
 (:network-auth-required 511))

(defgclass (httprequest :bind "HTTPRequest" :api :core)
 (:signals
  (request-completed result int response-code int headers packed-string-array
   body packed-byte-array)))


(defgenum (httprequest+result :class 'httprequest) (:success 0)
 (:chunked-body-size-mismatch 1) (:cant-connect 2) (:cant-resolve 3)
 (:connection-error 4) (:tls-handshake-error 5) (:no-response 6)
 (:body-size-limit-exceeded 7) (:body-decompress-failed 8) (:request-failed 9)
 (:download-file-cant-open 10) (:download-file-write-error 11)
 (:redirect-limit-reached 12) (:timeout 13))

(defgclass
 (hashing-context :bind "HashingContext" :api :core :refcounted common-lisp:t))


(defgenum (hashing-context+hash-type :class 'hashing-context) (:md5 0)
 (:sha1 1) (:sha256 2))

(defgclass
 (height-map-shape-3d :bind "HeightMapShape3D" :api :core :refcounted
  common-lisp:t))

(defgclass (hinge-joint-3d :bind "HingeJoint3D" :api :core))


(defgenum (hinge-joint-3d+param :class 'hinge-joint-3d) (:bias 0)
 (:limit-upper 1) (:limit-lower 2) (:limit-bias 3) (:limit-softness 4)
 (:limit-relaxation 5) (:motor-target-velocity 6) (:motor-max-impulse 7)
 (:max 8))


(defgenum (hinge-joint-3d+flag :class 'hinge-joint-3d) (:use-limit 0)
 (:enable-motor 1) (:max 2))

(defgclass
 (ikmodifier-3d :bind "IKModifier3D" :api :core :instantiable common-lisp:nil))

(defgclass (ip :bind "IP" :api :core :instantiable common-lisp:nil))


(defgconstant +ip+resolver-max-queries+ :value 256 :bind "RESOLVER_MAX_QUERIES"
 :class 'ip)


(defgconstant +ip+resolver-invalid-id+ :value -1 :bind "RESOLVER_INVALID_ID"
 :class 'ip)


(defgenum (ip+resolver-status :class 'ip) (:none 0) (:waiting 1) (:done 2)
 (:error 3))


(defgenum (ip+type :class 'ip) (:none 0) (:ipv4 1) (:ipv6 2) (:any 3))

(defgclass (image :bind "Image" :api :core :refcounted common-lisp:t))


(defgconstant +image+max-width+ :value 16777216 :bind "MAX_WIDTH" :class 'image)


(defgconstant +image+max-height+ :value 16777216 :bind "MAX_HEIGHT" :class
 'image)


(defgenum (image+format :class 'image) (:l8 0) (:la8 1) (:r8 2) (:rg8 3)
 (:rgb8 4) (:rgba8 5) (:rgba4444 6) (:rgb565 7) (:rf 8) (:rgf 9) (:rgbf 10)
 (:rgbaf 11) (:rh 12) (:rgh 13) (:rgbh 14) (:rgbah 15) (:rgbe9995 16)
 (:dxt1 17) (:dxt3 18) (:dxt5 19) (:rgtc-r 20) (:rgtc-rg 21) (:bptc-rgba 22)
 (:bptc-rgbf 23) (:bptc-rgbfu 24) (:etc 25) (:etc2-r11 26) (:etc2-r11s 27)
 (:etc2-rg11 28) (:etc2-rg11s 29) (:etc2-rgb8 30) (:etc2-rgba8 31)
 (:etc2-rgb8a1 32) (:etc2-ra-as-rg 33) (:dxt5-ra-as-rg 34) (:astc-4x4 35)
 (:astc-4x4-hdr 36) (:astc-8x8 37) (:astc-8x8-hdr 38) (:r16 39) (:rg16 40)
 (:rgb16 41) (:rgba16 42) (:r16i 43) (:rg16i 44) (:rgb16i 45) (:rgba16i 46)
 (:max 47))


(defgenum (image+interpolation :class 'image) (:nearest 0) (:bilinear 1)
 (:cubic 2) (:trilinear 3) (:lanczos 4))


(defgenum (image+alpha-mode :class 'image) (:none 0) (:bit 1) (:blend 2))


(defgenum (image+compress-mode :class 'image) (:s3tc 0) (:etc 1) (:etc2 2)
 (:bptc 3) (:astc 4) (:max 5))


(defgenum (image+used-channels :class 'image) (:l 0) (:la 1) (:r 2) (:rg 3)
 (:rgb 4) (:rgba 5))


(defgenum (image+compress-source :class 'image) (:generic 0) (:srgb 1)
 (:normal 2))


(defgenum (image+astcformat :class 'image) (:|4X4| 0) (:|8X8| 1))

(defgclass
 (image-format-loader :bind "ImageFormatLoader" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))


(defgenum
 (image-format-loader+loader-flags :bitfield common-lisp:t :class
  'image-format-loader)
 (:none 0) (:force-linear 1) (:convert-colors 2))

(defgclass
 (image-format-loader-extension :bind "ImageFormatLoaderExtension" :api :core
  :refcounted common-lisp:t))

(defgclass
 (image-texture :bind "ImageTexture" :api :core :refcounted common-lisp:t))

(defgclass
 (image-texture-3d :bind "ImageTexture3D" :api :core :refcounted common-lisp:t))

(defgclass
 (image-texture-layered :bind "ImageTextureLayered" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (immediate-mesh :bind "ImmediateMesh" :api :core :refcounted common-lisp:t))

(defgclass
 (importer-mesh :bind "ImporterMesh" :api :core :refcounted common-lisp:t))

(defgclass
 (importer-mesh-instance-3d :bind "ImporterMeshInstance3D" :api :core))

(defgclass (input :bind "Input" :api :core :instantiable common-lisp:nil)
 (:signals (joy-connection-changed device int connected bool)))


(defgenum (input+mouse-mode :class 'input) (:visible 0) (:hidden 1)
 (:captured 2) (:confined 3) (:confined-hidden 4) (:max 5))


(defgenum (input+cursor-shape :class 'input) (:arrow 0) (:ibeam 1)
 (:pointing-hand 2) (:cross 3) (:wait 4) (:busy 5) (:drag 6) (:can-drop 7)
 (:forbidden 8) (:vsize 9) (:hsize 10) (:bdiagsize 11) (:fdiagsize 12)
 (:move 13) (:vsplit 14) (:hsplit 15) (:help 16))

(defgclass
 (input-event :bind "InputEvent" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))


(defgconstant +input-event+device-id-emulation+ :value -1 :bind
 "DEVICE_ID_EMULATION" :class 'input-event)


(defgconstant +input-event+device-id-keyboard+ :value 16 :bind
 "DEVICE_ID_KEYBOARD" :class 'input-event)


(defgconstant +input-event+device-id-mouse+ :value 32 :bind "DEVICE_ID_MOUSE"
 :class 'input-event)

(defgclass
 (input-event-action :bind "InputEventAction" :api :core :refcounted
  common-lisp:t))

(defgclass
 (input-event-from-window :bind "InputEventFromWindow" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (input-event-gesture :bind "InputEventGesture" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (input-event-joypad-button :bind "InputEventJoypadButton" :api :core
  :refcounted common-lisp:t))

(defgclass
 (input-event-joypad-motion :bind "InputEventJoypadMotion" :api :core
  :refcounted common-lisp:t))

(defgclass
 (input-event-key :bind "InputEventKey" :api :core :refcounted common-lisp:t))

(defgclass
 (input-event-midi :bind "InputEventMIDI" :api :core :refcounted common-lisp:t))

(defgclass
 (input-event-magnify-gesture :bind "InputEventMagnifyGesture" :api :core
  :refcounted common-lisp:t))

(defgclass
 (input-event-mouse :bind "InputEventMouse" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (input-event-mouse-button :bind "InputEventMouseButton" :api :core :refcounted
  common-lisp:t))

(defgclass
 (input-event-mouse-motion :bind "InputEventMouseMotion" :api :core :refcounted
  common-lisp:t))

(defgclass
 (input-event-pan-gesture :bind "InputEventPanGesture" :api :core :refcounted
  common-lisp:t))

(defgclass
 (input-event-screen-drag :bind "InputEventScreenDrag" :api :core :refcounted
  common-lisp:t))

(defgclass
 (input-event-screen-touch :bind "InputEventScreenTouch" :api :core :refcounted
  common-lisp:t))

(defgclass
 (input-event-shortcut :bind "InputEventShortcut" :api :core :refcounted
  common-lisp:t))

(defgclass
 (input-event-with-modifiers :bind "InputEventWithModifiers" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass (input-map :bind "InputMap" :api :core)
 (:signals (project-settings-loaded)))

(defgclass
 (instance-placeholder :bind "InstancePlaceholder" :api :core :instantiable
  common-lisp:nil))

(defgclass
 (interval-tweener :bind "IntervalTweener" :api :core :refcounted
  common-lisp:t))

(defgclass (item-list :bind "ItemList" :api :core)
 (:signals (item-selected index int)
  (empty-clicked at-position vector-2 mouse-button-index int)
  (item-clicked index int at-position vector-2 mouse-button-index int)
  (multi-selected index int selected bool) (item-activated index int)))


(defgenum (item-list+icon-mode :class 'item-list) (:top 0) (:left 1))


(defgenum (item-list+select-mode :class 'item-list) (:single 0) (:multi 1)
 (:toggle 2))


(defgenum (item-list+scroll-hint-mode :class 'item-list) (:disabled 0)
 (:both 1) (:top 2) (:bottom 3))

(defgclass
 (iterate-ik3d :bind "IterateIK3D" :api :core :instantiable common-lisp:nil))

(defgclass (jnisingleton :bind "JNISingleton" :api :core))

(defgclass (json :bind "JSON" :api :core :refcounted common-lisp:t))

(defgclass (jsonrpc :bind "JSONRPC" :api :core))


(defgenum (jsonrpc+error-code :class 'jsonrpc) (:parse-error -32700)
 (:invalid-request -32600) (:method-not-found -32601) (:invalid-params -32602)
 (:internal-error -32603))

(defgclass (jacobian-ik3d :bind "JacobianIK3D" :api :core))

(defgclass (java-class :bind "JavaClass" :api :core :refcounted common-lisp:t))

(defgclass (java-class-wrapper :bind "JavaClassWrapper" :api :core))

(defgclass
 (java-object :bind "JavaObject" :api :core :refcounted common-lisp:t))

(defgclass
 (java-script-bridge :bind "JavaScriptBridge" :api :core :instantiable
  common-lisp:nil)
 (:signals (pwa-update-available)))

(defgclass
 (java-script-object :bind "JavaScriptObject" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))

(defgclass (joint-2d :bind "Joint2D" :api :core :instantiable common-lisp:nil))

(defgclass (joint-3d :bind "Joint3D" :api :core :instantiable common-lisp:nil))

(defgclass
 (joint-limitation-3d :bind "JointLimitation3D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (joint-limitation-cone-3d :bind "JointLimitationCone3D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (kinematic-collision-2d :bind "KinematicCollision2D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (kinematic-collision-3d :bind "KinematicCollision3D" :api :core :refcounted
  common-lisp:t))

(defgclass (label :bind "Label" :api :core))

(defgclass (label-3d :bind "Label3D" :api :core))


(defgenum (label-3d+draw-flags :class 'label-3d) (:shaded 0) (:double-sided 1)
 (:disable-depth-test 2) (:fixed-size 3) (:max 4))


(defgenum (label-3d+alpha-cut-mode :class 'label-3d) (:disabled 0) (:discard 1)
 (:opaque-prepass 2) (:hash 3))

(defgclass
 (label-settings :bind "LabelSettings" :api :core :refcounted common-lisp:t))

(defgclass (light-2d :bind "Light2D" :api :core :instantiable common-lisp:nil))


(defgenum (light-2d+shadow-filter :class 'light-2d) (:none 0) (:pcf5 1)
 (:pcf13 2))


(defgenum (light-2d+blend-mode :class 'light-2d) (:add 0) (:sub 1) (:mix 2))

(defgclass (light-3d :bind "Light3D" :api :core :instantiable common-lisp:nil))


(defgenum (light-3d+param :class 'light-3d) (:energy 0) (:indirect-energy 1)
 (:volumetric-fog-energy 2) (:specular 3) (:range 4) (:size 5) (:attenuation 6)
 (:spot-angle 7) (:spot-attenuation 8) (:shadow-max-distance 9)
 (:shadow-split-1-offset 10) (:shadow-split-2-offset 11)
 (:shadow-split-3-offset 12) (:shadow-fade-start 13) (:shadow-normal-bias 14)
 (:shadow-bias 15) (:shadow-pancake-size 16) (:shadow-opacity 17)
 (:shadow-blur 18) (:transmittance-bias 19) (:intensity 20) (:max 21))


(defgenum (light-3d+bake-mode :class 'light-3d) (:disabled 0) (:static 1)
 (:dynamic 2))

(defgclass (light-occluder-2d :bind "LightOccluder2D" :api :core))

(defgclass (lightmap-gi :bind "LightmapGI" :api :core))


(defgenum (lightmap-gi+bake-quality :class 'lightmap-gi) (:low 0) (:medium 1)
 (:high 2) (:ultra 3))


(defgenum (lightmap-gi+generate-probes :class 'lightmap-gi) (:disabled 0)
 (:subdiv-4 1) (:subdiv-8 2) (:subdiv-16 3) (:subdiv-32 4))


(defgenum (lightmap-gi+bake-error :class 'lightmap-gi) (:ok 0)
 (:no-scene-root 1) (:foreign-data 2) (:no-lightmapper 3) (:no-save-path 4)
 (:no-meshes 5) (:meshes-invalid 6) (:cant-create-image 7) (:user-aborted 8)
 (:texture-size-too-small 9) (:lightmap-too-small 10) (:atlas-too-small 11))


(defgenum (lightmap-gi+environment-mode :class 'lightmap-gi) (:disabled 0)
 (:scene 1) (:custom-sky 2) (:custom-color 3))

(defgclass
 (lightmap-gidata :bind "LightmapGIData" :api :core :refcounted common-lisp:t))


(defgenum (lightmap-gidata+shadowmask-mode :class 'lightmap-gidata) (:none 0)
 (:replace 1) (:overlay 2))

(defgclass (lightmap-probe :bind "LightmapProbe" :api :core))

(defgclass
 (lightmapper :bind "Lightmapper" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))

(defgclass
 (lightmapper-rd :bind "LightmapperRD" :api :core :refcounted common-lisp:t))

(defgclass
 (limit-angular-velocity-modifier-3d :bind "LimitAngularVelocityModifier3D"
  :api :core))

(defgclass (line-2d :bind "Line2D" :api :core))


(defgenum (line-2d+line-joint-mode :class 'line-2d) (:sharp 0) (:bevel 1)
 (:round 2))


(defgenum (line-2d+line-cap-mode :class 'line-2d) (:none 0) (:box 1) (:round 2))


(defgenum (line-2d+line-texture-mode :class 'line-2d) (:none 0) (:tile 1)
 (:stretch 2))

(defgclass (line-edit :bind "LineEdit" :api :core)
 (:signals (text-changed new-text string)
  (text-change-rejected rejected-substring string)
  (text-submitted new-text string) (editing-toggled toggled-on bool)))


(defgenum (line-edit+menu-items :class 'line-edit) (:cut 0) (:copy 1)
 (:paste 2) (:clear 3) (:select-all 4) (:undo 5) (:redo 6)
 (:submenu-text-dir 7) (:dir-inherited 8) (:dir-auto 9) (:dir-ltr 10)
 (:dir-rtl 11) (:display-ucc 12) (:submenu-insert-ucc 13) (:insert-lrm 14)
 (:insert-rlm 15) (:insert-lre 16) (:insert-rle 17) (:insert-lro 18)
 (:insert-rlo 19) (:insert-pdf 20) (:insert-alm 21) (:insert-lri 22)
 (:insert-rli 23) (:insert-fsi 24) (:insert-pdi 25) (:insert-zwj 26)
 (:insert-zwnj 27) (:insert-wj 28) (:insert-shy 29) (:emoji-and-symbol 30)
 (:max 31))


(defgenum (line-edit+virtual-keyboard-type :class 'line-edit) (:default 0)
 (:multiline 1) (:number 2) (:number-decimal 3) (:phone 4) (:email-address 5)
 (:password 6) (:url 7))


(defgenum (line-edit+expand-mode :class 'line-edit) (:original-size 0)
 (:fit-to-text 1) (:fit-to-line-edit 2))

(defgclass (link-button :bind "LinkButton" :api :core))


(defgenum (link-button+underline-mode :class 'link-button) (:always 0)
 (:on-hover 1) (:never 2))

(defgclass (logger :bind "Logger" :api :core :refcounted common-lisp:t))


(defgenum (logger+error-type :class 'logger) (:error 0) (:warning 1)
 (:script 2) (:shader 3))

(defgclass (look-at-modifier-3d :bind "LookAtModifier3D" :api :core))


(defgenum (look-at-modifier-3d+origin-from :class 'look-at-modifier-3d)
 (:self 0) (:specific-bone 1) (:external-node 2))

(defgclass (main-loop :bind "MainLoop" :api :core)
 (:signals (on-request-permissions-result permission string granted bool)))


(defgconstant +main-loop+notification-os-memory-warning+ :value 2009 :bind
 "NOTIFICATION_OS_MEMORY_WARNING" :class 'main-loop)


(defgconstant +main-loop+notification-translation-changed+ :value 2010 :bind
 "NOTIFICATION_TRANSLATION_CHANGED" :class 'main-loop)


(defgconstant +main-loop+notification-wm-about+ :value 2011 :bind
 "NOTIFICATION_WM_ABOUT" :class 'main-loop)


(defgconstant +main-loop+notification-crash+ :value 2012 :bind
 "NOTIFICATION_CRASH" :class 'main-loop)


(defgconstant +main-loop+notification-os-ime-update+ :value 2013 :bind
 "NOTIFICATION_OS_IME_UPDATE" :class 'main-loop)


(defgconstant +main-loop+notification-application-resumed+ :value 2014 :bind
 "NOTIFICATION_APPLICATION_RESUMED" :class 'main-loop)


(defgconstant +main-loop+notification-application-paused+ :value 2015 :bind
 "NOTIFICATION_APPLICATION_PAUSED" :class 'main-loop)


(defgconstant +main-loop+notification-application-focus-in+ :value 2016 :bind
 "NOTIFICATION_APPLICATION_FOCUS_IN" :class 'main-loop)


(defgconstant +main-loop+notification-application-focus-out+ :value 2017 :bind
 "NOTIFICATION_APPLICATION_FOCUS_OUT" :class 'main-loop)


(defgconstant +main-loop+notification-text-server-changed+ :value 2018 :bind
 "NOTIFICATION_TEXT_SERVER_CHANGED" :class 'main-loop)


(defgconstant +main-loop+notification-application-pip-mode-entered+ :value 2019
 :bind "NOTIFICATION_APPLICATION_PIP_MODE_ENTERED" :class 'main-loop)


(defgconstant +main-loop+notification-application-pip-mode-exited+ :value 2020
 :bind "NOTIFICATION_APPLICATION_PIP_MODE_EXITED" :class 'main-loop)

(defgclass (margin-container :bind "MarginContainer" :api :core))

(defgclass (marker-2d :bind "Marker2D" :api :core))

(defgclass (marker-3d :bind "Marker3D" :api :core))

(defgclass (marshalls :bind "Marshalls" :api :core))

(defgclass (material :bind "Material" :api :core :refcounted common-lisp:t))


(defgconstant +material+render-priority-max+ :value 127 :bind
 "RENDER_PRIORITY_MAX" :class 'material)


(defgconstant +material+render-priority-min+ :value -128 :bind
 "RENDER_PRIORITY_MIN" :class 'material)

(defgclass (menu-bar :bind "MenuBar" :api :core))

(defgclass (menu-button :bind "MenuButton" :api :core)
 (:signals (about-to-popup)))

(defgclass (mesh :bind "Mesh" :api :core :refcounted common-lisp:t))


(defgenum (mesh+primitive-type :class 'mesh) (:points 0) (:lines 1)
 (:line-strip 2) (:triangles 3) (:triangle-strip 4))


(defgenum (mesh+array-type :class 'mesh) (:vertex 0) (:normal 1) (:tangent 2)
 (:color 3) (:tex-uv 4) (:tex-uv2 5) (:custom0 6) (:custom1 7) (:custom2 8)
 (:custom3 9) (:bones 10) (:weights 11) (:index 12) (:max 13))


(defgenum (mesh+array-custom-format :class 'mesh) (:rgba8-unorm 0)
 (:rgba8-snorm 1) (:rg-half 2) (:rgba-half 3) (:r-float 4) (:rg-float 5)
 (:rgb-float 6) (:rgba-float 7) (:max 8))


(defgenum (mesh+array-format :bitfield common-lisp:t :class 'mesh)
 (:format-vertex 1) (:format-normal 2) (:format-tangent 4) (:format-color 8)
 (:format-tex-uv 16) (:format-tex-uv2 32) (:format-custom0 64)
 (:format-custom1 128) (:format-custom2 256) (:format-custom3 512)
 (:format-bones 1024) (:format-weights 2048) (:format-index 4096)
 (:format-blend-shape-mask 7) (:format-custom-base 13) (:format-custom-bits 3)
 (:format-custom0-shift 13) (:format-custom1-shift 16)
 (:format-custom2-shift 19) (:format-custom3-shift 22) (:format-custom-mask 7)
 (:compress-flags-base 25) (:flag-use-2d-vertices 33554432)
 (:flag-use-dynamic-update 67108864) (:flag-use-8-bone-weights 134217728)
 (:flag-uses-empty-vertex-array 268435456)
 (:flag-compress-attributes 536870912))


(defgenum (mesh+blend-shape-mode :class 'mesh) (:normalized 0) (:relative 1))

(defgclass
 (mesh-convex-decomposition-settings :bind "MeshConvexDecompositionSettings"
  :api :core :refcounted common-lisp:t))


(defgenum
 (mesh-convex-decomposition-settings+mode :class
  'mesh-convex-decomposition-settings)
 (:voxel 0) (:tetrahedron 1))

(defgclass
 (mesh-data-tool :bind "MeshDataTool" :api :core :refcounted common-lisp:t))

(defgclass (mesh-instance-2d :bind "MeshInstance2D" :api :core)
 (:signals (texture-changed)))

(defgclass (mesh-instance-3d :bind "MeshInstance3D" :api :core))

(defgclass
 (mesh-library :bind "MeshLibrary" :api :core :refcounted common-lisp:t))

(defgclass
 (mesh-texture :bind "MeshTexture" :api :core :refcounted common-lisp:t))

(defgclass
 (method-tweener :bind "MethodTweener" :api :core :refcounted common-lisp:t))

(defgclass (missing-node :bind "MissingNode" :api :core))

(defgclass
 (missing-resource :bind "MissingResource" :api :core :refcounted
  common-lisp:t))

(defgclass
 (mobile-vrinterface :bind "MobileVRInterface" :api :core :refcounted
  common-lisp:t))

(defgclass (modifier-bone-target-3d :bind "ModifierBoneTarget3D" :api :core))

(defgclass (movie-writer :bind "MovieWriter" :api :core))

(defgclass (multi-mesh :bind "MultiMesh" :api :core :refcounted common-lisp:t))


(defgenum (multi-mesh+transform-format :class 'multi-mesh) (:|2D| 0) (:|3D| 1))


(defgenum (multi-mesh+physics-interpolation-quality :class 'multi-mesh)
 (:fast 0) (:high 1))

(defgclass (multi-mesh-instance-2d :bind "MultiMeshInstance2D" :api :core)
 (:signals (texture-changed)))

(defgclass (multi-mesh-instance-3d :bind "MultiMeshInstance3D" :api :core))

(defgclass
 (multiplayer-api :bind "MultiplayerAPI" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t)
 (:signals (peer-connected id int) (peer-disconnected id int)
  (connected-to-server) (connection-failed) (server-disconnected)))


(defgenum (multiplayer-api+rpcmode :class 'multiplayer-api) (:disabled 0)
 (:any-peer 1) (:authority 2))

(defgclass
 (multiplayer-apiextension :bind "MultiplayerAPIExtension" :api :core
  :refcounted common-lisp:t))

(defgclass
 (multiplayer-peer :bind "MultiplayerPeer" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t)
 (:signals (peer-connected id int) (peer-disconnected id int)))


(defgconstant +multiplayer-peer+target-peer-broadcast+ :value 0 :bind
 "TARGET_PEER_BROADCAST" :class 'multiplayer-peer)


(defgconstant +multiplayer-peer+target-peer-server+ :value 1 :bind
 "TARGET_PEER_SERVER" :class 'multiplayer-peer)


(defgenum (multiplayer-peer+connection-status :class 'multiplayer-peer)
 (:disconnected 0) (:connecting 1) (:connected 2))


(defgenum (multiplayer-peer+transfer-mode :class 'multiplayer-peer)
 (:unreliable 0) (:unreliable-ordered 1) (:reliable 2))

(defgclass
 (multiplayer-peer-extension :bind "MultiplayerPeerExtension" :api :core
  :refcounted common-lisp:t))

(defgclass (multiplayer-spawner :bind "MultiplayerSpawner" :api :core)
 (:signals (despawned node node) (spawned node node)))

(defgclass
 (multiplayer-synchronizer :bind "MultiplayerSynchronizer" :api :core)
 (:signals (synchronized) (delta-synchronized)
  (visibility-changed for-peer int)))


(defgenum
 (multiplayer-synchronizer+visibility-update-mode :class
  'multiplayer-synchronizer)
 (:idle 0) (:physics 1) (:none 2))

(defgclass (mutex :bind "Mutex" :api :core :refcounted common-lisp:t))

(defgclass (native-menu :bind "NativeMenu" :api :core))


(defgenum (native-menu+feature :class 'native-menu) (:global-menu 0)
 (:popup-menu 1) (:open-close-callback 2) (:hover-callback 3) (:key-callback 4))


(defgenum (native-menu+system-menus :class 'native-menu) (:invalid-menu-id 0)
 (:main-menu-id 1) (:application-menu-id 2) (:window-menu-id 3)
 (:help-menu-id 4) (:dock-menu-id 5))

(defgclass (navigation-agent-2d :bind "NavigationAgent2D" :api :core)
 (:signals (path-changed) (target-reached)
  (waypoint-reached details dictionary) (link-reached details dictionary)
  (navigation-finished) (velocity-computed safe-velocity vector-2)))

(defgclass (navigation-agent-3d :bind "NavigationAgent3D" :api :core)
 (:signals (path-changed) (target-reached)
  (waypoint-reached details dictionary) (link-reached details dictionary)
  (navigation-finished) (velocity-computed safe-velocity vector-3)))

(defgclass (navigation-link-2d :bind "NavigationLink2D" :api :core))

(defgclass (navigation-link-3d :bind "NavigationLink3D" :api :core))

(defgclass
 (navigation-mesh :bind "NavigationMesh" :api :core :refcounted common-lisp:t))


(defgenum (navigation-mesh+sample-partition-type :class 'navigation-mesh)
 (:watershed 0) (:monotone 1) (:layers 2) (:max 3))


(defgenum (navigation-mesh+parsed-geometry-type :class 'navigation-mesh)
 (:mesh-instances 0) (:static-colliders 1) (:both 2) (:max 3))


(defgenum (navigation-mesh+source-geometry-mode :class 'navigation-mesh)
 (:root-node-children 0) (:groups-with-children 1) (:groups-explicit 2)
 (:max 3))

(defgclass
 (navigation-mesh-generator :bind "NavigationMeshGenerator" :api :core))

(defgclass
 (navigation-mesh-source-geometry-data-2d :bind
  "NavigationMeshSourceGeometryData2D" :api :core :refcounted common-lisp:t))

(defgclass
 (navigation-mesh-source-geometry-data-3d :bind
  "NavigationMeshSourceGeometryData3D" :api :core :refcounted common-lisp:t))

(defgclass (navigation-obstacle-2d :bind "NavigationObstacle2D" :api :core))

(defgclass (navigation-obstacle-3d :bind "NavigationObstacle3D" :api :core))

(defgclass
 (navigation-path-query-parameters-2d :bind "NavigationPathQueryParameters2D"
  :api :core :refcounted common-lisp:t))


(defgenum
 (navigation-path-query-parameters-2d+pathfinding-algorithm :class
  'navigation-path-query-parameters-2d)
 (:pathfinding-algorithm-astar 0))


(defgenum
 (navigation-path-query-parameters-2d+path-post-processing :class
  'navigation-path-query-parameters-2d)
 (:corridorfunnel 0) (:edgecentered 1) (:none 2))


(defgenum
 (navigation-path-query-parameters-2d+path-metadata-flags :bitfield
  common-lisp:t :class 'navigation-path-query-parameters-2d)
 (:none 0) (:types 1) (:rids 2) (:owners 4) (:all 7))

(defgclass
 (navigation-path-query-parameters-3d :bind "NavigationPathQueryParameters3D"
  :api :core :refcounted common-lisp:t))


(defgenum
 (navigation-path-query-parameters-3d+pathfinding-algorithm :class
  'navigation-path-query-parameters-3d)
 (:pathfinding-algorithm-astar 0))


(defgenum
 (navigation-path-query-parameters-3d+path-post-processing :class
  'navigation-path-query-parameters-3d)
 (:corridorfunnel 0) (:edgecentered 1) (:none 2))


(defgenum
 (navigation-path-query-parameters-3d+path-metadata-flags :bitfield
  common-lisp:t :class 'navigation-path-query-parameters-3d)
 (:none 0) (:types 1) (:rids 2) (:owners 4) (:all 7))

(defgclass
 (navigation-path-query-result-2d :bind "NavigationPathQueryResult2D" :api
  :core :refcounted common-lisp:t))


(defgenum
 (navigation-path-query-result-2d+path-segment-type :class
  'navigation-path-query-result-2d)
 (:region 0) (:link 1))

(defgclass
 (navigation-path-query-result-3d :bind "NavigationPathQueryResult3D" :api
  :core :refcounted common-lisp:t))


(defgenum
 (navigation-path-query-result-3d+path-segment-type :class
  'navigation-path-query-result-3d)
 (:region 0) (:link 1))

(defgclass
 (navigation-polygon :bind "NavigationPolygon" :api :core :refcounted
  common-lisp:t))


(defgenum (navigation-polygon+sample-partition-type :class 'navigation-polygon)
 (:convex-partition 0) (:triangulate 1) (:max 2))


(defgenum (navigation-polygon+parsed-geometry-type :class 'navigation-polygon)
 (:mesh-instances 0) (:static-colliders 1) (:both 2) (:max 3))


(defgenum (navigation-polygon+source-geometry-mode :class 'navigation-polygon)
 (:root-node-children 0) (:groups-with-children 1) (:groups-explicit 2)
 (:max 3))

(defgclass (navigation-region-2d :bind "NavigationRegion2D" :api :core)
 (:signals (navigation-polygon-changed) (bake-finished)))

(defgclass (navigation-region-3d :bind "NavigationRegion3D" :api :core)
 (:signals (navigation-mesh-changed) (bake-finished)))

(defgclass
 (navigation-server-2d :bind "NavigationServer2D" :api :core :instantiable
  common-lisp:nil)
 (:signals (map-changed map rid) (navigation-debug-changed)
  (avoidance-debug-changed)))


(defgenum (navigation-server-2d+process-info :class 'navigation-server-2d)
 (:active-maps 0) (:region-count 1) (:agent-count 2) (:link-count 3)
 (:polygon-count 4) (:edge-count 5) (:edge-merge-count 6)
 (:edge-connection-count 7) (:edge-free-count 8) (:obstacle-count 9))

(defgclass
 (navigation-server-2dmanager :bind "NavigationServer2DManager" :api :core))

(defgclass
 (navigation-server-3d :bind "NavigationServer3D" :api :core :instantiable
  common-lisp:nil)
 (:signals (map-changed map rid) (navigation-debug-changed)
  (avoidance-debug-changed)))


(defgenum (navigation-server-3d+process-info :class 'navigation-server-3d)
 (:active-maps 0) (:region-count 1) (:agent-count 2) (:link-count 3)
 (:polygon-count 4) (:edge-count 5) (:edge-merge-count 6)
 (:edge-connection-count 7) (:edge-free-count 8) (:obstacle-count 9))

(defgclass
 (navigation-server-3dmanager :bind "NavigationServer3DManager" :api :core))

(defgclass (nine-patch-rect :bind "NinePatchRect" :api :core)
 (:signals (texture-changed)))


(defgenum (nine-patch-rect+axis-stretch-mode :class 'nine-patch-rect)
 (:stretch 0) (:tile 1) (:tile-fit 2))

(defgclass (node :bind "Node" :api :core)
 (:signals (ready) (renamed) (tree-entered) (tree-exiting) (tree-exited)
  (child-entered-tree node node) (child-exiting-tree node node)
  (child-order-changed) (replacing-by node node)
  (editor-description-changed node node) (editor-state-changed)))


(defgconstant +node+notification-enter-tree+ :value 10 :bind
 "NOTIFICATION_ENTER_TREE" :class 'node)


(defgconstant +node+notification-exit-tree+ :value 11 :bind
 "NOTIFICATION_EXIT_TREE" :class 'node)


(defgconstant +node+notification-moved-in-parent+ :value 12 :bind
 "NOTIFICATION_MOVED_IN_PARENT" :class 'node)


(defgconstant +node+notification-ready+ :value 13 :bind "NOTIFICATION_READY"
 :class 'node)


(defgconstant +node+notification-paused+ :value 14 :bind "NOTIFICATION_PAUSED"
 :class 'node)


(defgconstant +node+notification-unpaused+ :value 15 :bind
 "NOTIFICATION_UNPAUSED" :class 'node)


(defgconstant +node+notification-physics-process+ :value 16 :bind
 "NOTIFICATION_PHYSICS_PROCESS" :class 'node)


(defgconstant +node+notification-process+ :value 17 :bind
 "NOTIFICATION_PROCESS" :class 'node)


(defgconstant +node+notification-parented+ :value 18 :bind
 "NOTIFICATION_PARENTED" :class 'node)


(defgconstant +node+notification-unparented+ :value 19 :bind
 "NOTIFICATION_UNPARENTED" :class 'node)


(defgconstant +node+notification-scene-instantiated+ :value 20 :bind
 "NOTIFICATION_SCENE_INSTANTIATED" :class 'node)


(defgconstant +node+notification-drag-begin+ :value 21 :bind
 "NOTIFICATION_DRAG_BEGIN" :class 'node)


(defgconstant +node+notification-drag-end+ :value 22 :bind
 "NOTIFICATION_DRAG_END" :class 'node)


(defgconstant +node+notification-path-renamed+ :value 23 :bind
 "NOTIFICATION_PATH_RENAMED" :class 'node)


(defgconstant +node+notification-child-order-changed+ :value 24 :bind
 "NOTIFICATION_CHILD_ORDER_CHANGED" :class 'node)


(defgconstant +node+notification-internal-process+ :value 25 :bind
 "NOTIFICATION_INTERNAL_PROCESS" :class 'node)


(defgconstant +node+notification-internal-physics-process+ :value 26 :bind
 "NOTIFICATION_INTERNAL_PHYSICS_PROCESS" :class 'node)


(defgconstant +node+notification-post-enter-tree+ :value 27 :bind
 "NOTIFICATION_POST_ENTER_TREE" :class 'node)


(defgconstant +node+notification-disabled+ :value 28 :bind
 "NOTIFICATION_DISABLED" :class 'node)


(defgconstant +node+notification-enabled+ :value 29 :bind
 "NOTIFICATION_ENABLED" :class 'node)


(defgconstant +node+notification-reset-physics-interpolation+ :value 2001 :bind
 "NOTIFICATION_RESET_PHYSICS_INTERPOLATION" :class 'node)


(defgconstant +node+notification-editor-pre-save+ :value 9001 :bind
 "NOTIFICATION_EDITOR_PRE_SAVE" :class 'node)


(defgconstant +node+notification-editor-post-save+ :value 9002 :bind
 "NOTIFICATION_EDITOR_POST_SAVE" :class 'node)


(defgconstant +node+notification-wm-mouse-enter+ :value 1002 :bind
 "NOTIFICATION_WM_MOUSE_ENTER" :class 'node)


(defgconstant +node+notification-wm-mouse-exit+ :value 1003 :bind
 "NOTIFICATION_WM_MOUSE_EXIT" :class 'node)


(defgconstant +node+notification-wm-window-focus-in+ :value 1004 :bind
 "NOTIFICATION_WM_WINDOW_FOCUS_IN" :class 'node)


(defgconstant +node+notification-wm-window-focus-out+ :value 1005 :bind
 "NOTIFICATION_WM_WINDOW_FOCUS_OUT" :class 'node)


(defgconstant +node+notification-wm-close-request+ :value 1006 :bind
 "NOTIFICATION_WM_CLOSE_REQUEST" :class 'node)


(defgconstant +node+notification-wm-go-back-request+ :value 1007 :bind
 "NOTIFICATION_WM_GO_BACK_REQUEST" :class 'node)


(defgconstant +node+notification-wm-size-changed+ :value 1008 :bind
 "NOTIFICATION_WM_SIZE_CHANGED" :class 'node)


(defgconstant +node+notification-wm-dpi-change+ :value 1009 :bind
 "NOTIFICATION_WM_DPI_CHANGE" :class 'node)


(defgconstant +node+notification-vp-mouse-enter+ :value 1010 :bind
 "NOTIFICATION_VP_MOUSE_ENTER" :class 'node)


(defgconstant +node+notification-vp-mouse-exit+ :value 1011 :bind
 "NOTIFICATION_VP_MOUSE_EXIT" :class 'node)


(defgconstant +node+notification-wm-position-changed+ :value 1012 :bind
 "NOTIFICATION_WM_POSITION_CHANGED" :class 'node)


(defgconstant +node+notification-wm-output-max-linear-value-changed+ :value
 1013 :bind "NOTIFICATION_WM_OUTPUT_MAX_LINEAR_VALUE_CHANGED" :class 'node)


(defgconstant +node+notification-os-memory-warning+ :value 2009 :bind
 "NOTIFICATION_OS_MEMORY_WARNING" :class 'node)


(defgconstant +node+notification-translation-changed+ :value 2010 :bind
 "NOTIFICATION_TRANSLATION_CHANGED" :class 'node)


(defgconstant +node+notification-wm-about+ :value 2011 :bind
 "NOTIFICATION_WM_ABOUT" :class 'node)


(defgconstant +node+notification-crash+ :value 2012 :bind "NOTIFICATION_CRASH"
 :class 'node)


(defgconstant +node+notification-os-ime-update+ :value 2013 :bind
 "NOTIFICATION_OS_IME_UPDATE" :class 'node)


(defgconstant +node+notification-application-resumed+ :value 2014 :bind
 "NOTIFICATION_APPLICATION_RESUMED" :class 'node)


(defgconstant +node+notification-application-paused+ :value 2015 :bind
 "NOTIFICATION_APPLICATION_PAUSED" :class 'node)


(defgconstant +node+notification-application-focus-in+ :value 2016 :bind
 "NOTIFICATION_APPLICATION_FOCUS_IN" :class 'node)


(defgconstant +node+notification-application-focus-out+ :value 2017 :bind
 "NOTIFICATION_APPLICATION_FOCUS_OUT" :class 'node)


(defgconstant +node+notification-text-server-changed+ :value 2018 :bind
 "NOTIFICATION_TEXT_SERVER_CHANGED" :class 'node)


(defgconstant +node+notification-application-pip-mode-entered+ :value 2019
 :bind "NOTIFICATION_APPLICATION_PIP_MODE_ENTERED" :class 'node)


(defgconstant +node+notification-application-pip-mode-exited+ :value 2020 :bind
 "NOTIFICATION_APPLICATION_PIP_MODE_EXITED" :class 'node)


(defgconstant +node+notification-accessibility-update+ :value 3000 :bind
 "NOTIFICATION_ACCESSIBILITY_UPDATE" :class 'node)


(defgconstant +node+notification-accessibility-invalidate+ :value 3001 :bind
 "NOTIFICATION_ACCESSIBILITY_INVALIDATE" :class 'node)


(defgenum (node+process-mode :class 'node) (:inherit 0) (:pausable 1)
 (:when-paused 2) (:always 3) (:disabled 4))


(defgenum (node+process-thread-group :class 'node) (:inherit 0)
 (:main-thread 1) (:sub-thread 2))


(defgenum (node+process-thread-messages :bitfield common-lisp:t :class 'node)
 (:messages 1) (:messages-physics 2) (:messages-all 3))


(defgenum (node+physics-interpolation-mode :class 'node) (:inherit 0) (:on 1)
 (:off 2))


(defgenum (node+duplicate-flags :class 'node) (:signals 1) (:groups 2)
 (:scripts 4) (:use-instantiation 8) (:internal-state 16) (:default 15))


(defgenum (node+internal-mode :class 'node) (:disabled 0) (:front 1) (:back 2))


(defgenum (node+auto-translate-mode :class 'node) (:inherit 0) (:always 1)
 (:disabled 2))

(defgclass (node-2d :bind "Node2D" :api :core))

(defgclass (node-3d :bind "Node3D" :api :core) (:signals (visibility-changed)))


(defgconstant +node-3d+notification-transform-changed+ :value 2000 :bind
 "NOTIFICATION_TRANSFORM_CHANGED" :class 'node-3d)


(defgconstant +node-3d+notification-enter-world+ :value 41 :bind
 "NOTIFICATION_ENTER_WORLD" :class 'node-3d)


(defgconstant +node-3d+notification-exit-world+ :value 42 :bind
 "NOTIFICATION_EXIT_WORLD" :class 'node-3d)


(defgconstant +node-3d+notification-visibility-changed+ :value 43 :bind
 "NOTIFICATION_VISIBILITY_CHANGED" :class 'node-3d)


(defgconstant +node-3d+notification-local-transform-changed+ :value 44 :bind
 "NOTIFICATION_LOCAL_TRANSFORM_CHANGED" :class 'node-3d)


(defgenum (node-3d+rotation-edit-mode :class 'node-3d) (:euler 0)
 (:quaternion 1) (:basis 2))

(defgclass
 (node-3dgizmo :bind "Node3DGizmo" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))

(defgclass
 (noise :bind "Noise" :api :core :instantiable common-lisp:nil :refcounted
  common-lisp:t))

(defgclass
 (noise-texture-2d :bind "NoiseTexture2D" :api :core :refcounted common-lisp:t))

(defgclass
 (noise-texture-3d :bind "NoiseTexture3D" :api :core :refcounted common-lisp:t))

(defgclass
 (ormmaterial-3d :bind "ORMMaterial3D" :api :core :refcounted common-lisp:t))

(defgclass (os :bind "OS" :api :core))


(defgenum (os+rendering-driver :class 'os) (:vulkan 0) (:opengl3 1) (:d3d12 2)
 (:metal 3))


(defgenum (os+system-dir :class 'os) (:desktop 0) (:dcim 1) (:documents 2)
 (:downloads 3) (:movies 4) (:music 5) (:pictures 6) (:ringtones 7))


(defgenum (os+std-handle-type :class 'os) (:invalid 0) (:console 1) (:file 2)
 (:pipe 3) (:unknown 4))

(defgclass (object :bind "Object" :api :core)
 (:signals (script-changed) (property-list-changed)))


(defgconstant +object+notification-postinitialize+ :value 0 :bind
 "NOTIFICATION_POSTINITIALIZE" :class 'object)


(defgconstant +object+notification-predelete+ :value 1 :bind
 "NOTIFICATION_PREDELETE" :class 'object)


(defgconstant +object+notification-extension-reloaded+ :value 2 :bind
 "NOTIFICATION_EXTENSION_RELOADED" :class 'object)


(defgenum (object+connect-flags :bitfield common-lisp:t :class 'object)
 (:deferred 1) (:persist 2) (:one-shot 4) (:reference-counted 8)
 (:append-source-object 16))

(defgclass
 (occluder-3d :bind "Occluder3D" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))

(defgclass (occluder-instance-3d :bind "OccluderInstance3D" :api :core))

(defgclass
 (occluder-polygon-2d :bind "OccluderPolygon2D" :api :core :refcounted
  common-lisp:t))


(defgenum (occluder-polygon-2d+cull-mode :class 'occluder-polygon-2d)
 (:disabled 0) (:clockwise 1) (:counter-clockwise 2))

(defgclass
 (offline-multiplayer-peer :bind "OfflineMultiplayerPeer" :api :core
  :refcounted common-lisp:t))

(defgclass
 (ogg-packet-sequence :bind "OggPacketSequence" :api :core :refcounted
  common-lisp:t))

(defgclass
 (ogg-packet-sequence-playback :bind "OggPacketSequencePlayback" :api :core
  :refcounted common-lisp:t))

(defgclass (omni-light-3d :bind "OmniLight3D" :api :core))


(defgenum (omni-light-3d+shadow-mode :class 'omni-light-3d)
 (:dual-paraboloid 0) (:cube 1))

(defgclass
 (open-xrapiextension :bind "OpenXRAPIExtension" :api :core :refcounted
  common-lisp:t))


(defgenum
 (open-xrapiextension+open-xralpha-blend-mode-support :class
  'open-xrapiextension)
 (:none 0) (:real 1) (:emulating 2))

(defgclass
 (open-xraction :bind "OpenXRAction" :api :core :refcounted common-lisp:t))


(defgenum (open-xraction+action-type :class 'open-xraction) (:bool 0)
 (:float 1) (:vector2 2) (:pose 3))

(defgclass
 (open-xraction-binding-modifier :bind "OpenXRActionBindingModifier" :api :core
  :refcounted common-lisp:t))

(defgclass
 (open-xraction-map :bind "OpenXRActionMap" :api :core :refcounted
  common-lisp:t))

(defgclass
 (open-xraction-set :bind "OpenXRActionSet" :api :core :refcounted
  common-lisp:t))

(defgclass
 (open-xranalog-threshold-modifier :bind "OpenXRAnalogThresholdModifier" :api
  :core :refcounted common-lisp:t))

(defgclass
 (open-xranchor-tracker :bind "OpenXRAnchorTracker" :api :core :refcounted
  common-lisp:t)
 (:signals (uuid-changed)))

(defgclass
 (open-xrandroid-thread-settings-extension :bind
  "OpenXRAndroidThreadSettingsExtension" :api :core))


(defgenum
 (open-xrandroid-thread-settings-extension+thread-type :class
  'open-xrandroid-thread-settings-extension)
 (:application-main 0) (:application-worker 1) (:renderer-main 2)
 (:renderer-worker 3))

(defgclass
 (open-xrbinding-modifier :bind "OpenXRBindingModifier" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (open-xrbinding-modifier-editor :bind "OpenXRBindingModifierEditor" :api
  :editor)
 (:signals (binding-modifier-removed binding-modifier-editor object)))

(defgclass
 (open-xrcomposition-layer :bind "OpenXRCompositionLayer" :api :core
  :instantiable common-lisp:nil))


(defgenum (open-xrcomposition-layer+filter :class 'open-xrcomposition-layer)
 (:nearest 0) (:linear 1) (:cubic 2))


(defgenum
 (open-xrcomposition-layer+mipmap-mode :class 'open-xrcomposition-layer)
 (:disabled 0) (:nearest 1) (:linear 2))


(defgenum (open-xrcomposition-layer+wrap :class 'open-xrcomposition-layer)
 (:clamp-to-border 0) (:clamp-to-edge 1) (:repeat 2) (:mirrored-repeat 3)
 (:mirror-clamp-to-edge 4))


(defgenum (open-xrcomposition-layer+swizzle :class 'open-xrcomposition-layer)
 (:red 0) (:green 1) (:blue 2) (:alpha 3) (:zero 4) (:one 5))


(defgenum
 (open-xrcomposition-layer+eye-visibility :class 'open-xrcomposition-layer)
 (:both 0) (:left 1) (:right 2))

(defgclass
 (open-xrcomposition-layer-cylinder :bind "OpenXRCompositionLayerCylinder" :api
  :core))

(defgclass
 (open-xrcomposition-layer-equirect :bind "OpenXRCompositionLayerEquirect" :api
  :core))

(defgclass
 (open-xrcomposition-layer-quad :bind "OpenXRCompositionLayerQuad" :api :core))

(defgclass
 (open-xrdpad-binding-modifier :bind "OpenXRDpadBindingModifier" :api :core
  :refcounted common-lisp:t))

(defgclass (open-xrextension-wrapper :bind "OpenXRExtensionWrapper" :api :core))

(defgclass
 (open-xrextension-wrapper-extension :bind "OpenXRExtensionWrapperExtension"
  :api :core))

(defgclass
 (open-xrframe-synthesis-extension :bind "OpenXRFrameSynthesisExtension" :api
  :core))

(defgclass (open-xrfuture-extension :bind "OpenXRFutureExtension" :api :core))

(defgclass
 (open-xrfuture-result :bind "OpenXRFutureResult" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t)
 (:signals (completed result open-xrfuture-result)))


(defgenum (open-xrfuture-result+result-status :class 'open-xrfuture-result)
 (:running 0) (:finished 1) (:cancelled 2))

(defgclass (open-xrhand :bind "OpenXRHand" :api :core))


(defgenum (open-xrhand+hands :class 'open-xrhand) (:left 0) (:right 1) (:max 2))


(defgenum (open-xrhand+motion-range :class 'open-xrhand) (:unobstructed 0)
 (:conform-to-controller 1) (:max 2))


(defgenum (open-xrhand+skeleton-rig :class 'open-xrhand) (:openxr 0)
 (:humanoid 1) (:max 2))


(defgenum (open-xrhand+bone-update :class 'open-xrhand) (:full 0)
 (:rotation-only 1) (:max 2))

(defgclass
 (open-xrhaptic-base :bind "OpenXRHapticBase" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (open-xrhaptic-vibration :bind "OpenXRHapticVibration" :api :core :refcounted
  common-lisp:t))

(defgclass
 (open-xripbinding :bind "OpenXRIPBinding" :api :core :refcounted
  common-lisp:t))

(defgclass
 (open-xripbinding-modifier :bind "OpenXRIPBindingModifier" :api :core
  :refcounted common-lisp:t))

(defgclass
 (open-xrinteraction-profile :bind "OpenXRInteractionProfile" :api :core
  :refcounted common-lisp:t))

(defgclass
 (open-xrinteraction-profile-editor :bind "OpenXRInteractionProfileEditor" :api
  :editor))

(defgclass
 (open-xrinteraction-profile-editor-base :bind
  "OpenXRInteractionProfileEditorBase" :api :editor :instantiable
  common-lisp:nil))

(defgclass
 (open-xrinteraction-profile-metadata :bind "OpenXRInteractionProfileMetadata"
  :api :core))

(defgclass
 (open-xrinterface :bind "OpenXRInterface" :api :core :refcounted
  common-lisp:t)
 (:signals (session-begun) (session-stopping) (session-synchronized)
  (session-focussed) (session-visible) (session-loss-pending)
  (instance-exiting) (pose-recentered)
  (refresh-rate-changed refresh-rate float)
  (cpu-level-changed sub-domain int from-level int to-level int)
  (gpu-level-changed sub-domain int from-level int to-level int)
  (user-presence-changed is-user-present bool)))


(defgenum (open-xrinterface+session-state :class 'open-xrinterface)
 (:unknown 0) (:idle 1) (:ready 2) (:synchronized 3) (:visible 4) (:focused 5)
 (:stopping 6) (:loss-pending 7) (:exiting 8))


(defgenum (open-xrinterface+hand :class 'open-xrinterface) (:left 0) (:right 1)
 (:max 2))


(defgenum (open-xrinterface+hand-motion-range :class 'open-xrinterface)
 (:unobstructed 0) (:conform-to-controller 1) (:max 2))


(defgenum (open-xrinterface+hand-tracked-source :class 'open-xrinterface)
 (:unknown 0) (:unobstructed 1) (:controller 2) (:max 3))


(defgenum (open-xrinterface+hand-joints :class 'open-xrinterface) (:palm 0)
 (:wrist 1) (:thumb-metacarpal 2) (:thumb-proximal 3) (:thumb-distal 4)
 (:thumb-tip 5) (:index-metacarpal 6) (:index-proximal 7)
 (:index-intermediate 8) (:index-distal 9) (:index-tip 10)
 (:middle-metacarpal 11) (:middle-proximal 12) (:middle-intermediate 13)
 (:middle-distal 14) (:middle-tip 15) (:ring-metacarpal 16) (:ring-proximal 17)
 (:ring-intermediate 18) (:ring-distal 19) (:ring-tip 20)
 (:little-metacarpal 21) (:little-proximal 22) (:little-intermediate 23)
 (:little-distal 24) (:little-tip 25) (:max 26))


(defgenum (open-xrinterface+perf-settings-level :class 'open-xrinterface)
 (:power-savings 0) (:sustained-low 1) (:sustained-high 2) (:boost 3))


(defgenum (open-xrinterface+perf-settings-sub-domain :class 'open-xrinterface)
 (:compositing 0) (:rendering 1) (:thermal 2))


(defgenum
 (open-xrinterface+perf-settings-notification-level :class 'open-xrinterface)
 (:normal 0) (:warning 1) (:impaired 2))


(defgenum
 (open-xrinterface+hand-joint-flags :bitfield common-lisp:t :class
  'open-xrinterface)
 (:none 0) (:orientation-valid 1) (:orientation-tracked 2) (:position-valid 4)
 (:position-tracked 8) (:linear-velocity-valid 16) (:angular-velocity-valid 32))

(defgclass
 (open-xrmarker-tracker :bind "OpenXRMarkerTracker" :api :core :refcounted
  common-lisp:t))

(defgclass
 (open-xrplane-tracker :bind "OpenXRPlaneTracker" :api :core :refcounted
  common-lisp:t)
 (:signals (mesh-changed)))

(defgclass (open-xrrender-model :bind "OpenXRRenderModel" :api :core)
 (:signals (render-model-top-level-path-changed)))

(defgclass
 (open-xrrender-model-extension :bind "OpenXRRenderModelExtension" :api :core)
 (:signals (render-model-added render-model rid)
  (render-model-removed render-model rid)
  (render-model-top-level-path-changed render-model rid)))

(defgclass
 (open-xrrender-model-manager :bind "OpenXRRenderModelManager" :api :core)
 (:signals (render-model-added render-model open-xrrender-model)
  (render-model-removed render-model open-xrrender-model)))


(defgenum
 (open-xrrender-model-manager+render-model-tracker :class
  'open-xrrender-model-manager)
 (:any 0) (:none-set 1) (:left-hand 2) (:right-hand 3))

(defgclass
 (open-xrspatial-anchor-capability :bind "OpenXRSpatialAnchorCapability" :api
  :core))


(defgenum
 (open-xrspatial-anchor-capability+persistence-scope :class
  'open-xrspatial-anchor-capability)
 (:system-managed 1) (:local-anchors 1000781000))

(defgclass
 (open-xrspatial-capability-configuration-anchor :bind
  "OpenXRSpatialCapabilityConfigurationAnchor" :api :core :refcounted
  common-lisp:t))

(defgclass
 (open-xrspatial-capability-configuration-april-tag :bind
  "OpenXRSpatialCapabilityConfigurationAprilTag" :api :core :refcounted
  common-lisp:t))


(defgenum
 (open-xrspatial-capability-configuration-april-tag+april-tag-dict :class
  'open-xrspatial-capability-configuration-april-tag)
 (:|16H5| 1) (:|25H9| 2) (:|36H10| 3) (:|36H11| 4))

(defgclass
 (open-xrspatial-capability-configuration-aruco :bind
  "OpenXRSpatialCapabilityConfigurationAruco" :api :core :refcounted
  common-lisp:t))


(defgenum
 (open-xrspatial-capability-configuration-aruco+aruco-dict :class
  'open-xrspatial-capability-configuration-aruco)
 (:|4X4-50| 1) (:|4X4-100| 2) (:|4X4-250| 3) (:|4X4-1000| 4) (:|5X5-50| 5)
 (:|5X5-100| 6) (:|5X5-250| 7) (:|5X5-1000| 8) (:|6X6-50| 9) (:|6X6-100| 10)
 (:|6X6-250| 11) (:|6X6-1000| 12) (:|7X7-50| 13) (:|7X7-100| 14)
 (:|7X7-250| 15) (:|7X7-1000| 16))

(defgclass
 (open-xrspatial-capability-configuration-base-header :bind
  "OpenXRSpatialCapabilityConfigurationBaseHeader" :api :core :refcounted
  common-lisp:t))

(defgclass
 (open-xrspatial-capability-configuration-micro-qr-code :bind
  "OpenXRSpatialCapabilityConfigurationMicroQrCode" :api :core :refcounted
  common-lisp:t))

(defgclass
 (open-xrspatial-capability-configuration-plane-tracking :bind
  "OpenXRSpatialCapabilityConfigurationPlaneTracking" :api :core :refcounted
  common-lisp:t))

(defgclass
 (open-xrspatial-capability-configuration-qr-code :bind
  "OpenXRSpatialCapabilityConfigurationQrCode" :api :core :refcounted
  common-lisp:t))

(defgclass
 (open-xrspatial-component-anchor-list :bind "OpenXRSpatialComponentAnchorList"
  :api :core :refcounted common-lisp:t))

(defgclass
 (open-xrspatial-component-bounded-2dlist :bind
  "OpenXRSpatialComponentBounded2DList" :api :core :refcounted common-lisp:t))

(defgclass
 (open-xrspatial-component-bounded-3dlist :bind
  "OpenXRSpatialComponentBounded3DList" :api :core :refcounted common-lisp:t))

(defgclass
 (open-xrspatial-component-data :bind "OpenXRSpatialComponentData" :api :core
  :refcounted common-lisp:t))

(defgclass
 (open-xrspatial-component-marker-list :bind "OpenXRSpatialComponentMarkerList"
  :api :core :refcounted common-lisp:t))


(defgenum
 (open-xrspatial-component-marker-list+marker-type :class
  'open-xrspatial-component-marker-list)
 (:unknown 0) (:qrcode 1) (:micro-qrcode 2) (:aruco 3) (:april-tag 4) (:max 5))

(defgclass
 (open-xrspatial-component-mesh-2dlist :bind "OpenXRSpatialComponentMesh2DList"
  :api :core :refcounted common-lisp:t))

(defgclass
 (open-xrspatial-component-mesh-3dlist :bind "OpenXRSpatialComponentMesh3DList"
  :api :core :refcounted common-lisp:t))

(defgclass
 (open-xrspatial-component-parent-list :bind "OpenXRSpatialComponentParentList"
  :api :core :refcounted common-lisp:t))

(defgclass
 (open-xrspatial-component-persistence-list :bind
  "OpenXRSpatialComponentPersistenceList" :api :core :refcounted common-lisp:t))

(defgclass
 (open-xrspatial-component-plane-alignment-list :bind
  "OpenXRSpatialComponentPlaneAlignmentList" :api :core :refcounted
  common-lisp:t))


(defgenum
 (open-xrspatial-component-plane-alignment-list+plane-alignment :class
  'open-xrspatial-component-plane-alignment-list)
 (:horizontal-upward 0) (:horizontal-downward 1) (:vertical 2) (:arbitrary 3))

(defgclass
 (open-xrspatial-component-plane-semantic-label-list :bind
  "OpenXRSpatialComponentPlaneSemanticLabelList" :api :core :refcounted
  common-lisp:t))


(defgenum
 (open-xrspatial-component-plane-semantic-label-list+plane-semantic-label
  :class 'open-xrspatial-component-plane-semantic-label-list)
 (:uncategorized 1) (:floor 2) (:wall 3) (:ceiling 4) (:table 5))

(defgclass
 (open-xrspatial-component-polygon-2dlist :bind
  "OpenXRSpatialComponentPolygon2DList" :api :core :refcounted common-lisp:t))

(defgclass
 (open-xrspatial-context-persistence-config :bind
  "OpenXRSpatialContextPersistenceConfig" :api :core :refcounted common-lisp:t))

(defgclass
 (open-xrspatial-entity-extension :bind "OpenXRSpatialEntityExtension" :api
  :core)
 (:signals (spatial-discovery-recommended spatial-context rid)))


(defgenum
 (open-xrspatial-entity-extension+capability :class
  'open-xrspatial-entity-extension)
 (:plane-tracking 1000741000) (:marker-tracking-qr-code 1000743000)
 (:marker-tracking-micro-qr-code 1000743001)
 (:marker-tracking-aruco-marker 1000743002)
 (:marker-tracking-april-tag 1000743003) (:anchor 1000762000))


(defgenum
 (open-xrspatial-entity-extension+component-type :class
  'open-xrspatial-entity-extension)
 (:bounded-2d 1) (:bounded-3d 2) (:parent 3) (:mesh-3d 4)
 (:plane-alignment 1000741000) (:mesh-2d 1000741001) (:polygon-2d 1000741002)
 (:plane-semantic-label 1000741003) (:marker 1000743000) (:anchor 1000762000)
 (:persistence 1000763000))

(defgclass
 (open-xrspatial-entity-tracker :bind "OpenXRSpatialEntityTracker" :api :core
  :refcounted common-lisp:t)
 (:signals (next-changed)
  (spatial-tracking-state-changed spatial-tracking-state int)))


(defgenum
 (open-xrspatial-entity-tracker+entity-tracking-state :class
  'open-xrspatial-entity-tracker)
 (:stopped 1) (:paused 2) (:tracking 3))

(defgclass
 (open-xrspatial-marker-tracking-capability :bind
  "OpenXRSpatialMarkerTrackingCapability" :api :core))

(defgclass
 (open-xrspatial-plane-tracking-capability :bind
  "OpenXRSpatialPlaneTrackingCapability" :api :core))

(defgclass
 (open-xrspatial-query-result-data :bind "OpenXRSpatialQueryResultData" :api
  :core :refcounted common-lisp:t))

(defgclass
 (open-xrstructure-base :bind "OpenXRStructureBase" :api :core :refcounted
  common-lisp:t))

(defgclass (open-xrvisibility-mask :bind "OpenXRVisibilityMask" :api :core))

(defgclass
 (optimized-translation :bind "OptimizedTranslation" :api :core :refcounted
  common-lisp:t))

(defgclass (option-button :bind "OptionButton" :api :core)
 (:signals (item-selected index int) (item-focused index int)))

(defgclass (pckpacker :bind "PCKPacker" :api :core :refcounted common-lisp:t))

(defgclass
 (packed-data-container :bind "PackedDataContainer" :api :core :refcounted
  common-lisp:t))

(defgclass
 (packed-data-container-ref :bind "PackedDataContainerRef" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (packed-scene :bind "PackedScene" :api :core :refcounted common-lisp:t))


(defgenum (packed-scene+gen-edit-state :class 'packed-scene) (:disabled 0)
 (:instance 1) (:main 2) (:main-inherited 3))

(defgclass
 (packet-peer :bind "PacketPeer" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))

(defgclass
 (packet-peer-dtls :bind "PacketPeerDTLS" :api :core :refcounted common-lisp:t))


(defgenum (packet-peer-dtls+status :class 'packet-peer-dtls) (:disconnected 0)
 (:handshaking 1) (:connected 2) (:error 3) (:error-hostname-mismatch 4))

(defgclass
 (packet-peer-extension :bind "PacketPeerExtension" :api :core :refcounted
  common-lisp:t))

(defgclass
 (packet-peer-stream :bind "PacketPeerStream" :api :core :refcounted
  common-lisp:t))

(defgclass
 (packet-peer-udp :bind "PacketPeerUDP" :api :core :refcounted common-lisp:t))

(defgclass (panel :bind "Panel" :api :core))

(defgclass (panel-container :bind "PanelContainer" :api :core))

(defgclass
 (panorama-sky-material :bind "PanoramaSkyMaterial" :api :core :refcounted
  common-lisp:t))

(defgclass (parallax-2d :bind "Parallax2D" :api :core))

(defgclass (parallax-background :bind "ParallaxBackground" :api :core))

(defgclass (parallax-layer :bind "ParallaxLayer" :api :core))

(defgclass
 (particle-process-material :bind "ParticleProcessMaterial" :api :core
  :refcounted common-lisp:t)
 (:signals (emission-shape-changed)))


(defgenum
 (particle-process-material+parameter :class 'particle-process-material)
 (:initial-linear-velocity 0) (:angular-velocity 1) (:orbit-velocity 2)
 (:linear-accel 3) (:radial-accel 4) (:tangential-accel 5) (:damping 6)
 (:angle 7) (:scale 8) (:hue-variation 9) (:anim-speed 10) (:anim-offset 11)
 (:radial-velocity 15) (:directional-velocity 16) (:scale-over-velocity 17)
 (:max 18) (:turb-vel-influence 13) (:turb-init-displacement 14)
 (:turb-influence-over-life 12))


(defgenum
 (particle-process-material+particle-flags :class 'particle-process-material)
 (:align-y-to-velocity 0) (:rotate-y 1) (:disable-z 2) (:damping-as-friction 3)
 (:inherit-emitter-scale 4) (:max 5))


(defgenum
 (particle-process-material+emission-shape :class 'particle-process-material)
 (:point 0) (:sphere 1) (:sphere-surface 2) (:box 3) (:points 4)
 (:directed-points 5) (:ring 6) (:max 7))


(defgenum
 (particle-process-material+sub-emitter-mode :class 'particle-process-material)
 (:disabled 0) (:constant 1) (:at-end 2) (:at-collision 3) (:at-start 4)
 (:max 5))


(defgenum
 (particle-process-material+collision-mode :class 'particle-process-material)
 (:disabled 0) (:rigid 1) (:hide-on-contact 2) (:max 3))

(defgclass (path-2d :bind "Path2D" :api :core))

(defgclass (path-3d :bind "Path3D" :api :core)
 (:signals (curve-changed) (debug-color-changed)))

(defgclass (path-follow-2d :bind "PathFollow2D" :api :core))

(defgclass (path-follow-3d :bind "PathFollow3D" :api :core))


(defgenum (path-follow-3d+rotation-mode :class 'path-follow-3d) (:none 0)
 (:y 1) (:xy 2) (:xyz 3) (:oriented 4))

(defgclass (performance :bind "Performance" :api :core))


(defgenum (performance+monitor :class 'performance) (:time-fps 0)
 (:time-process 1) (:time-physics-process 2) (:time-navigation-process 3)
 (:memory-static 4) (:memory-static-max 5) (:memory-message-buffer-max 6)
 (:object-count 7) (:object-resource-count 8) (:object-node-count 9)
 (:object-orphan-node-count 10) (:render-total-objects-in-frame 11)
 (:render-total-primitives-in-frame 12) (:render-total-draw-calls-in-frame 13)
 (:render-video-mem-used 14) (:render-texture-mem-used 15)
 (:render-buffer-mem-used 16) (:physics-2d-active-objects 17)
 (:physics-2d-collision-pairs 18) (:physics-2d-island-count 19)
 (:physics-3d-active-objects 20) (:physics-3d-collision-pairs 21)
 (:physics-3d-island-count 22) (:audio-output-latency 23)
 (:navigation-active-maps 24) (:navigation-region-count 25)
 (:navigation-agent-count 26) (:navigation-link-count 27)
 (:navigation-polygon-count 28) (:navigation-edge-count 29)
 (:navigation-edge-merge-count 30) (:navigation-edge-connection-count 31)
 (:navigation-edge-free-count 32) (:navigation-obstacle-count 33)
 (:pipeline-compilations-canvas 34) (:pipeline-compilations-mesh 35)
 (:pipeline-compilations-surface 36) (:pipeline-compilations-draw 37)
 (:pipeline-compilations-specialization 38) (:navigation-2d-active-maps 39)
 (:navigation-2d-region-count 40) (:navigation-2d-agent-count 41)
 (:navigation-2d-link-count 42) (:navigation-2d-polygon-count 43)
 (:navigation-2d-edge-count 44) (:navigation-2d-edge-merge-count 45)
 (:navigation-2d-edge-connection-count 46) (:navigation-2d-edge-free-count 47)
 (:navigation-2d-obstacle-count 48) (:navigation-3d-active-maps 49)
 (:navigation-3d-region-count 50) (:navigation-3d-agent-count 51)
 (:navigation-3d-link-count 52) (:navigation-3d-polygon-count 53)
 (:navigation-3d-edge-count 54) (:navigation-3d-edge-merge-count 55)
 (:navigation-3d-edge-connection-count 56) (:navigation-3d-edge-free-count 57)
 (:navigation-3d-obstacle-count 58) (:monitor-max 59))


(defgenum (performance+monitor-type :class 'performance) (:quantity 0)
 (:memory 1) (:time 2) (:percentage 3))

(defgclass (physical-bone-2d :bind "PhysicalBone2D" :api :core))

(defgclass (physical-bone-3d :bind "PhysicalBone3D" :api :core))


(defgenum (physical-bone-3d+damp-mode :class 'physical-bone-3d) (:combine 0)
 (:replace 1))


(defgenum (physical-bone-3d+joint-type :class 'physical-bone-3d) (:none 0)
 (:pin 1) (:cone 2) (:hinge 3) (:slider 4) (:6dof 5))

(defgclass
 (physical-bone-simulator-3d :bind "PhysicalBoneSimulator3D" :api :core))

(defgclass
 (physical-sky-material :bind "PhysicalSkyMaterial" :api :core :refcounted
  common-lisp:t))

(defgclass
 (physics-body-2d :bind "PhysicsBody2D" :api :core :instantiable
  common-lisp:nil))

(defgclass
 (physics-body-3d :bind "PhysicsBody3D" :api :core :instantiable
  common-lisp:nil))

(defgclass
 (physics-direct-body-state-2d :bind "PhysicsDirectBodyState2D" :api :core
  :instantiable common-lisp:nil))

(defgclass
 (physics-direct-body-state-2dextension :bind
  "PhysicsDirectBodyState2DExtension" :api :core))

(defgclass
 (physics-direct-body-state-3d :bind "PhysicsDirectBodyState3D" :api :core
  :instantiable common-lisp:nil))

(defgclass
 (physics-direct-body-state-3dextension :bind
  "PhysicsDirectBodyState3DExtension" :api :core))

(defgclass
 (physics-direct-space-state-2d :bind "PhysicsDirectSpaceState2D" :api :core
  :instantiable common-lisp:nil))

(defgclass
 (physics-direct-space-state-2dextension :bind
  "PhysicsDirectSpaceState2DExtension" :api :core))

(defgclass
 (physics-direct-space-state-3d :bind "PhysicsDirectSpaceState3D" :api :core
  :instantiable common-lisp:nil))

(defgclass
 (physics-direct-space-state-3dextension :bind
  "PhysicsDirectSpaceState3DExtension" :api :core))

(defgclass
 (physics-material :bind "PhysicsMaterial" :api :core :refcounted
  common-lisp:t))

(defgclass
 (physics-point-query-parameters-2d :bind "PhysicsPointQueryParameters2D" :api
  :core :refcounted common-lisp:t))

(defgclass
 (physics-point-query-parameters-3d :bind "PhysicsPointQueryParameters3D" :api
  :core :refcounted common-lisp:t))

(defgclass
 (physics-ray-query-parameters-2d :bind "PhysicsRayQueryParameters2D" :api
  :core :refcounted common-lisp:t))

(defgclass
 (physics-ray-query-parameters-3d :bind "PhysicsRayQueryParameters3D" :api
  :core :refcounted common-lisp:t))

(defgclass
 (physics-server-2d :bind "PhysicsServer2D" :api :core :instantiable
  common-lisp:nil))


(defgenum (physics-server-2d+space-parameter :class 'physics-server-2d)
 (:contact-recycle-radius 0) (:contact-max-separation 1)
 (:contact-max-allowed-penetration 2) (:contact-default-bias 3)
 (:body-linear-velocity-sleep-threshold 4)
 (:body-angular-velocity-sleep-threshold 5) (:body-time-to-sleep 6)
 (:constraint-default-bias 7) (:solver-iterations 8))


(defgenum (physics-server-2d+shape-type :class 'physics-server-2d)
 (:world-boundary 0) (:separation-ray 1) (:segment 2) (:circle 3)
 (:rectangle 4) (:capsule 5) (:convex-polygon 6) (:concave-polygon 7)
 (:custom 8))


(defgenum (physics-server-2d+area-parameter :class 'physics-server-2d)
 (:gravity-override-mode 0) (:gravity 1) (:gravity-vector 2)
 (:gravity-is-point 3) (:gravity-point-unit-distance 4)
 (:linear-damp-override-mode 5) (:linear-damp 6)
 (:angular-damp-override-mode 7) (:angular-damp 8) (:priority 9))


(defgenum
 (physics-server-2d+area-space-override-mode :class 'physics-server-2d)
 (:disabled 0) (:combine 1) (:combine-replace 2) (:replace 3)
 (:replace-combine 4))


(defgenum (physics-server-2d+body-mode :class 'physics-server-2d) (:static 0)
 (:kinematic 1) (:rigid 2) (:rigid-linear 3))


(defgenum (physics-server-2d+body-parameter :class 'physics-server-2d)
 (:bounce 0) (:friction 1) (:mass 2) (:inertia 3) (:center-of-mass 4)
 (:gravity-scale 5) (:linear-damp-mode 6) (:angular-damp-mode 7)
 (:linear-damp 8) (:angular-damp 9) (:max 10))


(defgenum (physics-server-2d+body-damp-mode :class 'physics-server-2d)
 (:combine 0) (:replace 1))


(defgenum (physics-server-2d+body-state :class 'physics-server-2d)
 (:transform 0) (:linear-velocity 1) (:angular-velocity 2) (:sleeping 3)
 (:can-sleep 4))


(defgenum (physics-server-2d+joint-type :class 'physics-server-2d) (:pin 0)
 (:groove 1) (:damped-spring 2) (:max 3))


(defgenum (physics-server-2d+joint-param :class 'physics-server-2d) (:bias 0)
 (:max-bias 1) (:max-force 2))


(defgenum (physics-server-2d+pin-joint-param :class 'physics-server-2d)
 (:softness 0) (:limit-upper 1) (:limit-lower 2) (:motor-target-velocity 3))


(defgenum (physics-server-2d+pin-joint-flag :class 'physics-server-2d)
 (:angular-limit-enabled 0) (:motor-enabled 1))


(defgenum (physics-server-2d+damped-spring-param :class 'physics-server-2d)
 (:rest-length 0) (:stiffness 1) (:damping 2))


(defgenum (physics-server-2d+ccdmode :class 'physics-server-2d) (:disabled 0)
 (:cast-ray 1) (:cast-shape 2))


(defgenum (physics-server-2d+area-body-status :class 'physics-server-2d)
 (:added 0) (:removed 1))


(defgenum (physics-server-2d+process-info :class 'physics-server-2d)
 (:active-objects 0) (:collision-pairs 1) (:island-count 2))

(defgclass
 (physics-server-2dextension :bind "PhysicsServer2DExtension" :api :core))

(defgclass (physics-server-2dmanager :bind "PhysicsServer2DManager" :api :core))

(defgclass
 (physics-server-3d :bind "PhysicsServer3D" :api :core :instantiable
  common-lisp:nil))


(defgenum (physics-server-3d+joint-type :class 'physics-server-3d) (:pin 0)
 (:hinge 1) (:slider 2) (:cone-twist 3) (:6dof 4) (:max 5))


(defgenum (physics-server-3d+pin-joint-param :class 'physics-server-3d)
 (:bias 0) (:damping 1) (:impulse-clamp 2))


(defgenum (physics-server-3d+hinge-joint-param :class 'physics-server-3d)
 (:bias 0) (:limit-upper 1) (:limit-lower 2) (:limit-bias 3)
 (:limit-softness 4) (:limit-relaxation 5) (:motor-target-velocity 6)
 (:motor-max-impulse 7))


(defgenum (physics-server-3d+hinge-joint-flag :class 'physics-server-3d)
 (:use-limit 0) (:enable-motor 1))


(defgenum (physics-server-3d+slider-joint-param :class 'physics-server-3d)
 (:linear-limit-upper 0) (:linear-limit-lower 1) (:linear-limit-softness 2)
 (:linear-limit-restitution 3) (:linear-limit-damping 4)
 (:linear-motion-softness 5) (:linear-motion-restitution 6)
 (:linear-motion-damping 7) (:linear-orthogonal-softness 8)
 (:linear-orthogonal-restitution 9) (:linear-orthogonal-damping 10)
 (:angular-limit-upper 11) (:angular-limit-lower 12)
 (:angular-limit-softness 13) (:angular-limit-restitution 14)
 (:angular-limit-damping 15) (:angular-motion-softness 16)
 (:angular-motion-restitution 17) (:angular-motion-damping 18)
 (:angular-orthogonal-softness 19) (:angular-orthogonal-restitution 20)
 (:angular-orthogonal-damping 21) (:max 22))


(defgenum (physics-server-3d+cone-twist-joint-param :class 'physics-server-3d)
 (:swing-span 0) (:twist-span 1) (:bias 2) (:softness 3) (:relaxation 4))


(defgenum (physics-server-3d+g6dofjoint-axis-param :class 'physics-server-3d)
 (:linear-lower-limit 0) (:linear-upper-limit 1) (:linear-limit-softness 2)
 (:linear-restitution 3) (:linear-damping 4) (:linear-motor-target-velocity 5)
 (:linear-motor-force-limit 6) (:linear-spring-stiffness 7)
 (:linear-spring-damping 8) (:linear-spring-equilibrium-point 9)
 (:angular-lower-limit 10) (:angular-upper-limit 11)
 (:angular-limit-softness 12) (:angular-damping 13) (:angular-restitution 14)
 (:angular-force-limit 15) (:angular-erp 16)
 (:angular-motor-target-velocity 17) (:angular-motor-force-limit 18)
 (:angular-spring-stiffness 19) (:angular-spring-damping 20)
 (:angular-spring-equilibrium-point 21) (:max 22))


(defgenum (physics-server-3d+g6dofjoint-axis-flag :class 'physics-server-3d)
 (:enable-linear-limit 0) (:enable-angular-limit 1) (:enable-angular-spring 2)
 (:enable-linear-spring 3) (:enable-motor 4) (:enable-linear-motor 5) (:max 6))


(defgenum (physics-server-3d+shape-type :class 'physics-server-3d)
 (:world-boundary 0) (:separation-ray 1) (:sphere 2) (:box 3) (:capsule 4)
 (:cylinder 5) (:convex-polygon 6) (:concave-polygon 7) (:heightmap 8)
 (:soft-body 9) (:custom 10))


(defgenum (physics-server-3d+area-parameter :class 'physics-server-3d)
 (:gravity-override-mode 0) (:gravity 1) (:gravity-vector 2)
 (:gravity-is-point 3) (:gravity-point-unit-distance 4)
 (:linear-damp-override-mode 5) (:linear-damp 6)
 (:angular-damp-override-mode 7) (:angular-damp 8) (:priority 9)
 (:wind-force-magnitude 10) (:wind-source 11) (:wind-direction 12)
 (:wind-attenuation-factor 13))


(defgenum
 (physics-server-3d+area-space-override-mode :class 'physics-server-3d)
 (:disabled 0) (:combine 1) (:combine-replace 2) (:replace 3)
 (:replace-combine 4))


(defgenum (physics-server-3d+body-mode :class 'physics-server-3d) (:static 0)
 (:kinematic 1) (:rigid 2) (:rigid-linear 3))


(defgenum (physics-server-3d+body-parameter :class 'physics-server-3d)
 (:bounce 0) (:friction 1) (:mass 2) (:inertia 3) (:center-of-mass 4)
 (:gravity-scale 5) (:linear-damp-mode 6) (:angular-damp-mode 7)
 (:linear-damp 8) (:angular-damp 9) (:max 10))


(defgenum (physics-server-3d+body-damp-mode :class 'physics-server-3d)
 (:combine 0) (:replace 1))


(defgenum (physics-server-3d+body-state :class 'physics-server-3d)
 (:transform 0) (:linear-velocity 1) (:angular-velocity 2) (:sleeping 3)
 (:can-sleep 4))


(defgenum (physics-server-3d+area-body-status :class 'physics-server-3d)
 (:added 0) (:removed 1))


(defgenum (physics-server-3d+process-info :class 'physics-server-3d)
 (:active-objects 0) (:collision-pairs 1) (:island-count 2))


(defgenum (physics-server-3d+space-parameter :class 'physics-server-3d)
 (:contact-recycle-radius 0) (:contact-max-separation 1)
 (:contact-max-allowed-penetration 2) (:contact-default-bias 3)
 (:body-linear-velocity-sleep-threshold 4)
 (:body-angular-velocity-sleep-threshold 5) (:body-time-to-sleep 6)
 (:solver-iterations 7))


(defgenum (physics-server-3d+body-axis :class 'physics-server-3d) (:linear-x 1)
 (:linear-y 2) (:linear-z 4) (:angular-x 8) (:angular-y 16) (:angular-z 32))

(defgclass
 (physics-server-3dextension :bind "PhysicsServer3DExtension" :api :core))

(defgclass (physics-server-3dmanager :bind "PhysicsServer3DManager" :api :core))

(defgclass
 (physics-server-3drendering-server-handler :bind
  "PhysicsServer3DRenderingServerHandler" :api :core))

(defgclass
 (physics-shape-query-parameters-2d :bind "PhysicsShapeQueryParameters2D" :api
  :core :refcounted common-lisp:t))

(defgclass
 (physics-shape-query-parameters-3d :bind "PhysicsShapeQueryParameters3D" :api
  :core :refcounted common-lisp:t))

(defgclass
 (physics-test-motion-parameters-2d :bind "PhysicsTestMotionParameters2D" :api
  :core :refcounted common-lisp:t))

(defgclass
 (physics-test-motion-parameters-3d :bind "PhysicsTestMotionParameters3D" :api
  :core :refcounted common-lisp:t))

(defgclass
 (physics-test-motion-result-2d :bind "PhysicsTestMotionResult2D" :api :core
  :refcounted common-lisp:t))

(defgclass
 (physics-test-motion-result-3d :bind "PhysicsTestMotionResult3D" :api :core
  :refcounted common-lisp:t))

(defgclass (pin-joint-2d :bind "PinJoint2D" :api :core))

(defgclass (pin-joint-3d :bind "PinJoint3D" :api :core))


(defgenum (pin-joint-3d+param :class 'pin-joint-3d) (:bias 0) (:damping 1)
 (:impulse-clamp 2))

(defgclass
 (placeholder-cubemap :bind "PlaceholderCubemap" :api :core :refcounted
  common-lisp:t))

(defgclass
 (placeholder-cubemap-array :bind "PlaceholderCubemapArray" :api :core
  :refcounted common-lisp:t))

(defgclass
 (placeholder-material :bind "PlaceholderMaterial" :api :core :refcounted
  common-lisp:t))

(defgclass
 (placeholder-mesh :bind "PlaceholderMesh" :api :core :refcounted
  common-lisp:t))

(defgclass
 (placeholder-texture-2d :bind "PlaceholderTexture2D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (placeholder-texture-2darray :bind "PlaceholderTexture2DArray" :api :core
  :refcounted common-lisp:t))

(defgclass
 (placeholder-texture-3d :bind "PlaceholderTexture3D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (placeholder-texture-layered :bind "PlaceholderTextureLayered" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass (plane-mesh :bind "PlaneMesh" :api :core :refcounted common-lisp:t))


(defgenum (plane-mesh+orientation :class 'plane-mesh) (:x 0) (:y 1) (:z 2))

(defgclass (point-light-2d :bind "PointLight2D" :api :core))

(defgclass (point-mesh :bind "PointMesh" :api :core :refcounted common-lisp:t))

(defgclass (polygon-2d :bind "Polygon2D" :api :core))

(defgclass
 (polygon-occluder-3d :bind "PolygonOccluder3D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (polygon-path-finder :bind "PolygonPathFinder" :api :core :refcounted
  common-lisp:t))

(defgclass (popup :bind "Popup" :api :core) (:signals (popup-hide)))

(defgclass (popup-menu :bind "PopupMenu" :api :core)
 (:signals (id-pressed id int) (id-focused id int) (index-pressed index int)
  (menu-changed)))

(defgclass (popup-panel :bind "PopupPanel" :api :core))

(defgclass
 (portable-compressed-texture-2d :bind "PortableCompressedTexture2D" :api :core
  :refcounted common-lisp:t))


(defgenum
 (portable-compressed-texture-2d+compression-mode :class
  'portable-compressed-texture-2d)
 (:lossless 0) (:lossy 1) (:basis-universal 2) (:s3tc 3) (:etc2 4) (:bptc 5)
 (:astc 6))

(defgclass
 (primitive-mesh :bind "PrimitiveMesh" :api :core :refcounted common-lisp:t))

(defgclass (prism-mesh :bind "PrismMesh" :api :core :refcounted common-lisp:t))

(defgclass
 (procedural-sky-material :bind "ProceduralSkyMaterial" :api :core :refcounted
  common-lisp:t))

(defgclass (progress-bar :bind "ProgressBar" :api :core))


(defgenum (progress-bar+fill-mode :class 'progress-bar) (:begin-to-end 0)
 (:end-to-begin 1) (:top-to-bottom 2) (:bottom-to-top 3))

(defgclass (project-settings :bind "ProjectSettings" :api :core)
 (:signals (settings-changed)))

(defgclass
 (property-tweener :bind "PropertyTweener" :api :core :refcounted
  common-lisp:t))

(defgclass (quad-mesh :bind "QuadMesh" :api :core :refcounted common-lisp:t))

(defgclass
 (quad-occluder-3d :bind "QuadOccluder3D" :api :core :refcounted common-lisp:t))

(defgclass
 (rdacceleration-structure-geometry :bind "RDAccelerationStructureGeometry"
  :api :core :refcounted common-lisp:t))

(defgclass
 (rdacceleration-structure-instance :bind "RDAccelerationStructureInstance"
  :api :core :refcounted common-lisp:t))

(defgclass
 (rdattachment-format :bind "RDAttachmentFormat" :api :core :refcounted
  common-lisp:t))

(defgclass
 (rdframebuffer-pass :bind "RDFramebufferPass" :api :core :refcounted
  common-lisp:t))


(defgconstant +rdframebuffer-pass+attachment-unused+ :value -1 :bind
 "ATTACHMENT_UNUSED" :class 'rdframebuffer-pass)

(defgclass
 (rdhit-group :bind "RDHitGroup" :api :core :refcounted common-lisp:t))

(defgclass
 (rdpipeline-color-blend-state :bind "RDPipelineColorBlendState" :api :core
  :refcounted common-lisp:t))

(defgclass
 (rdpipeline-color-blend-state-attachment :bind
  "RDPipelineColorBlendStateAttachment" :api :core :refcounted common-lisp:t))

(defgclass
 (rdpipeline-depth-stencil-state :bind "RDPipelineDepthStencilState" :api :core
  :refcounted common-lisp:t))

(defgclass
 (rdpipeline-multisample-state :bind "RDPipelineMultisampleState" :api :core
  :refcounted common-lisp:t))

(defgclass
 (rdpipeline-rasterization-state :bind "RDPipelineRasterizationState" :api
  :core :refcounted common-lisp:t))

(defgclass
 (rdpipeline-shader :bind "RDPipelineShader" :api :core :refcounted
  common-lisp:t))

(defgclass
 (rdpipeline-specialization-constant :bind "RDPipelineSpecializationConstant"
  :api :core :refcounted common-lisp:t))

(defgclass
 (rdsampler-state :bind "RDSamplerState" :api :core :refcounted common-lisp:t))

(defgclass
 (rdshader-file :bind "RDShaderFile" :api :core :refcounted common-lisp:t))

(defgclass
 (rdshader-spirv :bind "RDShaderSPIRV" :api :core :refcounted common-lisp:t))

(defgclass
 (rdshader-source :bind "RDShaderSource" :api :core :refcounted common-lisp:t))

(defgclass
 (rdtexture-format :bind "RDTextureFormat" :api :core :refcounted
  common-lisp:t))

(defgclass
 (rdtexture-view :bind "RDTextureView" :api :core :refcounted common-lisp:t))

(defgclass (rduniform :bind "RDUniform" :api :core :refcounted common-lisp:t))

(defgclass
 (rdvertex-attribute :bind "RDVertexAttribute" :api :core :refcounted
  common-lisp:t))

(defgclass
 (random-number-generator :bind "RandomNumberGenerator" :api :core :refcounted
  common-lisp:t))

(defgclass (range :bind "Range" :api :core)
 (:signals (value-changed value float) (changed)))

(defgclass (ray-cast-2d :bind "RayCast2D" :api :core))

(defgclass (ray-cast-3d :bind "RayCast3D" :api :core))

(defgclass
 (rectangle-shape-2d :bind "RectangleShape2D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (ref-counted :bind "RefCounted" :api :core :refcounted common-lisp:t))

(defgclass (reference-rect :bind "ReferenceRect" :api :core))

(defgclass (reflection-probe :bind "ReflectionProbe" :api :core))


(defgenum (reflection-probe+update-mode :class 'reflection-probe) (:once 0)
 (:always 1))


(defgenum (reflection-probe+ambient-mode :class 'reflection-probe)
 (:disabled 0) (:environment 1) (:color 2))

(defgclass (reg-ex :bind "RegEx" :api :core :refcounted common-lisp:t))

(defgclass
 (reg-ex-match :bind "RegExMatch" :api :core :refcounted common-lisp:t))

(defgclass (remote-transform-2d :bind "RemoteTransform2D" :api :core))

(defgclass (remote-transform-3d :bind "RemoteTransform3D" :api :core))

(defgclass
 (render-data :bind "RenderData" :api :core :instantiable common-lisp:nil))

(defgclass (render-data-extension :bind "RenderDataExtension" :api :core))

(defgclass (render-data-rd :bind "RenderDataRD" :api :core))

(defgclass
 (render-scene-buffers :bind "RenderSceneBuffers" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (render-scene-buffers-configuration :bind "RenderSceneBuffersConfiguration"
  :api :core :refcounted common-lisp:t))

(defgclass
 (render-scene-buffers-extension :bind "RenderSceneBuffersExtension" :api :core
  :refcounted common-lisp:t))

(defgclass
 (render-scene-buffers-rd :bind "RenderSceneBuffersRD" :api :core :refcounted
  common-lisp:t))

(defgclass
 (render-scene-data :bind "RenderSceneData" :api :core :instantiable
  common-lisp:nil))

(defgclass
 (render-scene-data-extension :bind "RenderSceneDataExtension" :api :core))

(defgclass (render-scene-data-rd :bind "RenderSceneDataRD" :api :core))

(defgclass
 (rendering-device :bind "RenderingDevice" :api :core :instantiable
  common-lisp:nil))


(defgconstant +rendering-device+invalid-id+ :value -1 :bind "INVALID_ID" :class
 'rendering-device)


(defgconstant +rendering-device+invalid-format-id+ :value -1 :bind
 "INVALID_FORMAT_ID" :class 'rendering-device)


(defgenum (rendering-device+device-type :class 'rendering-device) (:other 0)
 (:integrated-gpu 1) (:discrete-gpu 2) (:virtual-gpu 3) (:cpu 4) (:max 5))


(defgenum (rendering-device+driver-resource :class 'rendering-device)
 (:logical-device 0) (:physical-device 1) (:topmost-object 2)
 (:command-queue 3) (:queue-family 4) (:texture 5) (:texture-view 6)
 (:texture-data-format 7) (:sampler 8) (:uniform-set 9) (:buffer 10)
 (:compute-pipeline 11) (:render-pipeline 12) (:vulkan-device 0)
 (:vulkan-physical-device 1) (:vulkan-instance 2) (:vulkan-queue 3)
 (:vulkan-queue-family-index 4) (:vulkan-image 5) (:vulkan-image-view 6)
 (:vulkan-image-native-texture-format 7) (:vulkan-sampler 8)
 (:vulkan-descriptor-set 9) (:vulkan-buffer 10) (:vulkan-compute-pipeline 11)
 (:vulkan-render-pipeline 12))


(defgenum (rendering-device+data-format :class 'rendering-device)
 (:r4g4-unorm-pack8 0) (:r4g4b4a4-unorm-pack16 1) (:b4g4r4a4-unorm-pack16 2)
 (:r5g6b5-unorm-pack16 3) (:b5g6r5-unorm-pack16 4) (:r5g5b5a1-unorm-pack16 5)
 (:b5g5r5a1-unorm-pack16 6) (:a1r5g5b5-unorm-pack16 7) (:r8-unorm 8)
 (:r8-snorm 9) (:r8-uscaled 10) (:r8-sscaled 11) (:r8-uint 12) (:r8-sint 13)
 (:r8-srgb 14) (:r8g8-unorm 15) (:r8g8-snorm 16) (:r8g8-uscaled 17)
 (:r8g8-sscaled 18) (:r8g8-uint 19) (:r8g8-sint 20) (:r8g8-srgb 21)
 (:r8g8b8-unorm 22) (:r8g8b8-snorm 23) (:r8g8b8-uscaled 24)
 (:r8g8b8-sscaled 25) (:r8g8b8-uint 26) (:r8g8b8-sint 27) (:r8g8b8-srgb 28)
 (:b8g8r8-unorm 29) (:b8g8r8-snorm 30) (:b8g8r8-uscaled 31)
 (:b8g8r8-sscaled 32) (:b8g8r8-uint 33) (:b8g8r8-sint 34) (:b8g8r8-srgb 35)
 (:r8g8b8a8-unorm 36) (:r8g8b8a8-snorm 37) (:r8g8b8a8-uscaled 38)
 (:r8g8b8a8-sscaled 39) (:r8g8b8a8-uint 40) (:r8g8b8a8-sint 41)
 (:r8g8b8a8-srgb 42) (:b8g8r8a8-unorm 43) (:b8g8r8a8-snorm 44)
 (:b8g8r8a8-uscaled 45) (:b8g8r8a8-sscaled 46) (:b8g8r8a8-uint 47)
 (:b8g8r8a8-sint 48) (:b8g8r8a8-srgb 49) (:a8b8g8r8-unorm-pack32 50)
 (:a8b8g8r8-snorm-pack32 51) (:a8b8g8r8-uscaled-pack32 52)
 (:a8b8g8r8-sscaled-pack32 53) (:a8b8g8r8-uint-pack32 54)
 (:a8b8g8r8-sint-pack32 55) (:a8b8g8r8-srgb-pack32 56)
 (:a2r10g10b10-unorm-pack32 57) (:a2r10g10b10-snorm-pack32 58)
 (:a2r10g10b10-uscaled-pack32 59) (:a2r10g10b10-sscaled-pack32 60)
 (:a2r10g10b10-uint-pack32 61) (:a2r10g10b10-sint-pack32 62)
 (:a2b10g10r10-unorm-pack32 63) (:a2b10g10r10-snorm-pack32 64)
 (:a2b10g10r10-uscaled-pack32 65) (:a2b10g10r10-sscaled-pack32 66)
 (:a2b10g10r10-uint-pack32 67) (:a2b10g10r10-sint-pack32 68) (:r16-unorm 69)
 (:r16-snorm 70) (:r16-uscaled 71) (:r16-sscaled 72) (:r16-uint 73)
 (:r16-sint 74) (:r16-sfloat 75) (:r16g16-unorm 76) (:r16g16-snorm 77)
 (:r16g16-uscaled 78) (:r16g16-sscaled 79) (:r16g16-uint 80) (:r16g16-sint 81)
 (:r16g16-sfloat 82) (:r16g16b16-unorm 83) (:r16g16b16-snorm 84)
 (:r16g16b16-uscaled 85) (:r16g16b16-sscaled 86) (:r16g16b16-uint 87)
 (:r16g16b16-sint 88) (:r16g16b16-sfloat 89) (:r16g16b16a16-unorm 90)
 (:r16g16b16a16-snorm 91) (:r16g16b16a16-uscaled 92) (:r16g16b16a16-sscaled 93)
 (:r16g16b16a16-uint 94) (:r16g16b16a16-sint 95) (:r16g16b16a16-sfloat 96)
 (:r32-uint 97) (:r32-sint 98) (:r32-sfloat 99) (:r32g32-uint 100)
 (:r32g32-sint 101) (:r32g32-sfloat 102) (:r32g32b32-uint 103)
 (:r32g32b32-sint 104) (:r32g32b32-sfloat 105) (:r32g32b32a32-uint 106)
 (:r32g32b32a32-sint 107) (:r32g32b32a32-sfloat 108) (:r64-uint 109)
 (:r64-sint 110) (:r64-sfloat 111) (:r64g64-uint 112) (:r64g64-sint 113)
 (:r64g64-sfloat 114) (:r64g64b64-uint 115) (:r64g64b64-sint 116)
 (:r64g64b64-sfloat 117) (:r64g64b64a64-uint 118) (:r64g64b64a64-sint 119)
 (:r64g64b64a64-sfloat 120) (:b10g11r11-ufloat-pack32 121)
 (:e5b9g9r9-ufloat-pack32 122) (:d16-unorm 123) (:x8-d24-unorm-pack32 124)
 (:d32-sfloat 125) (:s8-uint 126) (:d16-unorm-s8-uint 127)
 (:d24-unorm-s8-uint 128) (:d32-sfloat-s8-uint 129) (:bc1-rgb-unorm-block 130)
 (:bc1-rgb-srgb-block 131) (:bc1-rgba-unorm-block 132)
 (:bc1-rgba-srgb-block 133) (:bc2-unorm-block 134) (:bc2-srgb-block 135)
 (:bc3-unorm-block 136) (:bc3-srgb-block 137) (:bc4-unorm-block 138)
 (:bc4-snorm-block 139) (:bc5-unorm-block 140) (:bc5-snorm-block 141)
 (:bc6h-ufloat-block 142) (:bc6h-sfloat-block 143) (:bc7-unorm-block 144)
 (:bc7-srgb-block 145) (:etc2-r8g8b8-unorm-block 146)
 (:etc2-r8g8b8-srgb-block 147) (:etc2-r8g8b8a1-unorm-block 148)
 (:etc2-r8g8b8a1-srgb-block 149) (:etc2-r8g8b8a8-unorm-block 150)
 (:etc2-r8g8b8a8-srgb-block 151) (:eac-r11-unorm-block 152)
 (:eac-r11-snorm-block 153) (:eac-r11g11-unorm-block 154)
 (:eac-r11g11-snorm-block 155) (:astc-4x4-unorm-block 156)
 (:astc-4x4-srgb-block 157) (:astc-5x4-unorm-block 158)
 (:astc-5x4-srgb-block 159) (:astc-5x5-unorm-block 160)
 (:astc-5x5-srgb-block 161) (:astc-6x5-unorm-block 162)
 (:astc-6x5-srgb-block 163) (:astc-6x6-unorm-block 164)
 (:astc-6x6-srgb-block 165) (:astc-8x5-unorm-block 166)
 (:astc-8x5-srgb-block 167) (:astc-8x6-unorm-block 168)
 (:astc-8x6-srgb-block 169) (:astc-8x8-unorm-block 170)
 (:astc-8x8-srgb-block 171) (:astc-10x5-unorm-block 172)
 (:astc-10x5-srgb-block 173) (:astc-10x6-unorm-block 174)
 (:astc-10x6-srgb-block 175) (:astc-10x8-unorm-block 176)
 (:astc-10x8-srgb-block 177) (:astc-10x10-unorm-block 178)
 (:astc-10x10-srgb-block 179) (:astc-12x10-unorm-block 180)
 (:astc-12x10-srgb-block 181) (:astc-12x12-unorm-block 182)
 (:astc-12x12-srgb-block 183) (:g8b8g8r8-422-unorm 184)
 (:b8g8r8g8-422-unorm 185) (:g8-b8-r8-3plane-420-unorm 186)
 (:g8-b8r8-2plane-420-unorm 187) (:g8-b8-r8-3plane-422-unorm 188)
 (:g8-b8r8-2plane-422-unorm 189) (:g8-b8-r8-3plane-444-unorm 190)
 (:r10x6-unorm-pack16 191) (:r10x6g10x6-unorm-2pack16 192)
 (:r10x6g10x6b10x6a10x6-unorm-4pack16 193)
 (:g10x6b10x6g10x6r10x6-422-unorm-4pack16 194)
 (:b10x6g10x6r10x6g10x6-422-unorm-4pack16 195)
 (:g10x6-b10x6-r10x6-3plane-420-unorm-3pack16 196)
 (:g10x6-b10x6r10x6-2plane-420-unorm-3pack16 197)
 (:g10x6-b10x6-r10x6-3plane-422-unorm-3pack16 198)
 (:g10x6-b10x6r10x6-2plane-422-unorm-3pack16 199)
 (:g10x6-b10x6-r10x6-3plane-444-unorm-3pack16 200) (:r12x4-unorm-pack16 201)
 (:r12x4g12x4-unorm-2pack16 202) (:r12x4g12x4b12x4a12x4-unorm-4pack16 203)
 (:g12x4b12x4g12x4r12x4-422-unorm-4pack16 204)
 (:b12x4g12x4r12x4g12x4-422-unorm-4pack16 205)
 (:g12x4-b12x4-r12x4-3plane-420-unorm-3pack16 206)
 (:g12x4-b12x4r12x4-2plane-420-unorm-3pack16 207)
 (:g12x4-b12x4-r12x4-3plane-422-unorm-3pack16 208)
 (:g12x4-b12x4r12x4-2plane-422-unorm-3pack16 209)
 (:g12x4-b12x4-r12x4-3plane-444-unorm-3pack16 210)
 (:g16b16g16r16-422-unorm 211) (:b16g16r16g16-422-unorm 212)
 (:g16-b16-r16-3plane-420-unorm 213) (:g16-b16r16-2plane-420-unorm 214)
 (:g16-b16-r16-3plane-422-unorm 215) (:g16-b16r16-2plane-422-unorm 216)
 (:g16-b16-r16-3plane-444-unorm 217) (:astc-4x4-sfloat-block 218)
 (:astc-5x4-sfloat-block 219) (:astc-5x5-sfloat-block 220)
 (:astc-6x5-sfloat-block 221) (:astc-6x6-sfloat-block 222)
 (:astc-8x5-sfloat-block 223) (:astc-8x6-sfloat-block 224)
 (:astc-8x8-sfloat-block 225) (:astc-10x5-sfloat-block 226)
 (:astc-10x6-sfloat-block 227) (:astc-10x8-sfloat-block 228)
 (:astc-10x10-sfloat-block 229) (:astc-12x10-sfloat-block 230)
 (:astc-12x12-sfloat-block 231) (:max 232))


(defgenum
 (rendering-device+barrier-mask :bitfield common-lisp:t :class
  'rendering-device)
 (:vertex 1) (:fragment 8) (:compute 2) (:transfer 4) (:raster 9)
 (:all-barriers 32767) (:no-barrier 32768))


(defgenum (rendering-device+texture-type :class 'rendering-device) (:|1D| 0)
 (:|2D| 1) (:|3D| 2) (:cube 3) (:1d-array 4) (:2d-array 5) (:cube-array 6)
 (:max 7))


(defgenum (rendering-device+texture-samples :class 'rendering-device) (:|1| 0)
 (:|2| 1) (:|4| 2) (:|8| 3) (:|16| 4) (:|32| 5) (:|64| 6) (:max 7))


(defgenum
 (rendering-device+texture-usage-bits :bitfield common-lisp:t :class
  'rendering-device)
 (:sampling-bit 1) (:color-attachment-bit 2) (:depth-stencil-attachment-bit 4)
 (:depth-resolve-attachment-bit 4096) (:storage-bit 8) (:storage-atomic-bit 16)
 (:cpu-read-bit 32) (:can-update-bit 64) (:can-copy-from-bit 128)
 (:can-copy-to-bit 256) (:input-attachment-bit 512))


(defgenum (rendering-device+texture-swizzle :class 'rendering-device)
 (:identity 0) (:zero 1) (:one 2) (:r 3) (:g 4) (:b 5) (:a 6) (:max 7))


(defgenum (rendering-device+texture-slice-type :class 'rendering-device)
 (:|2D| 0) (:cubemap 1) (:|3D| 2))


(defgenum (rendering-device+sampler-filter :class 'rendering-device)
 (:nearest 0) (:linear 1))


(defgenum (rendering-device+sampler-repeat-mode :class 'rendering-device)
 (:repeat 0) (:mirrored-repeat 1) (:clamp-to-edge 2) (:clamp-to-border 3)
 (:mirror-clamp-to-edge 4) (:max 5))


(defgenum (rendering-device+sampler-border-color :class 'rendering-device)
 (:float-transparent-black 0) (:int-transparent-black 1)
 (:float-opaque-black 2) (:int-opaque-black 3) (:float-opaque-white 4)
 (:int-opaque-white 5) (:max 6))


(defgenum (rendering-device+vertex-frequency :class 'rendering-device)
 (:vertex 0) (:instance 1))


(defgenum (rendering-device+index-buffer-format :class 'rendering-device)
 (:uint16 0) (:uint32 1))


(defgenum
 (rendering-device+storage-buffer-usage :bitfield common-lisp:t :class
  'rendering-device)
 (:storage-buffer-usage-dispatch-indirect 1))


(defgenum
 (rendering-device+buffer-creation-bits :bitfield common-lisp:t :class
  'rendering-device)
 (:device-address-bit 1) (:as-storage-bit 2)
 (:acceleration-structure-build-input-read-only-bit 8))


(defgenum
 (rendering-device+acceleration-structure-flag-bits :bitfield common-lisp:t
  :class 'rendering-device)
 (:allow-update-bit 1) (:allow-compaction-bit 2) (:prefer-fast-trace-bit 4)
 (:prefer-fast-build-bit 8) (:low-memory-bit 16))


(defgenum
 (rendering-device+acceleration-structure-geometry-flag-bits :bitfield
  common-lisp:t :class 'rendering-device)
 (:opaque-bit 1) (:no-duplicate-any-hit-invocation-bit 2))


(defgenum
 (rendering-device+acceleration-structure-instance-flag-bits :bitfield
  common-lisp:t :class 'rendering-device)
 (:triangle-facing-cull-disable-bit 1) (:triangle-flip-facing-bit 2)
 (:force-opaque-bit 4) (:force-no-opaque-bit 8))


(defgenum (rendering-device+uniform-type :class 'rendering-device) (:sampler 0)
 (:sampler-with-texture 1) (:texture 2) (:image 3) (:texture-buffer 4)
 (:sampler-with-texture-buffer 5) (:image-buffer 6) (:uniform-buffer 7)
 (:storage-buffer 8) (:input-attachment 9) (:uniform-buffer-dynamic 10)
 (:storage-buffer-dynamic 11) (:acceleration-structure 12) (:max 13))


(defgenum (rendering-device+render-primitive :class 'rendering-device)
 (:points 0) (:lines 1) (:lines-with-adjacency 2) (:linestrips 3)
 (:linestrips-with-adjacency 4) (:triangles 5) (:triangles-with-adjacency 6)
 (:triangle-strips 7) (:triangle-strips-with-ajacency 8)
 (:triangle-strips-with-restart-index 9) (:tesselation-patch 10) (:max 11))


(defgenum (rendering-device+polygon-cull-mode :class 'rendering-device)
 (:disabled 0) (:front 1) (:back 2))


(defgenum (rendering-device+polygon-front-face :class 'rendering-device)
 (:clockwise 0) (:counter-clockwise 1))


(defgenum (rendering-device+stencil-operation :class 'rendering-device)
 (:keep 0) (:zero 1) (:replace 2) (:increment-and-clamp 3)
 (:decrement-and-clamp 4) (:invert 5) (:increment-and-wrap 6)
 (:decrement-and-wrap 7) (:max 8))


(defgenum (rendering-device+compare-operator :class 'rendering-device)
 (:never 0) (:less 1) (:equal 2) (:less-or-equal 3) (:greater 4) (:not-equal 5)
 (:greater-or-equal 6) (:always 7) (:max 8))


(defgenum (rendering-device+logic-operation :class 'rendering-device)
 (:clear 0) (:and 1) (:and-reverse 2) (:copy 3) (:and-inverted 4) (:no-op 5)
 (:xor 6) (:or 7) (:nor 8) (:equivalent 9) (:invert 10) (:or-reverse 11)
 (:copy-inverted 12) (:or-inverted 13) (:nand 14) (:set 15) (:max 16))


(defgenum (rendering-device+blend-factor :class 'rendering-device) (:zero 0)
 (:one 1) (:src-color 2) (:one-minus-src-color 3) (:dst-color 4)
 (:one-minus-dst-color 5) (:src-alpha 6) (:one-minus-src-alpha 7)
 (:dst-alpha 8) (:one-minus-dst-alpha 9) (:constant-color 10)
 (:one-minus-constant-color 11) (:constant-alpha 12)
 (:one-minus-constant-alpha 13) (:src-alpha-saturate 14) (:src1-color 15)
 (:one-minus-src1-color 16) (:src1-alpha 17) (:one-minus-src1-alpha 18)
 (:max 19))


(defgenum (rendering-device+blend-operation :class 'rendering-device) (:add 0)
 (:subtract 1) (:reverse-subtract 2) (:minimum 3) (:maximum 4) (:max 5))


(defgenum
 (rendering-device+pipeline-dynamic-state-flags :bitfield common-lisp:t :class
  'rendering-device)
 (:line-width 1) (:depth-bias 2) (:blend-constants 4) (:depth-bounds 8)
 (:stencil-compare-mask 16) (:stencil-write-mask 32) (:stencil-reference 64))


(defgenum (rendering-device+initial-action :class 'rendering-device) (:load 0)
 (:clear 1) (:discard 2) (:max 3) (:clear-region 1) (:clear-region-continue 1)
 (:keep 0) (:drop 2) (:continue 0))


(defgenum (rendering-device+final-action :class 'rendering-device) (:store 0)
 (:discard 1) (:max 2) (:read 0) (:continue 0))


(defgenum (rendering-device+shader-stage :class 'rendering-device) (:vertex 0)
 (:fragment 1) (:tesselation-control 2) (:tesselation-evaluation 3)
 (:compute 4) (:raygen 5) (:any-hit 6) (:closest-hit 7) (:miss 8)
 (:intersection 9) (:max 10) (:vertex-bit 1) (:fragment-bit 2)
 (:tesselation-control-bit 4) (:tesselation-evaluation-bit 8) (:compute-bit 16)
 (:raygen-bit 32) (:any-hit-bit 64) (:closest-hit-bit 128) (:miss-bit 256)
 (:intersection-bit 512))


(defgenum (rendering-device+shader-language :class 'rendering-device) (:glsl 0)
 (:hlsl 1))


(defgenum
 (rendering-device+pipeline-specialization-constant-type :class
  'rendering-device)
 (:bool 0) (:int 1) (:float 2))


(defgenum (rendering-device+features :class 'rendering-device)
 (:metalfx-spatial 3) (:metalfx-temporal 4) (:buffer-device-address 6)
 (:image-atomic-32-bit 7) (:ray-query 11) (:raytracing-pipeline 12)
 (:hdr-output 13))


(defgenum (rendering-device+limit :class 'rendering-device)
 (:max-bound-uniform-sets 0) (:max-framebuffer-color-attachments 1)
 (:max-textures-per-uniform-set 2) (:max-samplers-per-uniform-set 3)
 (:max-storage-buffers-per-uniform-set 4)
 (:max-storage-images-per-uniform-set 5)
 (:max-uniform-buffers-per-uniform-set 6) (:max-draw-indexed-index 7)
 (:max-framebuffer-height 8) (:max-framebuffer-width 9)
 (:max-texture-array-layers 10) (:max-texture-size-1d 11)
 (:max-texture-size-2d 12) (:max-texture-size-3d 13)
 (:max-texture-size-cube 14) (:max-textures-per-shader-stage 15)
 (:max-samplers-per-shader-stage 16) (:max-storage-buffers-per-shader-stage 17)
 (:max-storage-images-per-shader-stage 18)
 (:max-uniform-buffers-per-shader-stage 19) (:max-push-constant-size 20)
 (:max-uniform-buffer-size 21) (:max-vertex-input-attribute-offset 22)
 (:max-vertex-input-attributes 23) (:max-vertex-input-bindings 24)
 (:max-vertex-input-binding-stride 25)
 (:min-uniform-buffer-offset-alignment 26) (:max-compute-shared-memory-size 27)
 (:max-compute-workgroup-count-x 28) (:max-compute-workgroup-count-y 29)
 (:max-compute-workgroup-count-z 30) (:max-compute-workgroup-invocations 31)
 (:max-compute-workgroup-size-x 32) (:max-compute-workgroup-size-y 33)
 (:max-compute-workgroup-size-z 34) (:max-viewport-dimensions-x 35)
 (:max-viewport-dimensions-y 36) (:metalfx-temporal-scaler-min-scale 46)
 (:metalfx-temporal-scaler-max-scale 47))


(defgenum (rendering-device+memory-type :class 'rendering-device) (:textures 0)
 (:buffers 1) (:total 2))


(defgenum (rendering-device+breadcrumb-marker :class 'rendering-device)
 (:none 0) (:reflection-probes 65536) (:sky-pass 131072)
 (:lightmapper-pass 196608) (:shadow-pass-directional 262144)
 (:shadow-pass-cube 327680) (:opaque-pass 393216) (:alpha-pass 458752)
 (:transparent-pass 524288) (:post-processing-pass 589824) (:blit-pass 655360)
 (:ui-pass 720896) (:debug-pass 786432))


(defgenum
 (rendering-device+draw-flags :bitfield common-lisp:t :class 'rendering-device)
 (:default-all 0) (:clear-color-0 1) (:clear-color-1 2) (:clear-color-2 4)
 (:clear-color-3 8) (:clear-color-4 16) (:clear-color-5 32) (:clear-color-6 64)
 (:clear-color-7 128) (:clear-color-mask 255) (:clear-color-all 255)
 (:ignore-color-0 256) (:ignore-color-1 512) (:ignore-color-2 1024)
 (:ignore-color-3 2048) (:ignore-color-4 4096) (:ignore-color-5 8192)
 (:ignore-color-6 16384) (:ignore-color-7 32768) (:ignore-color-mask 65280)
 (:ignore-color-all 65280) (:clear-depth 65536) (:ignore-depth 131072)
 (:clear-stencil 262144) (:ignore-stencil 524288) (:clear-all 327935)
 (:ignore-all 720640))

(defgclass
 (rendering-server :bind "RenderingServer" :api :core :instantiable
  common-lisp:nil)
 (:signals (frame-pre-draw) (frame-post-draw)))


(defgconstant +rendering-server+no-index-array+ :value -1 :bind
 "NO_INDEX_ARRAY" :class 'rendering-server)


(defgconstant +rendering-server+array-weights-size+ :value 4 :bind
 "ARRAY_WEIGHTS_SIZE" :class 'rendering-server)


(defgconstant +rendering-server+canvas-item-z-min+ :value -4096 :bind
 "CANVAS_ITEM_Z_MIN" :class 'rendering-server)


(defgconstant +rendering-server+canvas-item-z-max+ :value 4096 :bind
 "CANVAS_ITEM_Z_MAX" :class 'rendering-server)


(defgconstant +rendering-server+canvas-layer-min+ :value -2147483648 :bind
 "CANVAS_LAYER_MIN" :class 'rendering-server)


(defgconstant +rendering-server+canvas-layer-max+ :value 2147483647 :bind
 "CANVAS_LAYER_MAX" :class 'rendering-server)


(defgconstant +rendering-server+max-glow-levels+ :value 7 :bind
 "MAX_GLOW_LEVELS" :class 'rendering-server)


(defgconstant +rendering-server+max-cursors+ :value 8 :bind "MAX_CURSORS"
 :class 'rendering-server)


(defgconstant +rendering-server+max-2d-directional-lights+ :value 8 :bind
 "MAX_2D_DIRECTIONAL_LIGHTS" :class 'rendering-server)


(defgconstant +rendering-server+max-mesh-surfaces+ :value 256 :bind
 "MAX_MESH_SURFACES" :class 'rendering-server)


(defgconstant +rendering-server+material-render-priority-min+ :value -128 :bind
 "MATERIAL_RENDER_PRIORITY_MIN" :class 'rendering-server)


(defgconstant +rendering-server+material-render-priority-max+ :value 127 :bind
 "MATERIAL_RENDER_PRIORITY_MAX" :class 'rendering-server)


(defgconstant +rendering-server+array-custom-count+ :value 4 :bind
 "ARRAY_CUSTOM_COUNT" :class 'rendering-server)


(defgconstant +rendering-server+particles-emit-flag-position+ :value 1 :bind
 "PARTICLES_EMIT_FLAG_POSITION" :class 'rendering-server)


(defgconstant +rendering-server+particles-emit-flag-rotation-scale+ :value 2
 :bind "PARTICLES_EMIT_FLAG_ROTATION_SCALE" :class 'rendering-server)


(defgconstant +rendering-server+particles-emit-flag-velocity+ :value 4 :bind
 "PARTICLES_EMIT_FLAG_VELOCITY" :class 'rendering-server)


(defgconstant +rendering-server+particles-emit-flag-color+ :value 8 :bind
 "PARTICLES_EMIT_FLAG_COLOR" :class 'rendering-server)


(defgconstant +rendering-server+particles-emit-flag-custom+ :value 16 :bind
 "PARTICLES_EMIT_FLAG_CUSTOM" :class 'rendering-server)


(defgenum (rendering-server+texture-type :class 'rendering-server) (:|2D| 0)
 (:layered 1) (:|3D| 2))


(defgenum (rendering-server+texture-layered-type :class 'rendering-server)
 (:2d-array 0) (:cubemap 1) (:cubemap-array 2))


(defgenum (rendering-server+cube-map-layer :class 'rendering-server) (:left 0)
 (:right 1) (:bottom 2) (:top 3) (:front 4) (:back 5))


(defgenum (rendering-server+texture-drawable-format :class 'rendering-server)
 (:rgba8 0) (:rgba8-srgb 1) (:rgbah 2) (:rgbaf 3))


(defgenum (rendering-server+shader-mode :class 'rendering-server) (:spatial 0)
 (:canvas-item 1) (:particles 2) (:sky 3) (:fog 4) (:texture-blit 5) (:max 6))


(defgenum (rendering-server+array-type :class 'rendering-server) (:vertex 0)
 (:normal 1) (:tangent 2) (:color 3) (:tex-uv 4) (:tex-uv2 5) (:custom0 6)
 (:custom1 7) (:custom2 8) (:custom3 9) (:bones 10) (:weights 11) (:index 12)
 (:max 13))


(defgenum (rendering-server+array-custom-format :class 'rendering-server)
 (:rgba8-unorm 0) (:rgba8-snorm 1) (:rg-half 2) (:rgba-half 3) (:r-float 4)
 (:rg-float 5) (:rgb-float 6) (:rgba-float 7) (:max 8))


(defgenum
 (rendering-server+array-format :bitfield common-lisp:t :class
  'rendering-server)
 (:format-vertex 1) (:format-normal 2) (:format-tangent 4) (:format-color 8)
 (:format-tex-uv 16) (:format-tex-uv2 32) (:format-custom0 64)
 (:format-custom1 128) (:format-custom2 256) (:format-custom3 512)
 (:format-bones 1024) (:format-weights 2048) (:format-index 4096)
 (:format-blend-shape-mask 7) (:format-custom-base 13) (:format-custom-bits 3)
 (:format-custom0-shift 13) (:format-custom1-shift 16)
 (:format-custom2-shift 19) (:format-custom3-shift 22) (:format-custom-mask 7)
 (:compress-flags-base 25) (:flag-use-2d-vertices 33554432)
 (:flag-use-dynamic-update 67108864) (:flag-use-8-bone-weights 134217728)
 (:flag-uses-empty-vertex-array 268435456)
 (:flag-compress-attributes 536870912) (:flag-format-version-base 35)
 (:flag-format-version-shift 35) (:flag-format-version-1 0)
 (:flag-format-version-2 34359738368)
 (:flag-format-current-version 34359738368) (:flag-format-version-mask 255))


(defgenum (rendering-server+primitive-type :class 'rendering-server)
 (:points 0) (:lines 1) (:line-strip 2) (:triangles 3) (:triangle-strip 4)
 (:max 5))


(defgenum (rendering-server+blend-shape-mode :class 'rendering-server)
 (:normalized 0) (:relative 1))


(defgenum
 (rendering-server+multimesh-transform-format :class 'rendering-server)
 (:|2D| 0) (:|3D| 1))


(defgenum
 (rendering-server+multimesh-physics-interpolation-quality :class
  'rendering-server)
 (:fast 0) (:high 1))


(defgenum (rendering-server+light-projector-filter :class 'rendering-server)
 (:nearest 0) (:linear 1) (:nearest-mipmaps 2) (:linear-mipmaps 3)
 (:nearest-mipmaps-anisotropic 4) (:linear-mipmaps-anisotropic 5))


(defgenum (rendering-server+light-type :class 'rendering-server)
 (:directional 0) (:omni 1) (:spot 2) (:area 3))


(defgenum (rendering-server+light-param :class 'rendering-server) (:energy 0)
 (:indirect-energy 1) (:volumetric-fog-energy 2) (:specular 3) (:range 4)
 (:size 5) (:attenuation 6) (:spot-angle 7) (:spot-attenuation 8)
 (:shadow-max-distance 9) (:shadow-split-1-offset 10)
 (:shadow-split-2-offset 11) (:shadow-split-3-offset 12)
 (:shadow-fade-start 13) (:shadow-normal-bias 14) (:shadow-bias 15)
 (:shadow-pancake-size 16) (:shadow-opacity 17) (:shadow-blur 18)
 (:transmittance-bias 19) (:intensity 20) (:max 21))


(defgenum (rendering-server+light-bake-mode :class 'rendering-server)
 (:disabled 0) (:static 1) (:dynamic 2))


(defgenum (rendering-server+light-omni-shadow-mode :class 'rendering-server)
 (:dual-paraboloid 0) (:cube 1))


(defgenum
 (rendering-server+light-directional-shadow-mode :class 'rendering-server)
 (:orthogonal 0) (:parallel-2-splits 1) (:parallel-4-splits 2))


(defgenum
 (rendering-server+light-directional-sky-mode :class 'rendering-server)
 (:light-and-sky 0) (:light-only 1) (:sky-only 2))


(defgenum (rendering-server+shadow-quality :class 'rendering-server) (:hard 0)
 (:soft-very-low 1) (:soft-low 2) (:soft-medium 3) (:soft-high 4)
 (:soft-ultra 5) (:max 6))


(defgenum
 (rendering-server+reflection-probe-update-mode :class 'rendering-server)
 (:once 0) (:always 1))


(defgenum
 (rendering-server+reflection-probe-ambient-mode :class 'rendering-server)
 (:disabled 0) (:environment 1) (:color 2))


(defgenum (rendering-server+decal-texture :class 'rendering-server) (:albedo 0)
 (:normal 1) (:orm 2) (:emission 3) (:max 4))


(defgenum (rendering-server+decal-filter :class 'rendering-server) (:nearest 0)
 (:linear 1) (:nearest-mipmaps 2) (:linear-mipmaps 3)
 (:nearest-mipmaps-anisotropic 4) (:linear-mipmaps-anisotropic 5))


(defgenum (rendering-server+voxel-giquality :class 'rendering-server) (:low 0)
 (:high 1))


(defgenum (rendering-server+particles-mode :class 'rendering-server) (:|2D| 0)
 (:|3D| 1))


(defgenum (rendering-server+particles-transform-align :class 'rendering-server)
 (:disabled 0) (:z-billboard 1) (:y-to-velocity 2)
 (:z-billboard-y-to-velocity 3) (:local-billboard 4))


(defgenum
 (rendering-server+particles-transform-align-custom-src :class
  'rendering-server)
 (:disabled 0) (:x 1) (:y 2) (:z 3) (:w 4))


(defgenum
 (rendering-server+particles-transform-align-axis :class 'rendering-server)
 (:x 0) (:y 1))


(defgenum (rendering-server+particles-draw-order :class 'rendering-server)
 (:index 0) (:lifetime 1) (:reverse-lifetime 2) (:view-depth 3))


(defgenum (rendering-server+particles-collision-type :class 'rendering-server)
 (:sphere-attract 0) (:box-attract 1) (:vector-field-attract 2)
 (:sphere-collide 3) (:box-collide 4) (:sdf-collide 5) (:heightfield-collide 6))


(defgenum
 (rendering-server+particles-collision-heightfield-resolution :class
  'rendering-server)
 (:|256| 0) (:|512| 1) (:|1024| 2) (:|2048| 3) (:|4096| 4) (:|8192| 5) (:max 6))


(defgenum (rendering-server+fog-volume-shape :class 'rendering-server)
 (:ellipsoid 0) (:cone 1) (:cylinder 2) (:box 3) (:world 4) (:max 5))


(defgenum (rendering-server+viewport-scaling-3dmode :class 'rendering-server)
 (:bilinear 0) (:fsr 1) (:fsr2 2) (:metalfx-spatial 3) (:metalfx-temporal 4)
 (:nearest 5) (:max 6))


(defgenum (rendering-server+viewport-update-mode :class 'rendering-server)
 (:disabled 0) (:once 1) (:when-visible 2) (:when-parent-visible 3) (:always 4))


(defgenum (rendering-server+viewport-clear-mode :class 'rendering-server)
 (:always 0) (:never 1) (:only-next-frame 2))


(defgenum (rendering-server+viewport-environment-mode :class 'rendering-server)
 (:disabled 0) (:enabled 1) (:inherit 2) (:max 3))


(defgenum (rendering-server+viewport-sdfoversize :class 'rendering-server)
 (:100-percent 0) (:120-percent 1) (:150-percent 2) (:200-percent 3) (:max 4))


(defgenum (rendering-server+viewport-sdfscale :class 'rendering-server)
 (:100-percent 0) (:50-percent 1) (:25-percent 2) (:max 3))


(defgenum (rendering-server+viewport-msaa :class 'rendering-server)
 (:disabled 0) (:|2X| 1) (:|4X| 2) (:|8X| 3) (:max 4))


(defgenum
 (rendering-server+viewport-anisotropic-filtering :class 'rendering-server)
 (:disabled 0) (:|2X| 1) (:|4X| 2) (:|8X| 3) (:|16X| 4) (:max 5))


(defgenum (rendering-server+viewport-screen-space-aa :class 'rendering-server)
 (:disabled 0) (:fxaa 1) (:smaa 2) (:max 3))


(defgenum
 (rendering-server+viewport-occlusion-culling-build-quality :class
  'rendering-server)
 (:low 0) (:medium 1) (:high 2))


(defgenum (rendering-server+viewport-render-info :class 'rendering-server)
 (:objects-in-frame 0) (:primitives-in-frame 1) (:draw-calls-in-frame 2)
 (:max 3))


(defgenum (rendering-server+viewport-render-info-type :class 'rendering-server)
 (:visible 0) (:shadow 1) (:canvas 2) (:max 3))


(defgenum (rendering-server+viewport-debug-draw :class 'rendering-server)
 (:disabled 0) (:unshaded 1) (:lighting 2) (:overdraw 3) (:wireframe 4)
 (:normal-buffer 5) (:voxel-gi-albedo 6) (:voxel-gi-lighting 7)
 (:voxel-gi-emission 8) (:shadow-atlas 9) (:directional-shadow-atlas 10)
 (:scene-luminance 11) (:ssao 12) (:ssil 13) (:pssm-splits 14)
 (:decal-atlas 15) (:sdfgi 16) (:sdfgi-probes 17) (:gi-buffer 18)
 (:disable-lod 19) (:cluster-omni-lights 20) (:cluster-spot-lights 21)
 (:cluster-decals 22) (:cluster-reflection-probes 23) (:occluders 24)
 (:motion-vectors 25) (:internal-buffer 26))


(defgenum (rendering-server+viewport-vrsmode :class 'rendering-server)
 (:disabled 0) (:texture 1) (:xr 2) (:max 3))


(defgenum (rendering-server+viewport-vrsupdate-mode :class 'rendering-server)
 (:disabled 0) (:once 1) (:always 2) (:max 3))


(defgenum (rendering-server+sky-mode :class 'rendering-server) (:automatic 0)
 (:quality 1) (:incremental 2) (:realtime 3))


(defgenum (rendering-server+compositor-effect-flags :class 'rendering-server)
 (:access-resolved-color 1) (:access-resolved-depth 2)
 (:needs-motion-vectors 4) (:needs-roughness 8) (:needs-separate-specular 16))


(defgenum
 (rendering-server+compositor-effect-callback-type :class 'rendering-server)
 (:pre-opaque 0) (:post-opaque 1) (:post-sky 2) (:pre-transparent 3)
 (:post-transparent 4) (:any -1))


(defgenum (rendering-server+environment-bg :class 'rendering-server)
 (:clear-color 0) (:color 1) (:sky 2) (:canvas 3) (:keep 4) (:camera-feed 5)
 (:max 6))


(defgenum
 (rendering-server+environment-ambient-source :class 'rendering-server) (:bg 0)
 (:disabled 1) (:color 2) (:sky 3))


(defgenum
 (rendering-server+environment-reflection-source :class 'rendering-server)
 (:bg 0) (:disabled 1) (:sky 2))


(defgenum
 (rendering-server+environment-glow-blend-mode :class 'rendering-server)
 (:additive 0) (:screen 1) (:softlight 2) (:replace 3) (:mix 4))


(defgenum (rendering-server+environment-fog-mode :class 'rendering-server)
 (:exponential 0) (:depth 1))


(defgenum (rendering-server+environment-tone-mapper :class 'rendering-server)
 (:linear 0) (:reinhard 1) (:filmic 2) (:aces 3) (:agx 4))


(defgenum
 (rendering-server+environment-ssrroughness-quality :class 'rendering-server)
 (:disabled 0) (:low 1) (:medium 2) (:high 3))


(defgenum (rendering-server+environment-ssaoquality :class 'rendering-server)
 (:very-low 0) (:low 1) (:medium 2) (:high 3) (:ultra 4))


(defgenum (rendering-server+environment-ssilquality :class 'rendering-server)
 (:very-low 0) (:low 1) (:medium 2) (:high 3) (:ultra 4))


(defgenum (rendering-server+environment-sdfgiyscale :class 'rendering-server)
 (:50-percent 0) (:75-percent 1) (:100-percent 2))


(defgenum
 (rendering-server+environment-sdfgiray-count :class 'rendering-server)
 (:|4| 0) (:|8| 1) (:|16| 2) (:|32| 3) (:|64| 4) (:|96| 5) (:|128| 6) (:max 7))


(defgenum
 (rendering-server+environment-sdfgiframes-to-converge :class
  'rendering-server)
 (:in-5-frames 0) (:in-10-frames 1) (:in-15-frames 2) (:in-20-frames 3)
 (:in-25-frames 4) (:in-30-frames 5) (:max 6))


(defgenum
 (rendering-server+environment-sdfgiframes-to-update-light :class
  'rendering-server)
 (:in-1-frame 0) (:in-2-frames 1) (:in-4-frames 2) (:in-8-frames 3)
 (:in-16-frames 4) (:max 5))


(defgenum
 (rendering-server+sub-surface-scattering-quality :class 'rendering-server)
 (:disabled 0) (:low 1) (:medium 2) (:high 3))


(defgenum (rendering-server+dofbokeh-shape :class 'rendering-server) (:box 0)
 (:hexagon 1) (:circle 2))


(defgenum (rendering-server+dofblur-quality :class 'rendering-server)
 (:very-low 0) (:low 1) (:medium 2) (:high 3))


(defgenum (rendering-server+instance-type :class 'rendering-server) (:none 0)
 (:mesh 1) (:multimesh 2) (:particles 3) (:particles-collision 4) (:light 5)
 (:reflection-probe 6) (:decal 7) (:voxel-gi 8) (:lightmap 9) (:occluder 10)
 (:visiblity-notifier 11) (:fog-volume 12) (:max 13) (:geometry-mask 14))


(defgenum (rendering-server+instance-flags :class 'rendering-server)
 (:use-baked-light 0) (:use-dynamic-gi 1) (:draw-next-frame-if-visible 2)
 (:ignore-occlusion-culling 3) (:max 4))


(defgenum (rendering-server+shadow-casting-setting :class 'rendering-server)
 (:off 0) (:on 1) (:double-sided 2) (:shadows-only 3))


(defgenum
 (rendering-server+visibility-range-fade-mode :class 'rendering-server)
 (:disabled 0) (:self 1) (:dependencies 2))


(defgenum (rendering-server+bake-channels :class 'rendering-server)
 (:albedo-alpha 0) (:normal 1) (:orm 2) (:emission 3))


(defgenum (rendering-server+canvas-texture-channel :class 'rendering-server)
 (:diffuse 0) (:normal 1) (:specular 2))


(defgenum (rendering-server+nine-patch-axis-mode :class 'rendering-server)
 (:stretch 0) (:tile 1) (:tile-fit 2))


(defgenum
 (rendering-server+canvas-item-texture-filter :class 'rendering-server)
 (:default 0) (:nearest 1) (:linear 2) (:nearest-with-mipmaps 3)
 (:linear-with-mipmaps 4) (:nearest-with-mipmaps-anisotropic 5)
 (:linear-with-mipmaps-anisotropic 6) (:max 7))


(defgenum
 (rendering-server+canvas-item-texture-repeat :class 'rendering-server)
 (:default 0) (:disabled 1) (:enabled 2) (:mirror 3) (:max 4))


(defgenum (rendering-server+canvas-group-mode :class 'rendering-server)
 (:disabled 0) (:clip-only 1) (:clip-and-draw 2) (:transparent 3))


(defgenum (rendering-server+canvas-light-mode :class 'rendering-server)
 (:point 0) (:directional 1))


(defgenum (rendering-server+canvas-light-blend-mode :class 'rendering-server)
 (:add 0) (:sub 1) (:mix 2))


(defgenum
 (rendering-server+canvas-light-shadow-filter :class 'rendering-server)
 (:none 0) (:pcf5 1) (:pcf13 2) (:max 3))


(defgenum
 (rendering-server+canvas-occluder-polygon-cull-mode :class 'rendering-server)
 (:disabled 0) (:clockwise 1) (:counter-clockwise 2))


(defgenum
 (rendering-server+global-shader-parameter-type :class 'rendering-server)
 (:bool 0) (:bvec2 1) (:bvec3 2) (:bvec4 3) (:int 4) (:ivec2 5) (:ivec3 6)
 (:ivec4 7) (:rect2i 8) (:uint 9) (:uvec2 10) (:uvec3 11) (:uvec4 12)
 (:float 13) (:vec2 14) (:vec3 15) (:vec4 16) (:color 17) (:rect2 18)
 (:mat2 19) (:mat3 20) (:mat4 21) (:transform-2d 22) (:transform 23)
 (:sampler2d 24) (:sampler2darray 25) (:sampler3d 26) (:samplercube 27)
 (:samplerext 28) (:max 29))


(defgenum (rendering-server+rendering-info :class 'rendering-server)
 (:total-objects-in-frame 0) (:total-primitives-in-frame 1)
 (:total-draw-calls-in-frame 2) (:texture-mem-used 3) (:buffer-mem-used 4)
 (:video-mem-used 5) (:pipeline-compilations-canvas 6)
 (:pipeline-compilations-mesh 7) (:pipeline-compilations-surface 8)
 (:pipeline-compilations-draw 9) (:pipeline-compilations-specialization 10))


(defgenum (rendering-server+pipeline-source :class 'rendering-server)
 (:canvas 0) (:mesh 1) (:surface 2) (:draw 3) (:specialization 4) (:max 5))


(defgenum (rendering-server+splash-stretch-mode :class 'rendering-server)
 (:disabled 0) (:keep 1) (:keep-width 2) (:keep-height 3) (:cover 4)
 (:ignore 5))


(defgenum (rendering-server+features :class 'rendering-server) (:shaders 0)
 (:multithreaded 1))

(defgclass (resource :bind "Resource" :api :core :refcounted common-lisp:t)
 (:signals (changed) (setup-local-to-scene-requested)))


(defgenum (resource+deep-duplicate-mode :class 'resource) (:none 0)
 (:internal 1) (:all 2))

(defgclass
 (resource-format-loader :bind "ResourceFormatLoader" :api :core :refcounted
  common-lisp:t))


(defgenum (resource-format-loader+cache-mode :class 'resource-format-loader)
 (:ignore 0) (:reuse 1) (:replace 2) (:ignore-deep 3) (:replace-deep 4))

(defgclass
 (resource-format-saver :bind "ResourceFormatSaver" :api :core :refcounted
  common-lisp:t))

(defgclass
 (resource-importer :bind "ResourceImporter" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))


(defgenum (resource-importer+import-order :class 'resource-importer)
 (:default 0) (:scene 100))

(defgclass
 (resource-importer-bmfont :bind "ResourceImporterBMFont" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (resource-importer-bit-map :bind "ResourceImporterBitMap" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (resource-importer-csvtranslation :bind "ResourceImporterCSVTranslation" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (resource-importer-dynamic-font :bind "ResourceImporterDynamicFont" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (resource-importer-image :bind "ResourceImporterImage" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (resource-importer-image-font :bind "ResourceImporterImageFont" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (resource-importer-layered-texture :bind "ResourceImporterLayeredTexture" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (resource-importer-mp3 :bind "ResourceImporterMP3" :api :editor :refcounted
  common-lisp:t))

(defgclass
 (resource-importer-obj :bind "ResourceImporterOBJ" :api :editor :refcounted
  common-lisp:t))

(defgclass
 (resource-importer-ogg-vorbis :bind "ResourceImporterOggVorbis" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (resource-importer-svg :bind "ResourceImporterSVG" :api :editor :refcounted
  common-lisp:t))

(defgclass
 (resource-importer-scene :bind "ResourceImporterScene" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (resource-importer-shader-file :bind "ResourceImporterShaderFile" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (resource-importer-texture :bind "ResourceImporterTexture" :api :editor
  :refcounted common-lisp:t))

(defgclass
 (resource-importer-texture-atlas :bind "ResourceImporterTextureAtlas" :api
  :editor :refcounted common-lisp:t))

(defgclass
 (resource-importer-wav :bind "ResourceImporterWAV" :api :editor :refcounted
  common-lisp:t))

(defgclass (resource-loader :bind "ResourceLoader" :api :core))


(defgenum (resource-loader+thread-load-status :class 'resource-loader)
 (:invalid-resource 0) (:in-progress 1) (:failed 2) (:loaded 3))


(defgenum (resource-loader+cache-mode :class 'resource-loader) (:ignore 0)
 (:reuse 1) (:replace 2) (:ignore-deep 3) (:replace-deep 4))

(defgclass (resource-preloader :bind "ResourcePreloader" :api :core))

(defgclass (resource-saver :bind "ResourceSaver" :api :core))


(defgenum
 (resource-saver+saver-flags :bitfield common-lisp:t :class 'resource-saver)
 (:none 0) (:relative-paths 1) (:bundle-resources 2) (:change-path 4)
 (:omit-editor-properties 8) (:save-big-endian 16) (:compress 32)
 (:replace-subresource-paths 64))

(defgclass
 (resource-uid :bind "ResourceUID" :api :core :instantiable common-lisp:nil))


(defgconstant +resource-uid+invalid-id+ :value -1 :bind "INVALID_ID" :class
 'resource-uid)

(defgclass (retarget-modifier-3d :bind "RetargetModifier3D" :api :core))


(defgenum
 (retarget-modifier-3d+transform-flag :bitfield common-lisp:t :class
  'retarget-modifier-3d)
 (:position 1) (:rotation 2) (:scale 4) (:all 7))

(defgclass
 (ribbon-trail-mesh :bind "RibbonTrailMesh" :api :core :refcounted
  common-lisp:t))


(defgenum (ribbon-trail-mesh+shape :class 'ribbon-trail-mesh) (:flat 0)
 (:cross 1))

(defgclass
 (rich-text-effect :bind "RichTextEffect" :api :core :refcounted common-lisp:t))

(defgclass (rich-text-label :bind "RichTextLabel" :api :core)
 (:signals (meta-clicked meta variant) (meta-hover-started meta variant)
  (meta-hover-ended meta variant) (finished)))


(defgenum (rich-text-label+list-type :class 'rich-text-label) (:numbers 0)
 (:letters 1) (:roman 2) (:dots 3))


(defgenum (rich-text-label+menu-items :class 'rich-text-label) (:copy 0)
 (:select-all 1) (:max 2))


(defgenum (rich-text-label+meta-underline :class 'rich-text-label) (:never 0)
 (:always 1) (:on-hover 2))


(defgenum
 (rich-text-label+image-update-mask :bitfield common-lisp:t :class
  'rich-text-label)
 (:texture 1) (:size 2) (:color 4) (:alignment 8) (:region 16) (:pad 32)
 (:tooltip 64) (:width-unit 128))


(defgenum (rich-text-label+image-unit :class 'rich-text-label) (:pixel 0)
 (:percent 1) (:em 2))

(defgclass (rigid-body-2d :bind "RigidBody2D" :api :core)
 (:signals
  (body-shape-entered body-rid rid body node body-shape-index int
   local-shape-index int)
  (body-shape-exited body-rid rid body node body-shape-index int
   local-shape-index int)
  (body-entered body node) (body-exited body node) (sleeping-state-changed)))


(defgenum (rigid-body-2d+freeze-mode :class 'rigid-body-2d) (:static 0)
 (:kinematic 1))


(defgenum (rigid-body-2d+center-of-mass-mode :class 'rigid-body-2d) (:auto 0)
 (:custom 1))


(defgenum (rigid-body-2d+damp-mode :class 'rigid-body-2d) (:combine 0)
 (:replace 1))


(defgenum (rigid-body-2d+ccdmode :class 'rigid-body-2d) (:disabled 0)
 (:cast-ray 1) (:cast-shape 2))

(defgclass (rigid-body-3d :bind "RigidBody3D" :api :core)
 (:signals
  (body-shape-entered body-rid rid body node body-shape-index int
   local-shape-index int)
  (body-shape-exited body-rid rid body node body-shape-index int
   local-shape-index int)
  (body-entered body node) (body-exited body node) (sleeping-state-changed)))


(defgenum (rigid-body-3d+freeze-mode :class 'rigid-body-3d) (:static 0)
 (:kinematic 1))


(defgenum (rigid-body-3d+center-of-mass-mode :class 'rigid-body-3d) (:auto 0)
 (:custom 1))


(defgenum (rigid-body-3d+damp-mode :class 'rigid-body-3d) (:combine 0)
 (:replace 1))

(defgclass (root-motion-view :bind "RootMotionView" :api :core))

(defgclass
 (scene-multiplayer :bind "SceneMultiplayer" :api :core :refcounted
  common-lisp:t)
 (:signals (peer-authenticating id int) (peer-authentication-failed id int)
  (peer-packet id int packet packed-byte-array)))

(defgclass
 (scene-replication-config :bind "SceneReplicationConfig" :api :core
  :refcounted common-lisp:t))


(defgenum
 (scene-replication-config+replication-mode :class 'scene-replication-config)
 (:never 0) (:always 1) (:on-change 2))

(defgclass
 (scene-state :bind "SceneState" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))


(defgenum (scene-state+gen-edit-state :class 'scene-state) (:disabled 0)
 (:instance 1) (:main 2) (:main-inherited 3))

(defgclass (scene-tree :bind "SceneTree" :api :core)
 (:signals (tree-changed) (scene-changed) (tree-process-mode-changed)
  (node-added node node) (node-removed node node) (node-renamed node node)
  (node-configuration-warning-changed node node) (process-frame)
  (physics-frame)))


(defgenum (scene-tree+group-call-flags :class 'scene-tree) (:default 0)
 (:reverse 1) (:deferred 2) (:unique 4))

(defgclass
 (scene-tree-timer :bind "SceneTreeTimer" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t)
 (:signals (timeout)))

(defgclass
 (script :bind "Script" :api :core :instantiable common-lisp:nil :refcounted
  common-lisp:t))

(defgclass
 (script-backtrace :bind "ScriptBacktrace" :api :core :refcounted
  common-lisp:t))

(defgclass (script-create-dialog :bind "ScriptCreateDialog" :api :editor)
 (:signals (script-created script script)))

(defgclass
 (script-editor :bind "ScriptEditor" :api :editor :instantiable
  common-lisp:nil)
 (:signals (editor-script-changed script script) (script-close script script)))

(defgclass
 (script-editor-base :bind "ScriptEditorBase" :api :editor :instantiable
  common-lisp:nil)
 (:signals (name-changed) (edited-script-changed)
  (search-in-files-requested text string) (request-save-history)
  (request-help topic string)
  (request-open-script-at-line script object line int) (go-to-help what string)
  (request-save-previous-state state dictionary)
  (replace-in-files-requested text string)
  (go-to-method script object method string)))

(defgclass
 (script-extension :bind "ScriptExtension" :api :core :refcounted
  common-lisp:t))

(defgclass
 (script-language :bind "ScriptLanguage" :api :core :instantiable
  common-lisp:nil))


(defgenum (script-language+script-name-casing :class 'script-language)
 (:auto 0) (:pascal-case 1) (:snake-case 2) (:kebab-case 3) (:camel-case 4))

(defgclass
 (script-language-extension :bind "ScriptLanguageExtension" :api :core))


(defgenum
 (script-language-extension+lookup-result-type :class
  'script-language-extension)
 (:script-location 0) (:class 1) (:class-constant 2) (:class-property 3)
 (:class-method 4) (:class-signal 5) (:class-enum 6) (:class-tbd-globalscope 7)
 (:class-annotation 8) (:local-constant 9) (:local-variable 10) (:max 11))


(defgenum
 (script-language-extension+code-completion-location :class
  'script-language-extension)
 (:local 0) (:parent-mask 256) (:other-user-code 512) (:other 1024))


(defgenum
 (script-language-extension+code-completion-kind :class
  'script-language-extension)
 (:class 0) (:function 1) (:signal 2) (:variable 3) (:member 4) (:enum 5)
 (:constant 6) (:node-path 7) (:file-path 8) (:plain-text 9) (:keyword 10)
 (:max 11))

(defgclass
 (scroll-bar :bind "ScrollBar" :api :core :instantiable common-lisp:nil)
 (:signals (scrolling)))

(defgclass (scroll-container :bind "ScrollContainer" :api :core)
 (:signals (scroll-started) (scroll-ended)))


(defgenum (scroll-container+scroll-mode :class 'scroll-container) (:disabled 0)
 (:auto 1) (:show-always 2) (:show-never 3) (:reserve 4) (:maximize-first 5))


(defgenum (scroll-container+scroll-hint-mode :class 'scroll-container)
 (:disabled 0) (:all 1) (:top-and-left 2) (:bottom-and-right 3))

(defgclass
 (segment-shape-2d :bind "SegmentShape2D" :api :core :refcounted common-lisp:t))

(defgclass (semaphore :bind "Semaphore" :api :core :refcounted common-lisp:t))

(defgclass
 (separation-ray-shape-2d :bind "SeparationRayShape2D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (separation-ray-shape-3d :bind "SeparationRayShape3D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (separator :bind "Separator" :api :core :instantiable common-lisp:nil))

(defgclass (shader :bind "Shader" :api :core :refcounted common-lisp:t))


(defgenum (shader+mode :class 'shader) (:spatial 0) (:canvas-item 1)
 (:particles 2) (:sky 3) (:fog 4) (:texture-blit 5))

(defgclass (shader-globals-override :bind "ShaderGlobalsOverride" :api :core))

(defgclass
 (shader-include :bind "ShaderInclude" :api :core :refcounted common-lisp:t))

(defgclass (shader-include-db :bind "ShaderIncludeDB" :api :core))

(defgclass
 (shader-material :bind "ShaderMaterial" :api :core :refcounted common-lisp:t))

(defgclass
 (shape-2d :bind "Shape2D" :api :core :instantiable common-lisp:nil :refcounted
  common-lisp:t))

(defgclass
 (shape-3d :bind "Shape3D" :api :core :instantiable common-lisp:nil :refcounted
  common-lisp:t))

(defgclass (shape-cast-2d :bind "ShapeCast2D" :api :core))

(defgclass (shape-cast-3d :bind "ShapeCast3D" :api :core))

(defgclass (shortcut :bind "Shortcut" :api :core :refcounted common-lisp:t))

(defgclass (skeleton-2d :bind "Skeleton2D" :api :core)
 (:signals (bone-setup-changed)))

(defgclass (skeleton-3d :bind "Skeleton3D" :api :core)
 (:signals (rest-updated) (pose-updated) (skeleton-updated)
  (bone-enabled-changed bone-idx int) (bone-list-changed)
  (show-rest-only-changed)))


(defgconstant +skeleton-3d+notification-update-skeleton+ :value 50 :bind
 "NOTIFICATION_UPDATE_SKELETON" :class 'skeleton-3d)


(defgenum (skeleton-3d+modifier-callback-mode-process :class 'skeleton-3d)
 (:physics 0) (:idle 1) (:manual 2))

(defgclass (skeleton-ik3d :bind "SkeletonIK3D" :api :core))

(defgclass
 (skeleton-modification-2d :bind "SkeletonModification2D" :api :core
  :refcounted common-lisp:t))

(defgclass
 (skeleton-modification-2dccdik :bind "SkeletonModification2DCCDIK" :api :core
  :refcounted common-lisp:t))

(defgclass
 (skeleton-modification-2dfabrik :bind "SkeletonModification2DFABRIK" :api
  :core :refcounted common-lisp:t))

(defgclass
 (skeleton-modification-2djiggle :bind "SkeletonModification2DJiggle" :api
  :core :refcounted common-lisp:t))

(defgclass
 (skeleton-modification-2dlook-at :bind "SkeletonModification2DLookAt" :api
  :core :refcounted common-lisp:t))

(defgclass
 (skeleton-modification-2dphysical-bones :bind
  "SkeletonModification2DPhysicalBones" :api :core :refcounted common-lisp:t))

(defgclass
 (skeleton-modification-2dstack-holder :bind
  "SkeletonModification2DStackHolder" :api :core :refcounted common-lisp:t))

(defgclass
 (skeleton-modification-2dtwo-bone-ik :bind "SkeletonModification2DTwoBoneIK"
  :api :core :refcounted common-lisp:t))

(defgclass
 (skeleton-modification-stack-2d :bind "SkeletonModificationStack2D" :api :core
  :refcounted common-lisp:t))

(defgclass (skeleton-modifier-3d :bind "SkeletonModifier3D" :api :core)
 (:signals (modification-processed)))


(defgenum (skeleton-modifier-3d+bone-axis :class 'skeleton-modifier-3d)
 (:plus-x 0) (:minus-x 1) (:plus-y 2) (:minus-y 3) (:plus-z 4) (:minus-z 5))


(defgenum (skeleton-modifier-3d+bone-direction :class 'skeleton-modifier-3d)
 (:plus-x 0) (:minus-x 1) (:plus-y 2) (:minus-y 3) (:plus-z 4) (:minus-z 5)
 (:from-parent 6))


(defgenum
 (skeleton-modifier-3d+secondary-direction :class 'skeleton-modifier-3d)
 (:none 0) (:plus-x 1) (:minus-x 2) (:plus-y 3) (:minus-y 4) (:plus-z 5)
 (:minus-z 6) (:custom 7))


(defgenum (skeleton-modifier-3d+rotation-axis :class 'skeleton-modifier-3d)
 (:x 0) (:y 1) (:z 2) (:all 3) (:custom 4))

(defgclass
 (skeleton-profile :bind "SkeletonProfile" :api :core :refcounted
  common-lisp:t)
 (:signals (profile-updated)))


(defgenum (skeleton-profile+tail-direction :class 'skeleton-profile)
 (:average-children 0) (:specific-child 1) (:end 2))

(defgclass
 (skeleton-profile-humanoid :bind "SkeletonProfileHumanoid" :api :core
  :refcounted common-lisp:t))

(defgclass (skin :bind "Skin" :api :core :refcounted common-lisp:t))

(defgclass
 (skin-reference :bind "SkinReference" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))

(defgclass (sky :bind "Sky" :api :core :refcounted common-lisp:t))


(defgenum (sky+radiance-size :class 'sky) (:|32| 0) (:|64| 1) (:|128| 2)
 (:|256| 3) (:|512| 4) (:|1024| 5) (:|2048| 6) (:max 7))


(defgenum (sky+process-mode :class 'sky) (:automatic 0) (:quality 1)
 (:incremental 2) (:realtime 3))

(defgclass (slider :bind "Slider" :api :core :instantiable common-lisp:nil)
 (:signals (drag-started) (drag-ended value-changed bool)))


(defgenum (slider+tick-position :class 'slider) (:bottom-right 0) (:top-left 1)
 (:both 2) (:center 3))

(defgclass (slider-joint-3d :bind "SliderJoint3D" :api :core))


(defgenum (slider-joint-3d+param :class 'slider-joint-3d)
 (:linear-limit-upper 0) (:linear-limit-lower 1) (:linear-limit-softness 2)
 (:linear-limit-restitution 3) (:linear-limit-damping 4)
 (:linear-motion-softness 5) (:linear-motion-restitution 6)
 (:linear-motion-damping 7) (:linear-orthogonal-softness 8)
 (:linear-orthogonal-restitution 9) (:linear-orthogonal-damping 10)
 (:angular-limit-upper 11) (:angular-limit-lower 12)
 (:angular-limit-softness 13) (:angular-limit-restitution 14)
 (:angular-limit-damping 15) (:angular-motion-softness 16)
 (:angular-motion-restitution 17) (:angular-motion-damping 18)
 (:angular-orthogonal-softness 19) (:angular-orthogonal-restitution 20)
 (:angular-orthogonal-damping 21) (:max 22))

(defgclass
 (socket-server :bind "SocketServer" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))

(defgclass (soft-body-3d :bind "SoftBody3D" :api :core))


(defgenum (soft-body-3d+disable-mode :class 'soft-body-3d) (:remove 0)
 (:keep-active 1))

(defgclass
 (sphere-mesh :bind "SphereMesh" :api :core :refcounted common-lisp:t))

(defgclass
 (sphere-occluder-3d :bind "SphereOccluder3D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (sphere-shape-3d :bind "SphereShape3D" :api :core :refcounted common-lisp:t))

(defgclass (spin-box :bind "SpinBox" :api :core))

(defgclass (spline-ik3d :bind "SplineIK3D" :api :core))

(defgclass (split-container :bind "SplitContainer" :api :core)
 (:signals (dragged offset int) (drag-started) (drag-ended)))


(defgenum (split-container+dragger-visibility :class 'split-container)
 (:visible 0) (:hidden 1) (:hidden-collapsed 2))

(defgclass (spot-light-3d :bind "SpotLight3D" :api :core))

(defgclass (spring-arm-3d :bind "SpringArm3D" :api :core))

(defgclass (spring-bone-collision-3d :bind "SpringBoneCollision3D" :api :core))

(defgclass
 (spring-bone-collision-capsule-3d :bind "SpringBoneCollisionCapsule3D" :api
  :core))

(defgclass
 (spring-bone-collision-plane-3d :bind "SpringBoneCollisionPlane3D" :api :core))

(defgclass
 (spring-bone-collision-sphere-3d :bind "SpringBoneCollisionSphere3D" :api
  :core))

(defgclass (spring-bone-simulator-3d :bind "SpringBoneSimulator3D" :api :core))


(defgenum
 (spring-bone-simulator-3d+center-from :class 'spring-bone-simulator-3d)
 (:world-origin 0) (:node 1) (:bone 2))

(defgclass (sprite-2d :bind "Sprite2D" :api :core)
 (:signals (frame-changed) (texture-changed)))

(defgclass (sprite-3d :bind "Sprite3D" :api :core)
 (:signals (frame-changed) (texture-changed)))

(defgclass
 (sprite-base-3d :bind "SpriteBase3D" :api :core :instantiable common-lisp:nil))


(defgenum (sprite-base-3d+draw-flags :class 'sprite-base-3d) (:transparent 0)
 (:shaded 1) (:double-sided 2) (:disable-depth-test 3) (:fixed-size 4) (:max 5))


(defgenum (sprite-base-3d+alpha-cut-mode :class 'sprite-base-3d) (:disabled 0)
 (:discard 1) (:opaque-prepass 2) (:hash 3))

(defgclass
 (sprite-frames :bind "SpriteFrames" :api :core :refcounted common-lisp:t))


(defgenum (sprite-frames+loop-mode :class 'sprite-frames) (:none 0) (:linear 1)
 (:pingpong 2))

(defgclass
 (standard-material-3d :bind "StandardMaterial3D" :api :core :refcounted
  common-lisp:t))

(defgclass (static-body-2d :bind "StaticBody2D" :api :core))

(defgclass (static-body-3d :bind "StaticBody3D" :api :core))

(defgclass (status-indicator :bind "StatusIndicator" :api :core)
 (:signals (pressed mouse-button int mouse-position vector-2i)))

(defgclass
 (stream-peer :bind "StreamPeer" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))

(defgclass
 (stream-peer-buffer :bind "StreamPeerBuffer" :api :core :refcounted
  common-lisp:t))

(defgclass
 (stream-peer-extension :bind "StreamPeerExtension" :api :core :refcounted
  common-lisp:t))

(defgclass
 (stream-peer-gzip :bind "StreamPeerGZIP" :api :core :refcounted common-lisp:t))

(defgclass
 (stream-peer-socket :bind "StreamPeerSocket" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))


(defgenum (stream-peer-socket+status :class 'stream-peer-socket) (:none 0)
 (:connecting 1) (:connected 2) (:error 3))

(defgclass
 (stream-peer-tcp :bind "StreamPeerTCP" :api :core :refcounted common-lisp:t))

(defgclass
 (stream-peer-tls :bind "StreamPeerTLS" :api :core :refcounted common-lisp:t))


(defgenum (stream-peer-tls+status :class 'stream-peer-tls) (:disconnected 0)
 (:handshaking 1) (:connected 2) (:error 3) (:error-hostname-mismatch 4))

(defgclass
 (stream-peer-uds :bind "StreamPeerUDS" :api :core :refcounted common-lisp:t))

(defgclass (style-box :bind "StyleBox" :api :core :refcounted common-lisp:t))

(defgclass
 (style-box-empty :bind "StyleBoxEmpty" :api :core :refcounted common-lisp:t))

(defgclass
 (style-box-flat :bind "StyleBoxFlat" :api :core :refcounted common-lisp:t))

(defgclass
 (style-box-line :bind "StyleBoxLine" :api :core :refcounted common-lisp:t))

(defgclass
 (style-box-texture :bind "StyleBoxTexture" :api :core :refcounted
  common-lisp:t))


(defgenum (style-box-texture+axis-stretch-mode :class 'style-box-texture)
 (:stretch 0) (:tile 1) (:tile-fit 2))

(defgclass (sub-viewport :bind "SubViewport" :api :core))


(defgenum (sub-viewport+clear-mode :class 'sub-viewport) (:always 0) (:never 1)
 (:once 2))


(defgenum (sub-viewport+update-mode :class 'sub-viewport) (:disabled 0)
 (:once 1) (:when-visible 2) (:when-parent-visible 3) (:always 4))

(defgclass (sub-viewport-container :bind "SubViewportContainer" :api :core))

(defgclass
 (subtween-tweener :bind "SubtweenTweener" :api :core :refcounted
  common-lisp:t))

(defgclass
 (surface-tool :bind "SurfaceTool" :api :core :refcounted common-lisp:t))


(defgenum (surface-tool+custom-format :class 'surface-tool) (:rgba8-unorm 0)
 (:rgba8-snorm 1) (:rg-half 2) (:rgba-half 3) (:r-float 4) (:rg-float 5)
 (:rgb-float 6) (:rgba-float 7) (:max 8))


(defgenum (surface-tool+skin-weight-count :class 'surface-tool) (:4-weights 0)
 (:8-weights 1))

(defgclass
 (syntax-highlighter :bind "SyntaxHighlighter" :api :core :refcounted
  common-lisp:t))

(defgclass
 (system-font :bind "SystemFont" :api :core :refcounted common-lisp:t))

(defgclass (tcpserver :bind "TCPServer" :api :core :refcounted common-lisp:t))

(defgclass
 (tlsoptions :bind "TLSOptions" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))

(defgclass (tab-bar :bind "TabBar" :api :core)
 (:signals (tab-selected tab int) (tab-changed tab int) (tab-clicked tab int)
  (tab-rmb-clicked tab int) (tab-close-pressed tab int)
  (tab-button-pressed tab int) (tab-hovered tab int)
  (active-tab-rearranged idx-to int)))


(defgenum (tab-bar+alignment-mode :class 'tab-bar) (:left 0) (:center 1)
 (:right 2) (:max 3))


(defgenum (tab-bar+close-button-display-policy :class 'tab-bar) (:show-never 0)
 (:show-active-only 1) (:show-always 2) (:max 3))

(defgclass (tab-container :bind "TabContainer" :api :core)
 (:signals (active-tab-rearranged idx-to int) (tab-changed tab int)
  (tab-clicked tab int) (tab-hovered tab int) (tab-selected tab int)
  (tab-button-pressed tab int) (pre-popup-pressed)))


(defgenum (tab-container+tab-position :class 'tab-container) (:top 0)
 (:bottom 1) (:max 2))

(defgclass (text-edit :bind "TextEdit" :api :core)
 (:signals (text-set) (text-changed)
  (lines-edited-from from-line int to-line int) (caret-changed)
  (gutter-clicked line int gutter int) (gutter-added) (gutter-removed)))


(defgenum (text-edit+menu-items :class 'text-edit) (:cut 0) (:copy 1)
 (:paste 2) (:clear 3) (:select-all 4) (:undo 5) (:redo 6)
 (:submenu-text-dir 7) (:dir-inherited 8) (:dir-auto 9) (:dir-ltr 10)
 (:dir-rtl 11) (:display-ucc 12) (:submenu-insert-ucc 13) (:insert-lrm 14)
 (:insert-rlm 15) (:insert-lre 16) (:insert-rle 17) (:insert-lro 18)
 (:insert-rlo 19) (:insert-pdf 20) (:insert-alm 21) (:insert-lri 22)
 (:insert-rli 23) (:insert-fsi 24) (:insert-pdi 25) (:insert-zwj 26)
 (:insert-zwnj 27) (:insert-wj 28) (:insert-shy 29) (:emoji-and-symbol 30)
 (:max 31))


(defgenum (text-edit+edit-action :class 'text-edit) (:none 0) (:typing 1)
 (:backspace 2) (:delete 3))


(defgenum (text-edit+search-flags :class 'text-edit) (:match-case 1)
 (:whole-words 2) (:backwards 4))


(defgenum (text-edit+caret-type :class 'text-edit) (:line 0) (:block 1))


(defgenum (text-edit+selection-mode :class 'text-edit) (:none 0) (:shift 1)
 (:pointer 2) (:word 3) (:line 4))


(defgenum (text-edit+line-wrapping-mode :class 'text-edit) (:none 0)
 (:boundary 1))


(defgenum (text-edit+gutter-type :class 'text-edit) (:string 0) (:icon 1)
 (:custom 2))

(defgclass (text-line :bind "TextLine" :api :core :refcounted common-lisp:t))

(defgclass (text-mesh :bind "TextMesh" :api :core :refcounted common-lisp:t))

(defgclass
 (text-paragraph :bind "TextParagraph" :api :core :refcounted common-lisp:t))

(defgclass
 (text-server :bind "TextServer" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))


(defgenum (text-server+font-antialiasing :class 'text-server) (:none 0)
 (:gray 1) (:lcd 2))


(defgenum (text-server+font-lcdsubpixel-layout :class 'text-server) (:none 0)
 (:hrgb 1) (:hbgr 2) (:vrgb 3) (:vbgr 4) (:max 5))


(defgenum (text-server+direction :class 'text-server) (:auto 0) (:ltr 1)
 (:rtl 2) (:inherited 3))


(defgenum (text-server+orientation :class 'text-server) (:horizontal 0)
 (:vertical 1))


(defgenum
 (text-server+justification-flag :bitfield common-lisp:t :class 'text-server)
 (:none 0) (:kashida 1) (:word-bound 2) (:trim-edge-spaces 4)
 (:after-last-tab 8) (:constrain-ellipsis 16) (:skip-last-line 32)
 (:skip-last-line-with-visible-chars 64) (:do-not-skip-single-line 128))


(defgenum (text-server+autowrap-mode :class 'text-server) (:off 0)
 (:arbitrary 1) (:word 2) (:word-smart 3))


(defgenum
 (text-server+line-break-flag :bitfield common-lisp:t :class 'text-server)
 (:none 0) (:mandatory 1) (:word-bound 2) (:grapheme-bound 4) (:adaptive 8)
 (:trim-edge-spaces 16) (:trim-indent 32) (:trim-start-edge-spaces 64)
 (:trim-end-edge-spaces 128))


(defgenum (text-server+visible-characters-behavior :class 'text-server)
 (:chars-before-shaping 0) (:chars-after-shaping 1) (:glyphs-auto 2)
 (:glyphs-ltr 3) (:glyphs-rtl 4))


(defgenum (text-server+overrun-behavior :class 'text-server) (:no-trimming 0)
 (:trim-char 1) (:trim-word 2) (:trim-ellipsis 3) (:trim-word-ellipsis 4)
 (:trim-ellipsis-force 5) (:trim-word-ellipsis-force 6))


(defgenum
 (text-server+text-overrun-flag :bitfield common-lisp:t :class 'text-server)
 (:no-trim 0) (:trim 1) (:trim-word-only 2) (:add-ellipsis 4)
 (:enforce-ellipsis 8) (:justification-aware 16) (:short-string-ellipsis 32))


(defgenum
 (text-server+grapheme-flag :bitfield common-lisp:t :class 'text-server)
 (:valid 1) (:rtl 2) (:virtual 4) (:space 8) (:break-hard 16) (:break-soft 32)
 (:tab 64) (:elongation 128) (:punctuation 256) (:underscore 512)
 (:connected 1024) (:safe-to-insert-tatweel 2048) (:embedded-object 4096)
 (:soft-hyphen 8192))


(defgenum (text-server+hinting :class 'text-server) (:none 0) (:light 1)
 (:normal 2))


(defgenum (text-server+subpixel-positioning :class 'text-server) (:disabled 0)
 (:auto 1) (:one-half 2) (:one-quarter 3) (:one-half-max-size 20)
 (:one-quarter-max-size 16))


(defgenum (text-server+feature :class 'text-server) (:simple-layout 1)
 (:bidi-layout 2) (:vertical-layout 4) (:shaping 8) (:kashida-justification 16)
 (:break-iterators 32) (:font-bitmap 64) (:font-dynamic 128) (:font-msdf 256)
 (:font-system 512) (:font-variable 1024)
 (:context-sensitive-case-conversion 2048) (:use-support-data 4096)
 (:unicode-identifiers 8192) (:unicode-security 16384))


(defgenum (text-server+contour-point-tag :class 'text-server) (:on 1)
 (:off-conic 0) (:off-cubic 2))


(defgenum (text-server+spacing-type :class 'text-server) (:glyph 0) (:space 1)
 (:top 2) (:bottom 3) (:max 4))


(defgenum (text-server+font-style :bitfield common-lisp:t :class 'text-server)
 (:bold 1) (:italic 2) (:fixed-width 4))


(defgenum (text-server+structured-text-parser :class 'text-server) (:default 0)
 (:uri 1) (:file 2) (:email 3) (:list 4) (:gdscript 5) (:custom 6))


(defgenum (text-server+fixed-size-scale-mode :class 'text-server) (:disable 0)
 (:integer-only 1) (:enabled 2))

(defgclass
 (text-server-advanced :bind "TextServerAdvanced" :api :core :refcounted
  common-lisp:t))

(defgclass
 (text-server-dummy :bind "TextServerDummy" :api :core :refcounted
  common-lisp:t))

(defgclass
 (text-server-extension :bind "TextServerExtension" :api :core :refcounted
  common-lisp:t))

(defgclass (text-server-manager :bind "TextServerManager" :api :core)
 (:signals (interface-added interface-name string-name)
  (interface-removed interface-name string-name)))

(defgclass (texture :bind "Texture" :api :core :refcounted common-lisp:t))

(defgclass (texture-2d :bind "Texture2D" :api :core :refcounted common-lisp:t))

(defgclass
 (texture-2darray :bind "Texture2DArray" :api :core :refcounted common-lisp:t))

(defgclass
 (texture-2darray-rd :bind "Texture2DArrayRD" :api :core :refcounted
  common-lisp:t))

(defgclass
 (texture-2drd :bind "Texture2DRD" :api :core :refcounted common-lisp:t))

(defgclass (texture-3d :bind "Texture3D" :api :core :refcounted common-lisp:t))

(defgclass
 (texture-3drd :bind "Texture3DRD" :api :core :refcounted common-lisp:t))

(defgclass (texture-button :bind "TextureButton" :api :core))


(defgenum (texture-button+stretch-mode :class 'texture-button) (:scale 0)
 (:tile 1) (:keep 2) (:keep-centered 3) (:keep-aspect 4)
 (:keep-aspect-centered 5) (:keep-aspect-covered 6))

(defgclass
 (texture-cubemap-array-rd :bind "TextureCubemapArrayRD" :api :core :refcounted
  common-lisp:t))

(defgclass
 (texture-cubemap-rd :bind "TextureCubemapRD" :api :core :refcounted
  common-lisp:t))

(defgclass
 (texture-layered :bind "TextureLayered" :api :core :refcounted common-lisp:t))


(defgenum (texture-layered+layered-type :class 'texture-layered) (:2d-array 0)
 (:cubemap 1) (:cubemap-array 2))

(defgclass
 (texture-layered-rd :bind "TextureLayeredRD" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))

(defgclass (texture-progress-bar :bind "TextureProgressBar" :api :core))


(defgenum (texture-progress-bar+fill-mode :class 'texture-progress-bar)
 (:left-to-right 0) (:right-to-left 1) (:top-to-bottom 2) (:bottom-to-top 3)
 (:clockwise 4) (:counter-clockwise 5) (:bilinear-left-and-right 6)
 (:bilinear-top-and-bottom 7) (:clockwise-and-counter-clockwise 8))

(defgclass (texture-rect :bind "TextureRect" :api :core))


(defgenum (texture-rect+expand-mode :class 'texture-rect) (:keep-size 0)
 (:ignore-size 1) (:fit-width 2) (:fit-width-proportional 3) (:fit-height 4)
 (:fit-height-proportional 5))


(defgenum (texture-rect+stretch-mode :class 'texture-rect) (:scale 0) (:tile 1)
 (:keep 2) (:keep-centered 3) (:keep-aspect 4) (:keep-aspect-centered 5)
 (:keep-aspect-covered 6))

(defgclass (theme :bind "Theme" :api :core :refcounted common-lisp:t))


(defgenum (theme+data-type :class 'theme) (:color 0) (:constant 1) (:font 2)
 (:font-size 3) (:icon 4) (:stylebox 5) (:max 6))

(defgclass (theme-db :bind "ThemeDB" :api :core) (:signals (fallback-changed)))

(defgclass (thread :bind "Thread" :api :core :refcounted common-lisp:t))


(defgenum (thread+priority :class 'thread) (:low 0) (:normal 1) (:high 2))

(defgclass (tile-data :bind "TileData" :api :core) (:signals (changed)))

(defgclass (tile-map :bind "TileMap" :api :core) (:signals (changed)))


(defgenum (tile-map+visibility-mode :class 'tile-map) (:default 0)
 (:force-hide 2) (:force-show 1))

(defgclass (tile-map-layer :bind "TileMapLayer" :api :core)
 (:signals (changed)))


(defgenum (tile-map-layer+debug-visibility-mode :class 'tile-map-layer)
 (:default 0) (:force-hide 2) (:force-show 1))

(defgclass
 (tile-map-pattern :bind "TileMapPattern" :api :core :refcounted common-lisp:t))

(defgclass (tile-set :bind "TileSet" :api :core :refcounted common-lisp:t))


(defgenum (tile-set+tile-shape :class 'tile-set) (:square 0) (:isometric 1)
 (:half-offset-square 2) (:hexagon 3))


(defgenum (tile-set+tile-layout :class 'tile-set) (:stacked 0)
 (:stacked-offset 1) (:stairs-right 2) (:stairs-down 3) (:diamond-right 4)
 (:diamond-down 5))


(defgenum (tile-set+tile-offset-axis :class 'tile-set) (:horizontal 0)
 (:vertical 1))


(defgenum (tile-set+cell-neighbor :class 'tile-set) (:right-side 0)
 (:right-corner 1) (:bottom-right-side 2) (:bottom-right-corner 3)
 (:bottom-side 4) (:bottom-corner 5) (:bottom-left-side 6)
 (:bottom-left-corner 7) (:left-side 8) (:left-corner 9) (:top-left-side 10)
 (:top-left-corner 11) (:top-side 12) (:top-corner 13) (:top-right-side 14)
 (:top-right-corner 15))


(defgenum (tile-set+terrain-mode :class 'tile-set) (:corners-and-sides 0)
 (:corners 1) (:sides 2))

(defgclass
 (tile-set-atlas-source :bind "TileSetAtlasSource" :api :core :refcounted
  common-lisp:t))


(defgconstant +tile-set-atlas-source+transform-flip-h+ :value 4096 :bind
 "TRANSFORM_FLIP_H" :class 'tile-set-atlas-source)


(defgconstant +tile-set-atlas-source+transform-flip-v+ :value 8192 :bind
 "TRANSFORM_FLIP_V" :class 'tile-set-atlas-source)


(defgconstant +tile-set-atlas-source+transform-transpose+ :value 16384 :bind
 "TRANSFORM_TRANSPOSE" :class 'tile-set-atlas-source)


(defgenum
 (tile-set-atlas-source+tile-animation-mode :class 'tile-set-atlas-source)
 (:default 0) (:random-start-times 1) (:max 2))

(defgclass
 (tile-set-scenes-collection-source :bind "TileSetScenesCollectionSource" :api
  :core :refcounted common-lisp:t))

(defgclass
 (tile-set-source :bind "TileSetSource" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))

(defgclass (time :bind "Time" :api :core))


(defgenum (time+month :class 'time) (:january 1) (:february 2) (:march 3)
 (:april 4) (:may 5) (:june 6) (:july 7) (:august 8) (:september 9)
 (:october 10) (:november 11) (:december 12))


(defgenum (time+weekday :class 'time) (:sunday 0) (:monday 1) (:tuesday 2)
 (:wednesday 3) (:thursday 4) (:friday 5) (:saturday 6))

(defgclass (timer :bind "Timer" :api :core) (:signals (timeout)))


(defgenum (timer+timer-process-callback :class 'timer) (:physics 0) (:idle 1))

(defgclass (torus-mesh :bind "TorusMesh" :api :core :refcounted common-lisp:t))

(defgclass (touch-screen-button :bind "TouchScreenButton" :api :core)
 (:signals (pressed) (released)))


(defgenum (touch-screen-button+visibility-mode :class 'touch-screen-button)
 (:always 0) (:touchscreen-only 1))

(defgclass
 (translation :bind "Translation" :api :core :refcounted common-lisp:t))

(defgclass
 (translation-domain :bind "TranslationDomain" :api :core :refcounted
  common-lisp:t))

(defgclass (translation-server :bind "TranslationServer" :api :core))

(defgclass (tree :bind "Tree" :api :core)
 (:signals (item-selected) (cell-selected)
  (multi-selected item tree-item column int selected bool)
  (item-mouse-selected mouse-position vector-2 mouse-button-index int)
  (empty-clicked click-position vector-2 mouse-button-index int) (item-edited)
  (custom-item-clicked mouse-button-index int) (item-icon-double-clicked)
  (item-collapsed item tree-item)
  (check-propagated-to-item item tree-item column int)
  (button-clicked item tree-item column int id int mouse-button-index int)
  (custom-popup-edited arrow-clicked bool) (item-activated)
  (column-title-clicked column int mouse-button-index int) (nothing-selected)))


(defgenum (tree+select-mode :class 'tree) (:single 0) (:row 1) (:multi 2))


(defgenum (tree+drop-mode-flags :class 'tree) (:disabled 0) (:on-item 1)
 (:inbetween 2))


(defgenum (tree+scroll-hint-mode :class 'tree) (:disabled 0) (:both 1) (:top 2)
 (:bottom 3))

(defgclass
 (tree-item :bind "TreeItem" :api :core :instantiable common-lisp:nil))


(defgenum (tree-item+tree-cell-mode :class 'tree-item) (:string 0) (:check 1)
 (:range 2) (:icon 3) (:custom 4))

(defgclass
 (triangle-mesh :bind "TriangleMesh" :api :core :refcounted common-lisp:t))

(defgclass
 (tube-trail-mesh :bind "TubeTrailMesh" :api :core :refcounted common-lisp:t))

(defgclass (tween :bind "Tween" :api :core :refcounted common-lisp:t)
 (:signals (step-finished idx int) (loop-finished loop-count int) (finished)))


(defgenum (tween+tween-process-mode :class 'tween) (:physics 0) (:idle 1))


(defgenum (tween+tween-pause-mode :class 'tween) (:bound 0) (:stop 1)
 (:process 2))


(defgenum (tween+transition-type :class 'tween) (:linear 0) (:sine 1)
 (:quint 2) (:quart 3) (:quad 4) (:expo 5) (:elastic 6) (:cubic 7) (:circ 8)
 (:bounce 9) (:back 10) (:spring 11))


(defgenum (tween+ease-type :class 'tween) (:in 0) (:out 1) (:in-out 2)
 (:out-in 3))

(defgclass
 (tweener :bind "Tweener" :api :core :instantiable common-lisp:nil :refcounted
  common-lisp:t)
 (:signals (finished)))

(defgclass (two-bone-ik3d :bind "TwoBoneIK3D" :api :core))

(defgclass (udpserver :bind "UDPServer" :api :core :refcounted common-lisp:t))

(defgclass (udsserver :bind "UDSServer" :api :core :refcounted common-lisp:t))

(defgclass (upnp :bind "UPNP" :api :core :refcounted common-lisp:t))


(defgenum (upnp+upnpresult :class 'upnp) (:success 0) (:not-authorized 1)
 (:port-mapping-not-found 2) (:inconsistent-parameters 3)
 (:no-such-entry-in-array 4) (:action-failed 5)
 (:src-ip-wildcard-not-permitted 6) (:ext-port-wildcard-not-permitted 7)
 (:int-port-wildcard-not-permitted 8) (:remote-host-must-be-wildcard 9)
 (:ext-port-must-be-wildcard 10) (:no-port-maps-available 11)
 (:conflict-with-other-mechanism 12) (:conflict-with-other-mapping 13)
 (:same-port-values-required 14) (:only-permanent-lease-supported 15)
 (:invalid-gateway 16) (:invalid-port 17) (:invalid-protocol 18)
 (:invalid-duration 19) (:invalid-args 20) (:invalid-response 21)
 (:invalid-param 22) (:http-error 23) (:socket-error 24) (:mem-alloc-error 25)
 (:no-gateway 26) (:no-devices 27) (:unknown-error 28))

(defgclass (upnpdevice :bind "UPNPDevice" :api :core :refcounted common-lisp:t))


(defgenum (upnpdevice+igdstatus :class 'upnpdevice) (:ok 0) (:http-error 1)
 (:http-empty 2) (:no-urls 3) (:no-igd 4) (:disconnected 5) (:unknown-device 6)
 (:invalid-control 7) (:malloc-error 8) (:unknown-error 9))

(defgclass (undo-redo :bind "UndoRedo" :api :core) (:signals (version-changed)))


(defgenum (undo-redo+merge-mode :class 'undo-redo) (:disable 0) (:ends 1)
 (:all 2))

(defgclass (uniform-set-cache-rd :bind "UniformSetCacheRD" :api :core))

(defgclass (vbox-container :bind "VBoxContainer" :api :core))

(defgclass (vflow-container :bind "VFlowContainer" :api :core))

(defgclass (vscroll-bar :bind "VScrollBar" :api :core))

(defgclass (vseparator :bind "VSeparator" :api :core))

(defgclass (vslider :bind "VSlider" :api :core))

(defgclass (vsplit-container :bind "VSplitContainer" :api :core))

(defgclass (vehicle-body-3d :bind "VehicleBody3D" :api :core))

(defgclass (vehicle-wheel-3d :bind "VehicleWheel3D" :api :core))

(defgclass
 (video-stream :bind "VideoStream" :api :core :refcounted common-lisp:t))

(defgclass
 (video-stream-playback :bind "VideoStreamPlayback" :api :core :refcounted
  common-lisp:t))

(defgclass (video-stream-player :bind "VideoStreamPlayer" :api :core)
 (:signals (finished)))

(defgclass
 (video-stream-theora :bind "VideoStreamTheora" :api :core :refcounted
  common-lisp:t))

(defgclass (viewport :bind "Viewport" :api :core :instantiable common-lisp:nil)
 (:signals (size-changed) (gui-focus-changed node control)))


(defgenum (viewport+positional-shadow-atlas-quadrant-subdiv :class 'viewport)
 (:disabled 0) (:|1| 1) (:|4| 2) (:|16| 3) (:|64| 4) (:|256| 5) (:|1024| 6)
 (:max 7))


(defgenum (viewport+scaling-3dmode :class 'viewport) (:bilinear 0) (:fsr 1)
 (:fsr2 2) (:metalfx-spatial 3) (:metalfx-temporal 4) (:nearest 5) (:max 6))


(defgenum (viewport+msaa :class 'viewport) (:disabled 0) (:|2X| 1) (:|4X| 2)
 (:|8X| 3) (:max 4))


(defgenum (viewport+anisotropic-filtering :class 'viewport) (:disabled 0)
 (:|2X| 1) (:|4X| 2) (:|8X| 3) (:|16X| 4) (:max 5))


(defgenum (viewport+screen-space-aa :class 'viewport) (:disabled 0) (:fxaa 1)
 (:smaa 2) (:max 3))


(defgenum (viewport+render-info :class 'viewport) (:objects-in-frame 0)
 (:primitives-in-frame 1) (:draw-calls-in-frame 2) (:max 3))


(defgenum (viewport+render-info-type :class 'viewport) (:visible 0) (:shadow 1)
 (:canvas 2) (:max 3))


(defgenum (viewport+debug-draw :class 'viewport) (:disabled 0) (:unshaded 1)
 (:lighting 2) (:overdraw 3) (:wireframe 4) (:normal-buffer 5)
 (:voxel-gi-albedo 6) (:voxel-gi-lighting 7) (:voxel-gi-emission 8)
 (:shadow-atlas 9) (:directional-shadow-atlas 10) (:scene-luminance 11)
 (:ssao 12) (:ssil 13) (:pssm-splits 14) (:decal-atlas 15) (:sdfgi 16)
 (:sdfgi-probes 17) (:gi-buffer 18) (:disable-lod 19) (:cluster-omni-lights 20)
 (:cluster-spot-lights 21) (:cluster-decals 22) (:cluster-reflection-probes 23)
 (:occluders 24) (:motion-vectors 25) (:internal-buffer 26)
 (:cluster-area-lights 27) (:area-light-atlas 28))


(defgenum (viewport+default-canvas-item-texture-filter :class 'viewport)
 (:nearest 0) (:linear 1) (:linear-with-mipmaps 2) (:nearest-with-mipmaps 3)
 (:parent-node 4) (:max 5))


(defgenum (viewport+default-canvas-item-texture-repeat :class 'viewport)
 (:disabled 0) (:enabled 1) (:mirror 2) (:parent-node 3) (:max 4))


(defgenum (viewport+sdfoversize :class 'viewport) (:100-percent 0)
 (:120-percent 1) (:150-percent 2) (:200-percent 3) (:max 4))


(defgenum (viewport+sdfscale :class 'viewport) (:100-percent 0) (:50-percent 1)
 (:25-percent 2) (:max 3))


(defgenum (viewport+vrsmode :class 'viewport) (:disabled 0) (:texture 1)
 (:xr 2) (:max 3))


(defgenum (viewport+vrsupdate-mode :class 'viewport) (:disabled 0) (:once 1)
 (:always 2) (:max 3))

(defgclass
 (viewport-texture :bind "ViewportTexture" :api :core :refcounted
  common-lisp:t))

(defgclass (virtual-joystick :bind "VirtualJoystick" :api :core)
 (:signals (pressed) (tapped) (released input-vector vector-2)
  (flicked input-vector vector-2) (flick-canceled)))


(defgenum (virtual-joystick+joystick-mode :class 'virtual-joystick) (:fixed 0)
 (:dynamic 1) (:following 2))


(defgenum (virtual-joystick+visibility-mode :class 'virtual-joystick)
 (:always 0) (:when-touched 1))

(defgclass
 (visible-on-screen-enabler-2d :bind "VisibleOnScreenEnabler2D" :api :core))


(defgenum
 (visible-on-screen-enabler-2d+enable-mode :class
  'visible-on-screen-enabler-2d)
 (:inherit 0) (:always 1) (:when-paused 2))

(defgclass
 (visible-on-screen-enabler-3d :bind "VisibleOnScreenEnabler3D" :api :core))


(defgenum
 (visible-on-screen-enabler-3d+enable-mode :class
  'visible-on-screen-enabler-3d)
 (:inherit 0) (:always 1) (:when-paused 2))

(defgclass
 (visible-on-screen-notifier-2d :bind "VisibleOnScreenNotifier2D" :api :core)
 (:signals (screen-entered) (screen-exited)))

(defgclass
 (visible-on-screen-notifier-3d :bind "VisibleOnScreenNotifier3D" :api :core)
 (:signals (screen-entered) (screen-exited)))

(defgclass (visual-instance-3d :bind "VisualInstance3D" :api :core))

(defgclass
 (visual-shader :bind "VisualShader" :api :core :refcounted common-lisp:t))


(defgconstant +visual-shader+node-id-invalid+ :value -1 :bind "NODE_ID_INVALID"
 :class 'visual-shader)


(defgconstant +visual-shader+node-id-output+ :value 0 :bind "NODE_ID_OUTPUT"
 :class 'visual-shader)


(defgenum (visual-shader+type :class 'visual-shader) (:vertex 0) (:fragment 1)
 (:light 2) (:start 3) (:process 4) (:collide 5) (:start-custom 6)
 (:process-custom 7) (:sky 8) (:fog 9) (:texture-blit 10) (:max 11))


(defgenum (visual-shader+varying-mode :class 'visual-shader)
 (:vertex-to-frag-light 0) (:frag-to-light 1) (:max 2))


(defgenum (visual-shader+varying-type :class 'visual-shader) (:float 0)
 (:int 1) (:uint 2) (:vector-2d 3) (:vector-3d 4) (:vector-4d 5) (:boolean 6)
 (:transform 7) (:max 8))

(defgclass
 (visual-shader-node :bind "VisualShaderNode" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))


(defgenum (visual-shader-node+port-type :class 'visual-shader-node) (:scalar 0)
 (:scalar-int 1) (:scalar-uint 2) (:vector-2d 3) (:vector-3d 4) (:vector-4d 5)
 (:boolean 6) (:transform 7) (:sampler 8) (:max 9))

(defgclass
 (visual-shader-node-billboard :bind "VisualShaderNodeBillboard" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-billboard+billboard-type :class
  'visual-shader-node-billboard)
 (:disabled 0) (:enabled 1) (:fixed-y 2) (:particles 3) (:max 4))

(defgclass
 (visual-shader-node-boolean-constant :bind "VisualShaderNodeBooleanConstant"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-boolean-parameter :bind "VisualShaderNodeBooleanParameter"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-clamp :bind "VisualShaderNodeClamp" :api :core :refcounted
  common-lisp:t))


(defgenum (visual-shader-node-clamp+op-type :class 'visual-shader-node-clamp)
 (:float 0) (:int 1) (:uint 2) (:vector-2d 3) (:vector-3d 4) (:vector-4d 5)
 (:max 6))

(defgclass
 (visual-shader-node-color-constant :bind "VisualShaderNodeColorConstant" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-color-func :bind "VisualShaderNodeColorFunc" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-color-func+function :class 'visual-shader-node-color-func)
 (:grayscale 0) (:hsv2rgb 1) (:rgb2hsv 2) (:sepia 3) (:linear-to-srgb 4)
 (:srgb-to-linear 5) (:max 6))

(defgclass
 (visual-shader-node-color-op :bind "VisualShaderNodeColorOp" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-color-op+operator :class 'visual-shader-node-color-op)
 (:screen 0) (:difference 1) (:darken 2) (:lighten 3) (:overlay 4) (:dodge 5)
 (:burn 6) (:soft-light 7) (:hard-light 8) (:max 9))

(defgclass
 (visual-shader-node-color-parameter :bind "VisualShaderNodeColorParameter"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-comment :bind "VisualShaderNodeComment" :api :core
  :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-compare :bind "VisualShaderNodeCompare" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-compare+comparison-type :class
  'visual-shader-node-compare)
 (:scalar 0) (:scalar-int 1) (:scalar-uint 2) (:vector-2d 3) (:vector-3d 4)
 (:vector-4d 5) (:boolean 6) (:transform 7) (:max 8))


(defgenum
 (visual-shader-node-compare+function :class 'visual-shader-node-compare)
 (:equal 0) (:not-equal 1) (:greater-than 2) (:greater-than-equal 3)
 (:less-than 4) (:less-than-equal 5) (:max 6))


(defgenum
 (visual-shader-node-compare+condition :class 'visual-shader-node-compare)
 (:all 0) (:any 1) (:max 2))

(defgclass
 (visual-shader-node-constant :bind "VisualShaderNodeConstant" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-cubemap :bind "VisualShaderNodeCubemap" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-cubemap+source :class 'visual-shader-node-cubemap)
 (:texture 0) (:port 1) (:max 2))


(defgenum
 (visual-shader-node-cubemap+texture-type :class 'visual-shader-node-cubemap)
 (:data 0) (:color 1) (:normal-map 2) (:max 3))

(defgclass
 (visual-shader-node-cubemap-parameter :bind "VisualShaderNodeCubemapParameter"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-curve-texture :bind "VisualShaderNodeCurveTexture" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-curve-xyztexture :bind "VisualShaderNodeCurveXYZTexture"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-custom :bind "VisualShaderNodeCustom" :api :core
  :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-derivative-func :bind "VisualShaderNodeDerivativeFunc"
  :api :core :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-derivative-func+op-type :class
  'visual-shader-node-derivative-func)
 (:scalar 0) (:vector-2d 1) (:vector-3d 2) (:vector-4d 3) (:max 4))


(defgenum
 (visual-shader-node-derivative-func+function :class
  'visual-shader-node-derivative-func)
 (:sum 0) (:x 1) (:y 2) (:max 3))


(defgenum
 (visual-shader-node-derivative-func+precision :class
  'visual-shader-node-derivative-func)
 (:none 0) (:coarse 1) (:fine 2) (:max 3))

(defgclass
 (visual-shader-node-determinant :bind "VisualShaderNodeDeterminant" :api :core
  :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-distance-fade :bind "VisualShaderNodeDistanceFade" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-dot-product :bind "VisualShaderNodeDotProduct" :api :core
  :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-expression :bind "VisualShaderNodeExpression" :api :core
  :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-face-forward :bind "VisualShaderNodeFaceForward" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-float-constant :bind "VisualShaderNodeFloatConstant" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-float-func :bind "VisualShaderNodeFloatFunc" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-float-func+function :class 'visual-shader-node-float-func)
 (:sin 0) (:cos 1) (:tan 2) (:asin 3) (:acos 4) (:atan 5) (:sinh 6) (:cosh 7)
 (:tanh 8) (:log 9) (:exp 10) (:sqrt 11) (:abs 12) (:sign 13) (:floor 14)
 (:round 15) (:ceil 16) (:fract 17) (:saturate 18) (:negate 19) (:acosh 20)
 (:asinh 21) (:atanh 22) (:degrees 23) (:exp2 24) (:inverse-sqrt 25) (:log2 26)
 (:radians 27) (:reciprocal 28) (:roundeven 29) (:trunc 30) (:oneminus 31)
 (:max 32))

(defgclass
 (visual-shader-node-float-op :bind "VisualShaderNodeFloatOp" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-float-op+operator :class 'visual-shader-node-float-op)
 (:add 0) (:sub 1) (:mul 2) (:div 3) (:mod 4) (:pow 5) (:max 6) (:min 7)
 (:atan2 8) (:step 9) (:enum-size 10))

(defgclass
 (visual-shader-node-float-parameter :bind "VisualShaderNodeFloatParameter"
  :api :core :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-float-parameter+hint :class
  'visual-shader-node-float-parameter)
 (:none 0) (:range 1) (:range-step 2) (:max 3))

(defgclass
 (visual-shader-node-frame :bind "VisualShaderNodeFrame" :api :core :refcounted
  common-lisp:t))

(defgclass
 (visual-shader-node-fresnel :bind "VisualShaderNodeFresnel" :api :core
  :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-global-expression :bind "VisualShaderNodeGlobalExpression"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-group-base :bind "VisualShaderNodeGroupBase" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-if :bind "VisualShaderNodeIf" :api :core :refcounted
  common-lisp:t))

(defgclass
 (visual-shader-node-input :bind "VisualShaderNodeInput" :api :core :refcounted
  common-lisp:t)
 (:signals (input-type-changed)))

(defgclass
 (visual-shader-node-int-constant :bind "VisualShaderNodeIntConstant" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-int-func :bind "VisualShaderNodeIntFunc" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-int-func+function :class 'visual-shader-node-int-func)
 (:abs 0) (:negate 1) (:sign 2) (:bitwise-not 3) (:max 4))

(defgclass
 (visual-shader-node-int-op :bind "VisualShaderNodeIntOp" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-int-op+operator :class 'visual-shader-node-int-op)
 (:add 0) (:sub 1) (:mul 2) (:div 3) (:mod 4) (:max 5) (:min 6)
 (:bitwise-and 7) (:bitwise-or 8) (:bitwise-xor 9) (:bitwise-left-shift 10)
 (:bitwise-right-shift 11) (:enum-size 12))

(defgclass
 (visual-shader-node-int-parameter :bind "VisualShaderNodeIntParameter" :api
  :core :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-int-parameter+hint :class
  'visual-shader-node-int-parameter)
 (:none 0) (:range 1) (:range-step 2) (:enum 3) (:max 4))

(defgclass
 (visual-shader-node-is :bind "VisualShaderNodeIs" :api :core :refcounted
  common-lisp:t))


(defgenum (visual-shader-node-is+function :class 'visual-shader-node-is)
 (:is-inf 0) (:is-nan 1) (:max 2))

(defgclass
 (visual-shader-node-linear-scene-depth :bind
  "VisualShaderNodeLinearSceneDepth" :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-mix :bind "VisualShaderNodeMix" :api :core :refcounted
  common-lisp:t))


(defgenum (visual-shader-node-mix+op-type :class 'visual-shader-node-mix)
 (:scalar 0) (:vector-2d 1) (:vector-2d-scalar 2) (:vector-3d 3)
 (:vector-3d-scalar 4) (:vector-4d 5) (:vector-4d-scalar 6) (:max 7))

(defgclass
 (visual-shader-node-multiply-add :bind "VisualShaderNodeMultiplyAdd" :api
  :core :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-multiply-add+op-type :class
  'visual-shader-node-multiply-add)
 (:scalar 0) (:vector-2d 1) (:vector-3d 2) (:vector-4d 3) (:max 4))

(defgclass
 (visual-shader-node-outer-product :bind "VisualShaderNodeOuterProduct" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-output :bind "VisualShaderNodeOutput" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-parameter :bind "VisualShaderNodeParameter" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-parameter+qualifier :class 'visual-shader-node-parameter)
 (:none 0) (:global 1) (:instance 2) (:instance-index 3) (:max 4))

(defgclass
 (visual-shader-node-parameter-ref :bind "VisualShaderNodeParameterRef" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-particle-accelerator :bind
  "VisualShaderNodeParticleAccelerator" :api :core :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-particle-accelerator+mode :class
  'visual-shader-node-particle-accelerator)
 (:linear 0) (:radial 1) (:tangential 2) (:max 3))

(defgclass
 (visual-shader-node-particle-box-emitter :bind
  "VisualShaderNodeParticleBoxEmitter" :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-particle-cone-velocity :bind
  "VisualShaderNodeParticleConeVelocity" :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-particle-emit :bind "VisualShaderNodeParticleEmit" :api
  :core :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-particle-emit+emit-flags :class
  'visual-shader-node-particle-emit)
 (:position 1) (:rot-scale 2) (:velocity 4) (:color 8) (:custom 16))

(defgclass
 (visual-shader-node-particle-emitter :bind "VisualShaderNodeParticleEmitter"
  :api :core :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-particle-mesh-emitter :bind
  "VisualShaderNodeParticleMeshEmitter" :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-particle-multiply-by-axis-angle :bind
  "VisualShaderNodeParticleMultiplyByAxisAngle" :api :core :refcounted
  common-lisp:t))

(defgclass
 (visual-shader-node-particle-output :bind "VisualShaderNodeParticleOutput"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-particle-randomness :bind
  "VisualShaderNodeParticleRandomness" :api :core :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-particle-randomness+op-type :class
  'visual-shader-node-particle-randomness)
 (:scalar 0) (:vector-2d 1) (:vector-3d 2) (:vector-4d 3) (:max 4))

(defgclass
 (visual-shader-node-particle-ring-emitter :bind
  "VisualShaderNodeParticleRingEmitter" :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-particle-sphere-emitter :bind
  "VisualShaderNodeParticleSphereEmitter" :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-proximity-fade :bind "VisualShaderNodeProximityFade" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-random-range :bind "VisualShaderNodeRandomRange" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-remap :bind "VisualShaderNodeRemap" :api :core :refcounted
  common-lisp:t))


(defgenum (visual-shader-node-remap+op-type :class 'visual-shader-node-remap)
 (:scalar 0) (:vector-2d 1) (:vector-2d-scalar 2) (:vector-3d 3)
 (:vector-3d-scalar 4) (:vector-4d 5) (:vector-4d-scalar 6) (:max 7))

(defgclass
 (visual-shader-node-reroute :bind "VisualShaderNodeReroute" :api :core
  :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-resizable-base :bind "VisualShaderNodeResizableBase" :api
  :core :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-rotation-by-axis :bind "VisualShaderNodeRotationByAxis"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-sdfraymarch :bind "VisualShaderNodeSDFRaymarch" :api :core
  :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-sdfto-screen-uv :bind "VisualShaderNodeSDFToScreenUV" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-sample-3d :bind "VisualShaderNodeSample3D" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-sample-3d+source :class 'visual-shader-node-sample-3d)
 (:texture 0) (:port 1) (:max 2))

(defgclass
 (visual-shader-node-screen-normal-world-space :bind
  "VisualShaderNodeScreenNormalWorldSpace" :api :core :refcounted
  common-lisp:t))

(defgclass
 (visual-shader-node-screen-uvto-sdf :bind "VisualShaderNodeScreenUVToSDF" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-smooth-step :bind "VisualShaderNodeSmoothStep" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-smooth-step+op-type :class
  'visual-shader-node-smooth-step)
 (:scalar 0) (:vector-2d 1) (:vector-2d-scalar 2) (:vector-3d 3)
 (:vector-3d-scalar 4) (:vector-4d 5) (:vector-4d-scalar 6) (:max 7))

(defgclass
 (visual-shader-node-step :bind "VisualShaderNodeStep" :api :core :refcounted
  common-lisp:t))


(defgenum (visual-shader-node-step+op-type :class 'visual-shader-node-step)
 (:scalar 0) (:vector-2d 1) (:vector-2d-scalar 2) (:vector-3d 3)
 (:vector-3d-scalar 4) (:vector-4d 5) (:vector-4d-scalar 6) (:max 7))

(defgclass
 (visual-shader-node-switch :bind "VisualShaderNodeSwitch" :api :core
  :refcounted common-lisp:t))


(defgenum (visual-shader-node-switch+op-type :class 'visual-shader-node-switch)
 (:float 0) (:int 1) (:uint 2) (:vector-2d 3) (:vector-3d 4) (:vector-4d 5)
 (:boolean 6) (:transform 7) (:max 8))

(defgclass
 (visual-shader-node-texture :bind "VisualShaderNodeTexture" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-texture+source :class 'visual-shader-node-texture)
 (:texture 0) (:screen 1) (:2d-texture 2) (:2d-normal 3) (:depth 4) (:port 5)
 (:3d-normal 6) (:roughness 7) (:max 8))


(defgenum
 (visual-shader-node-texture+texture-type :class 'visual-shader-node-texture)
 (:data 0) (:color 1) (:normal-map 2) (:max 3))

(defgclass
 (visual-shader-node-texture-2darray :bind "VisualShaderNodeTexture2DArray"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-texture-2darray-parameter :bind
  "VisualShaderNodeTexture2DArrayParameter" :api :core :refcounted
  common-lisp:t))

(defgclass
 (visual-shader-node-texture-2dparameter :bind
  "VisualShaderNodeTexture2DParameter" :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-texture-3d :bind "VisualShaderNodeTexture3D" :api :core
  :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-texture-3dparameter :bind
  "VisualShaderNodeTexture3DParameter" :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-texture-parameter :bind "VisualShaderNodeTextureParameter"
  :api :core :instantiable common-lisp:nil :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-texture-parameter+texture-type :class
  'visual-shader-node-texture-parameter)
 (:data 0) (:color 1) (:normal-map 2) (:anisotropy 3) (:max 4))


(defgenum
 (visual-shader-node-texture-parameter+color-default :class
  'visual-shader-node-texture-parameter)
 (:white 0) (:black 1) (:transparent 2) (:max 3))


(defgenum
 (visual-shader-node-texture-parameter+texture-filter :class
  'visual-shader-node-texture-parameter)
 (:default 0) (:nearest 1) (:linear 2) (:nearest-mipmap 3) (:linear-mipmap 4)
 (:nearest-mipmap-anisotropic 5) (:linear-mipmap-anisotropic 6) (:max 7))


(defgenum
 (visual-shader-node-texture-parameter+texture-repeat :class
  'visual-shader-node-texture-parameter)
 (:default 0) (:enabled 1) (:disabled 2) (:max 3))


(defgenum
 (visual-shader-node-texture-parameter+texture-source :class
  'visual-shader-node-texture-parameter)
 (:none 0) (:screen 1) (:depth 2) (:normal-roughness 3) (:max 4))

(defgclass
 (visual-shader-node-texture-parameter-triplanar :bind
  "VisualShaderNodeTextureParameterTriplanar" :api :core :refcounted
  common-lisp:t))

(defgclass
 (visual-shader-node-texture-sdf :bind "VisualShaderNodeTextureSDF" :api :core
  :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-texture-sdfnormal :bind "VisualShaderNodeTextureSDFNormal"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-transform-compose :bind "VisualShaderNodeTransformCompose"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-transform-constant :bind
  "VisualShaderNodeTransformConstant" :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-transform-decompose :bind
  "VisualShaderNodeTransformDecompose" :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-transform-func :bind "VisualShaderNodeTransformFunc" :api
  :core :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-transform-func+function :class
  'visual-shader-node-transform-func)
 (:inverse 0) (:transpose 1) (:max 2))

(defgclass
 (visual-shader-node-transform-op :bind "VisualShaderNodeTransformOp" :api
  :core :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-transform-op+operator :class
  'visual-shader-node-transform-op)
 (:axb 0) (:bxa 1) (:axb-comp 2) (:bxa-comp 3) (:add 4) (:a-minus-b 5)
 (:b-minus-a 6) (:a-div-b 7) (:b-div-a 8) (:max 9))

(defgclass
 (visual-shader-node-transform-parameter :bind
  "VisualShaderNodeTransformParameter" :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-transform-vec-mult :bind
  "VisualShaderNodeTransformVecMult" :api :core :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-transform-vec-mult+operator :class
  'visual-shader-node-transform-vec-mult)
 (:axb 0) (:bxa 1) (:3x3-axb 2) (:3x3-bxa 3) (:max 4))

(defgclass
 (visual-shader-node-uint-constant :bind "VisualShaderNodeUIntConstant" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-uint-func :bind "VisualShaderNodeUIntFunc" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-uint-func+function :class 'visual-shader-node-uint-func)
 (:negate 0) (:bitwise-not 1) (:max 2))

(defgclass
 (visual-shader-node-uint-op :bind "VisualShaderNodeUIntOp" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-uint-op+operator :class 'visual-shader-node-uint-op)
 (:add 0) (:sub 1) (:mul 2) (:div 3) (:mod 4) (:max 5) (:min 6)
 (:bitwise-and 7) (:bitwise-or 8) (:bitwise-xor 9) (:bitwise-left-shift 10)
 (:bitwise-right-shift 11) (:enum-size 12))

(defgclass
 (visual-shader-node-uint-parameter :bind "VisualShaderNodeUIntParameter" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-uvfunc :bind "VisualShaderNodeUVFunc" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-uvfunc+function :class 'visual-shader-node-uvfunc)
 (:panning 0) (:scaling 1) (:max 2))

(defgclass
 (visual-shader-node-uvpolar-coord :bind "VisualShaderNodeUVPolarCoord" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-varying :bind "VisualShaderNodeVarying" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-varying-getter :bind "VisualShaderNodeVaryingGetter" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-varying-setter :bind "VisualShaderNodeVaryingSetter" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-vec-2constant :bind "VisualShaderNodeVec2Constant" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-vec-2parameter :bind "VisualShaderNodeVec2Parameter" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-vec-3constant :bind "VisualShaderNodeVec3Constant" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-vec-3parameter :bind "VisualShaderNodeVec3Parameter" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-vec-4constant :bind "VisualShaderNodeVec4Constant" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-vec-4parameter :bind "VisualShaderNodeVec4Parameter" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-vector-base :bind "VisualShaderNodeVectorBase" :api :core
  :instantiable common-lisp:nil :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-vector-base+op-type :class
  'visual-shader-node-vector-base)
 (:vector-2d 0) (:vector-3d 1) (:vector-4d 2) (:max 3))

(defgclass
 (visual-shader-node-vector-compose :bind "VisualShaderNodeVectorCompose" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-vector-decompose :bind "VisualShaderNodeVectorDecompose"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-vector-distance :bind "VisualShaderNodeVectorDistance"
  :api :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-vector-func :bind "VisualShaderNodeVectorFunc" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-vector-func+function :class
  'visual-shader-node-vector-func)
 (:normalize 0) (:saturate 1) (:negate 2) (:reciprocal 3) (:abs 4) (:acos 5)
 (:acosh 6) (:asin 7) (:asinh 8) (:atan 9) (:atanh 10) (:ceil 11) (:cos 12)
 (:cosh 13) (:degrees 14) (:exp 15) (:exp2 16) (:floor 17) (:fract 18)
 (:inverse-sqrt 19) (:log 20) (:log2 21) (:radians 22) (:round 23)
 (:roundeven 24) (:sign 25) (:sin 26) (:sinh 27) (:sqrt 28) (:tan 29)
 (:tanh 30) (:trunc 31) (:oneminus 32) (:max 33))

(defgclass
 (visual-shader-node-vector-len :bind "VisualShaderNodeVectorLen" :api :core
  :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-vector-op :bind "VisualShaderNodeVectorOp" :api :core
  :refcounted common-lisp:t))


(defgenum
 (visual-shader-node-vector-op+operator :class 'visual-shader-node-vector-op)
 (:add 0) (:sub 1) (:mul 2) (:div 3) (:mod 4) (:pow 5) (:max 6) (:min 7)
 (:cross 8) (:atan2 9) (:reflect 10) (:step 11) (:enum-size 12))

(defgclass
 (visual-shader-node-vector-refract :bind "VisualShaderNodeVectorRefract" :api
  :core :refcounted common-lisp:t))

(defgclass
 (visual-shader-node-world-position-from-depth :bind
  "VisualShaderNodeWorldPositionFromDepth" :api :core :refcounted
  common-lisp:t))

(defgclass (voxel-gi :bind "VoxelGI" :api :core))


(defgenum (voxel-gi+subdiv :class 'voxel-gi) (:|64| 0) (:|128| 1) (:|256| 2)
 (:|512| 3) (:max 4))

(defgclass
 (voxel-gidata :bind "VoxelGIData" :api :core :refcounted common-lisp:t))

(defgclass (weak-ref :bind "WeakRef" :api :core :refcounted common-lisp:t))

(defgclass
 (web-rtcdata-channel :bind "WebRTCDataChannel" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t))


(defgenum (web-rtcdata-channel+write-mode :class 'web-rtcdata-channel)
 (:text 0) (:binary 1))


(defgenum (web-rtcdata-channel+channel-state :class 'web-rtcdata-channel)
 (:connecting 0) (:open 1) (:closing 2) (:closed 3))

(defgclass
 (web-rtcdata-channel-extension :bind "WebRTCDataChannelExtension" :api :core
  :refcounted common-lisp:t))

(defgclass
 (web-rtcmultiplayer-peer :bind "WebRTCMultiplayerPeer" :api :core :refcounted
  common-lisp:t))

(defgclass
 (web-rtcpeer-connection :bind "WebRTCPeerConnection" :api :core :refcounted
  common-lisp:t)
 (:signals (session-description-created type string sdp string)
  (ice-candidate-created media string index int name string)
  (data-channel-received channel web-rtcdata-channel)))


(defgenum
 (web-rtcpeer-connection+connection-state :class 'web-rtcpeer-connection)
 (:new 0) (:connecting 1) (:connected 2) (:disconnected 3) (:failed 4)
 (:closed 5))


(defgenum
 (web-rtcpeer-connection+gathering-state :class 'web-rtcpeer-connection)
 (:new 0) (:gathering 1) (:complete 2))


(defgenum
 (web-rtcpeer-connection+signaling-state :class 'web-rtcpeer-connection)
 (:stable 0) (:have-local-offer 1) (:have-remote-offer 2)
 (:have-local-pranswer 3) (:have-remote-pranswer 4) (:closed 5))

(defgclass
 (web-rtcpeer-connection-extension :bind "WebRTCPeerConnectionExtension" :api
  :core :refcounted common-lisp:t))

(defgclass
 (web-socket-multiplayer-peer :bind "WebSocketMultiplayerPeer" :api :core
  :refcounted common-lisp:t))

(defgclass
 (web-socket-peer :bind "WebSocketPeer" :api :core :refcounted common-lisp:t))


(defgenum (web-socket-peer+write-mode :class 'web-socket-peer) (:text 0)
 (:binary 1))


(defgenum (web-socket-peer+state :class 'web-socket-peer) (:connecting 0)
 (:open 1) (:closing 2) (:closed 3))

(defgclass
 (web-xrinterface :bind "WebXRInterface" :api :core :instantiable
  common-lisp:nil :refcounted common-lisp:t)
 (:signals (session-supported session-mode string supported bool)
  (session-started) (session-ended) (session-failed message string)
  (selectstart input-source-id int) (select input-source-id int)
  (selectend input-source-id int) (squeezestart input-source-id int)
  (squeeze input-source-id int) (squeezeend input-source-id int)
  (visibility-state-changed) (reference-space-reset)
  (display-refresh-rate-changed)))


(defgenum (web-xrinterface+target-ray-mode :class 'web-xrinterface)
 (:unknown 0) (:gaze 1) (:tracked-pointer 2) (:screen 3))

(defgclass (window :bind "Window" :api :core)
 (:signals (window-input event input-event)
  (nonclient-window-input event input-event)
  (files-dropped files packed-string-array) (mouse-entered) (mouse-exited)
  (focus-entered) (focus-exited) (close-requested) (go-back-requested)
  (visibility-changed) (about-to-popup) (theme-changed) (dpi-changed)
  (titlebar-changed) (title-changed)
  (output-max-linear-value-changed output-max-linear-value float)))


(defgconstant +window+notification-visibility-changed+ :value 30 :bind
 "NOTIFICATION_VISIBILITY_CHANGED" :class 'window)


(defgconstant +window+notification-theme-changed+ :value 32 :bind
 "NOTIFICATION_THEME_CHANGED" :class 'window)


(defgenum (window+mode :class 'window) (:windowed 0) (:minimized 1)
 (:maximized 2) (:fullscreen 3) (:exclusive-fullscreen 4))


(defgenum (window+flags :class 'window) (:resize-disabled 0) (:borderless 1)
 (:always-on-top 2) (:transparent 3) (:no-focus 4) (:popup 5)
 (:extend-to-title 6) (:mouse-passthrough 7) (:sharp-corners 8)
 (:exclude-from-capture 9) (:popup-wm-hint 10) (:minimize-disabled 11)
 (:maximize-disabled 12) (:max 13))


(defgenum (window+content-scale-mode :class 'window) (:disabled 0)
 (:canvas-items 1) (:viewport 2))


(defgenum (window+content-scale-aspect :class 'window) (:ignore 0) (:keep 1)
 (:keep-width 2) (:keep-height 3) (:expand 4))


(defgenum (window+content-scale-stretch :class 'window) (:fractional 0)
 (:integer 1))


(defgenum (window+layout-direction :class 'window) (:inherited 0)
 (:application-locale 1) (:ltr 2) (:rtl 3) (:system-locale 4) (:max 5)
 (:locale 1))


(defgenum (window+window-initial-position :class 'window) (:absolute 0)
 (:center-primary-screen 1) (:center-main-window-screen 2)
 (:center-other-screen 3) (:center-screen-with-mouse-focus 4)
 (:center-screen-with-keyboard-focus 5))

(defgclass
 (worker-thread-pool :bind "WorkerThreadPool" :api :core :instantiable
  common-lisp:nil))

(defgclass (world-2d :bind "World2D" :api :core :refcounted common-lisp:t))

(defgclass (world-3d :bind "World3D" :api :core :refcounted common-lisp:t))

(defgclass
 (world-boundary-shape-2d :bind "WorldBoundaryShape2D" :api :core :refcounted
  common-lisp:t))

(defgclass
 (world-boundary-shape-3d :bind "WorldBoundaryShape3D" :api :core :refcounted
  common-lisp:t))

(defgclass (world-environment :bind "WorldEnvironment" :api :core))

(defgclass
 (x509certificate :bind "X509Certificate" :api :core :refcounted common-lisp:t))

(defgclass (xmlparser :bind "XMLParser" :api :core :refcounted common-lisp:t))


(defgenum (xmlparser+node-type :class 'xmlparser) (:none 0) (:element 1)
 (:element-end 2) (:text 3) (:comment 4) (:cdata 5) (:unknown 6))

(defgclass (xranchor-3d :bind "XRAnchor3D" :api :core))

(defgclass (xrbody-modifier-3d :bind "XRBodyModifier3D" :api :core))


(defgenum
 (xrbody-modifier-3d+body-update :bitfield common-lisp:t :class
  'xrbody-modifier-3d)
 (:upper-body 1) (:lower-body 2) (:hands 4))


(defgenum (xrbody-modifier-3d+bone-update :class 'xrbody-modifier-3d) (:full 0)
 (:rotation-only 1) (:max 2))

(defgclass
 (xrbody-tracker :bind "XRBodyTracker" :api :core :refcounted common-lisp:t))


(defgenum
 (xrbody-tracker+body-flags :bitfield common-lisp:t :class 'xrbody-tracker)
 (:upper-body-supported 1) (:lower-body-supported 2) (:hands-supported 4))


(defgenum (xrbody-tracker+joint :class 'xrbody-tracker) (:root 0) (:hips 1)
 (:spine 2) (:chest 3) (:upper-chest 4) (:neck 5) (:head 6) (:head-tip 7)
 (:left-shoulder 8) (:left-upper-arm 9) (:left-lower-arm 10)
 (:right-shoulder 11) (:right-upper-arm 12) (:right-lower-arm 13)
 (:left-upper-leg 14) (:left-lower-leg 15) (:left-foot 16) (:left-toes 17)
 (:right-upper-leg 18) (:right-lower-leg 19) (:right-foot 20) (:right-toes 21)
 (:left-hand 22) (:left-palm 23) (:left-wrist 24) (:left-thumb-metacarpal 25)
 (:left-thumb-phalanx-proximal 26) (:left-thumb-phalanx-distal 27)
 (:left-thumb-tip 28) (:left-index-finger-metacarpal 29)
 (:left-index-finger-phalanx-proximal 30)
 (:left-index-finger-phalanx-intermediate 31)
 (:left-index-finger-phalanx-distal 32) (:left-index-finger-tip 33)
 (:left-middle-finger-metacarpal 34) (:left-middle-finger-phalanx-proximal 35)
 (:left-middle-finger-phalanx-intermediate 36)
 (:left-middle-finger-phalanx-distal 37) (:left-middle-finger-tip 38)
 (:left-ring-finger-metacarpal 39) (:left-ring-finger-phalanx-proximal 40)
 (:left-ring-finger-phalanx-intermediate 41)
 (:left-ring-finger-phalanx-distal 42) (:left-ring-finger-tip 43)
 (:left-pinky-finger-metacarpal 44) (:left-pinky-finger-phalanx-proximal 45)
 (:left-pinky-finger-phalanx-intermediate 46)
 (:left-pinky-finger-phalanx-distal 47) (:left-pinky-finger-tip 48)
 (:right-hand 49) (:right-palm 50) (:right-wrist 51)
 (:right-thumb-metacarpal 52) (:right-thumb-phalanx-proximal 53)
 (:right-thumb-phalanx-distal 54) (:right-thumb-tip 55)
 (:right-index-finger-metacarpal 56) (:right-index-finger-phalanx-proximal 57)
 (:right-index-finger-phalanx-intermediate 58)
 (:right-index-finger-phalanx-distal 59) (:right-index-finger-tip 60)
 (:right-middle-finger-metacarpal 61)
 (:right-middle-finger-phalanx-proximal 62)
 (:right-middle-finger-phalanx-intermediate 63)
 (:right-middle-finger-phalanx-distal 64) (:right-middle-finger-tip 65)
 (:right-ring-finger-metacarpal 66) (:right-ring-finger-phalanx-proximal 67)
 (:right-ring-finger-phalanx-intermediate 68)
 (:right-ring-finger-phalanx-distal 69) (:right-ring-finger-tip 70)
 (:right-pinky-finger-metacarpal 71) (:right-pinky-finger-phalanx-proximal 72)
 (:right-pinky-finger-phalanx-intermediate 73)
 (:right-pinky-finger-phalanx-distal 74) (:right-pinky-finger-tip 75)
 (:lower-chest 76) (:left-scapula 77) (:left-wrist-twist 78)
 (:right-scapula 79) (:right-wrist-twist 80) (:left-foot-twist 81)
 (:left-heel 82) (:left-middle-foot 83) (:right-foot-twist 84) (:right-heel 85)
 (:right-middle-foot 86) (:max 87))


(defgenum
 (xrbody-tracker+joint-flags :bitfield common-lisp:t :class 'xrbody-tracker)
 (:orientation-valid 1) (:orientation-tracked 2) (:position-valid 4)
 (:position-tracked 8))

(defgclass (xrcamera-3d :bind "XRCamera3D" :api :core))

(defgclass (xrcontroller-3d :bind "XRController3D" :api :core)
 (:signals (button-pressed action-name string)
  (button-released action-name string)
  (input-float-changed action-name string value float)
  (input-vector2-changed action-name string value vector-2)
  (profile-changed role string)))

(defgclass
 (xrcontroller-tracker :bind "XRControllerTracker" :api :core :refcounted
  common-lisp:t))

(defgclass (xrface-modifier-3d :bind "XRFaceModifier3D" :api :core))

(defgclass
 (xrface-tracker :bind "XRFaceTracker" :api :core :refcounted common-lisp:t))


(defgenum (xrface-tracker+blend-shape-entry :class 'xrface-tracker)
 (:eye-look-out-right 0) (:eye-look-in-right 1) (:eye-look-up-right 2)
 (:eye-look-down-right 3) (:eye-look-out-left 4) (:eye-look-in-left 5)
 (:eye-look-up-left 6) (:eye-look-down-left 7) (:eye-closed-right 8)
 (:eye-closed-left 9) (:eye-squint-right 10) (:eye-squint-left 11)
 (:eye-wide-right 12) (:eye-wide-left 13) (:eye-dilation-right 14)
 (:eye-dilation-left 15) (:eye-constrict-right 16) (:eye-constrict-left 17)
 (:brow-pinch-right 18) (:brow-pinch-left 19) (:brow-lowerer-right 20)
 (:brow-lowerer-left 21) (:brow-inner-up-right 22) (:brow-inner-up-left 23)
 (:brow-outer-up-right 24) (:brow-outer-up-left 25) (:nose-sneer-right 26)
 (:nose-sneer-left 27) (:nasal-dilation-right 28) (:nasal-dilation-left 29)
 (:nasal-constrict-right 30) (:nasal-constrict-left 31)
 (:cheek-squint-right 32) (:cheek-squint-left 33) (:cheek-puff-right 34)
 (:cheek-puff-left 35) (:cheek-suck-right 36) (:cheek-suck-left 37)
 (:jaw-open 38) (:mouth-closed 39) (:jaw-right 40) (:jaw-left 41)
 (:jaw-forward 42) (:jaw-backward 43) (:jaw-clench 44) (:jaw-mandible-raise 45)
 (:lip-suck-upper-right 46) (:lip-suck-upper-left 47)
 (:lip-suck-lower-right 48) (:lip-suck-lower-left 49)
 (:lip-suck-corner-right 50) (:lip-suck-corner-left 51)
 (:lip-funnel-upper-right 52) (:lip-funnel-upper-left 53)
 (:lip-funnel-lower-right 54) (:lip-funnel-lower-left 55)
 (:lip-pucker-upper-right 56) (:lip-pucker-upper-left 57)
 (:lip-pucker-lower-right 58) (:lip-pucker-lower-left 59)
 (:mouth-upper-up-right 60) (:mouth-upper-up-left 61)
 (:mouth-lower-down-right 62) (:mouth-lower-down-left 63)
 (:mouth-upper-deepen-right 64) (:mouth-upper-deepen-left 65)
 (:mouth-upper-right 66) (:mouth-upper-left 67) (:mouth-lower-right 68)
 (:mouth-lower-left 69) (:mouth-corner-pull-right 70)
 (:mouth-corner-pull-left 71) (:mouth-corner-slant-right 72)
 (:mouth-corner-slant-left 73) (:mouth-frown-right 74) (:mouth-frown-left 75)
 (:mouth-stretch-right 76) (:mouth-stretch-left 77) (:mouth-dimple-right 78)
 (:mouth-dimple-left 79) (:mouth-raiser-upper 80) (:mouth-raiser-lower 81)
 (:mouth-press-right 82) (:mouth-press-left 83) (:mouth-tightener-right 84)
 (:mouth-tightener-left 85) (:tongue-out 86) (:tongue-up 87) (:tongue-down 88)
 (:tongue-right 89) (:tongue-left 90) (:tongue-roll 91) (:tongue-blend-down 92)
 (:tongue-curl-up 93) (:tongue-squish 94) (:tongue-flat 95)
 (:tongue-twist-right 96) (:tongue-twist-left 97) (:soft-palate-close 98)
 (:throat-swallow 99) (:neck-flex-right 100) (:neck-flex-left 101)
 (:eye-closed 102) (:eye-wide 103) (:eye-squint 104) (:eye-dilation 105)
 (:eye-constrict 106) (:brow-down-right 107) (:brow-down-left 108)
 (:brow-down 109) (:brow-up-right 110) (:brow-up-left 111) (:brow-up 112)
 (:nose-sneer 113) (:nasal-dilation 114) (:nasal-constrict 115)
 (:cheek-puff 116) (:cheek-suck 117) (:cheek-squint 118) (:lip-suck-upper 119)
 (:lip-suck-lower 120) (:lip-suck 121) (:lip-funnel-upper 122)
 (:lip-funnel-lower 123) (:lip-funnel 124) (:lip-pucker-upper 125)
 (:lip-pucker-lower 126) (:lip-pucker 127) (:mouth-upper-up 128)
 (:mouth-lower-down 129) (:mouth-open 130) (:mouth-right 131) (:mouth-left 132)
 (:mouth-smile-right 133) (:mouth-smile-left 134) (:mouth-smile 135)
 (:mouth-sad-right 136) (:mouth-sad-left 137) (:mouth-sad 138)
 (:mouth-stretch 139) (:mouth-dimple 140) (:mouth-tightener 141)
 (:mouth-press 142) (:max 143))

(defgclass (xrhand-modifier-3d :bind "XRHandModifier3D" :api :core))


(defgenum (xrhand-modifier-3d+bone-update :class 'xrhand-modifier-3d) (:full 0)
 (:rotation-only 1) (:max 2))

(defgclass
 (xrhand-tracker :bind "XRHandTracker" :api :core :refcounted common-lisp:t))


(defgenum (xrhand-tracker+hand-tracking-source :class 'xrhand-tracker)
 (:unknown 0) (:unobstructed 1) (:controller 2) (:not-tracked 3) (:max 4))


(defgenum (xrhand-tracker+hand-joint :class 'xrhand-tracker) (:palm 0)
 (:wrist 1) (:thumb-metacarpal 2) (:thumb-phalanx-proximal 3)
 (:thumb-phalanx-distal 4) (:thumb-tip 5) (:index-finger-metacarpal 6)
 (:index-finger-phalanx-proximal 7) (:index-finger-phalanx-intermediate 8)
 (:index-finger-phalanx-distal 9) (:index-finger-tip 10)
 (:middle-finger-metacarpal 11) (:middle-finger-phalanx-proximal 12)
 (:middle-finger-phalanx-intermediate 13) (:middle-finger-phalanx-distal 14)
 (:middle-finger-tip 15) (:ring-finger-metacarpal 16)
 (:ring-finger-phalanx-proximal 17) (:ring-finger-phalanx-intermediate 18)
 (:ring-finger-phalanx-distal 19) (:ring-finger-tip 20)
 (:pinky-finger-metacarpal 21) (:pinky-finger-phalanx-proximal 22)
 (:pinky-finger-phalanx-intermediate 23) (:pinky-finger-phalanx-distal 24)
 (:pinky-finger-tip 25) (:max 26))


(defgenum
 (xrhand-tracker+hand-joint-flags :bitfield common-lisp:t :class
  'xrhand-tracker)
 (:orientation-valid 1) (:orientation-tracked 2) (:position-valid 4)
 (:position-tracked 8) (:linear-velocity-valid 16) (:angular-velocity-valid 32))

(defgclass
 (xrinterface :bind "XRInterface" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t)
 (:signals (play-area-changed mode int)))


(defgenum (xrinterface+capabilities :class 'xrinterface) (:none 0) (:mono 1)
 (:stereo 2) (:quad 4) (:vr 8) (:ar 16) (:external 32))


(defgenum (xrinterface+tracking-status :class 'xrinterface)
 (:normal-tracking 0) (:excessive-motion 1) (:insufficient-features 2)
 (:unknown-tracking 3) (:not-tracking 4))


(defgenum (xrinterface+play-area-mode :class 'xrinterface) (:unknown 0)
 (:3dof 1) (:sitting 2) (:roomscale 3) (:stage 4) (:custom 2147483647))


(defgenum (xrinterface+environment-blend-mode :class 'xrinterface) (:opaque 0)
 (:additive 1) (:alpha-blend 2))


(defgenum (xrinterface+vrstexture-format :class 'xrinterface) (:unified 0)
 (:fragment-shading-rate 1) (:fragment-density-map 2))

(defgclass
 (xrinterface-extension :bind "XRInterfaceExtension" :api :core :refcounted
  common-lisp:t))

(defgclass (xrnode-3d :bind "XRNode3D" :api :core)
 (:signals (tracking-changed tracking bool)))

(defgclass (xrorigin-3d :bind "XROrigin3D" :api :core))

(defgclass (xrpose :bind "XRPose" :api :core :refcounted common-lisp:t))


(defgenum (xrpose+tracking-confidence :class 'xrpose) (:none 0) (:low 1)
 (:high 2))

(defgclass
 (xrpositional-tracker :bind "XRPositionalTracker" :api :core :refcounted
  common-lisp:t)
 (:signals (pose-changed pose xrpose) (pose-lost-tracking pose xrpose)
  (button-pressed action-name string) (button-released action-name string)
  (input-float-changed action-name string value float)
  (input-vector2-changed action-name string vector vector-2)
  (profile-changed role string)))


(defgenum (xrpositional-tracker+tracker-hand :class 'xrpositional-tracker)
 (:unknown 0) (:left 1) (:right 2) (:max 3))

(defgclass (xrserver :bind "XRServer" :api :core)
 (:signals (reference-frame-changed)
  (interface-added interface-name string-name)
  (interface-removed interface-name string-name)
  (tracker-added tracker-name string-name type int)
  (tracker-updated tracker-name string-name type int)
  (tracker-removed tracker-name string-name type int) (world-origin-changed)))


(defgenum (xrserver+tracker-type :class 'xrserver) (:head 1) (:controller 2)
 (:basestation 4) (:anchor 8) (:hand 16) (:body 32) (:face 64) (:any-known 127)
 (:unknown 128) (:any 255))


(defgenum (xrserver+rotation-mode :class 'xrserver) (:reset-full-rotation 0)
 (:reset-but-keep-tilt 1) (:dont-reset-rotation 2))

(defgclass
 (xrtracker :bind "XRTracker" :api :core :instantiable common-lisp:nil
  :refcounted common-lisp:t))

(defgclass (xrvrs :bind "XRVRS" :api :core))

(defgclass (zippacker :bind "ZIPPacker" :api :core :refcounted common-lisp:t))


(defgenum (zippacker+zip-append :class 'zippacker) (:create 0) (:createafter 1)
 (:addinzip 2))


(defgenum (zippacker+compression-level :class 'zippacker) (:default -1)
 (:none 0) (:fast 1) (:best 9))

(defgclass (zipreader :bind "ZIPReader" :api :core :refcounted common-lisp:t))