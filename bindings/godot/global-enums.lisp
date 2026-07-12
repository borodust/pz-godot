(common-lisp:in-package :%godot)



(defgenum side (:left 0) (:top 1) (:right 2) (:bottom 3))


(defgenum corner (:top-left 0) (:top-right 1) (:bottom-right 2)
 (:bottom-left 3))


(defgenum orientation (:vertical 1) (:horizontal 0))


(defgenum clock-direction (:clockwise 0) (:counterclockwise 1))


(defgenum horizontal-alignment (:left 0) (:center 1) (:right 2) (:fill 3))


(defgenum vertical-alignment (:top 0) (:center 1) (:bottom 2) (:fill 3))


(defgenum inline-alignment (:top-to 0) (:center-to 1) (:baseline-to 3)
 (:bottom-to 2) (:to-top 0) (:to-center 4) (:to-baseline 8) (:to-bottom 12)
 (:top 0) (:center 5) (:bottom 14) (:image-mask 3) (:text-mask 12))


(defgenum euler-order (:xyz 0) (:xzy 1) (:yxz 2) (:yzx 3) (:zxy 4) (:zyx 5))


(defgenum key (:none 0) (:special 4194304) (:escape 4194305) (:tab 4194306)
 (:backtab 4194307) (:backspace 4194308) (:enter 4194309) (:kp-enter 4194310)
 (:insert 4194311) (:delete 4194312) (:pause 4194313) (:print 4194314)
 (:sysreq 4194315) (:clear 4194316) (:home 4194317) (:end 4194318)
 (:left 4194319) (:up 4194320) (:right 4194321) (:down 4194322)
 (:pageup 4194323) (:pagedown 4194324) (:shift 4194325) (:ctrl 4194326)
 (:meta 4194327) (:alt 4194328) (:capslock 4194329) (:numlock 4194330)
 (:scrolllock 4194331) (:f1 4194332) (:f2 4194333) (:f3 4194334) (:f4 4194335)
 (:f5 4194336) (:f6 4194337) (:f7 4194338) (:f8 4194339) (:f9 4194340)
 (:f10 4194341) (:f11 4194342) (:f12 4194343) (:f13 4194344) (:f14 4194345)
 (:f15 4194346) (:f16 4194347) (:f17 4194348) (:f18 4194349) (:f19 4194350)
 (:f20 4194351) (:f21 4194352) (:f22 4194353) (:f23 4194354) (:f24 4194355)
 (:f25 4194356) (:f26 4194357) (:f27 4194358) (:f28 4194359) (:f29 4194360)
 (:f30 4194361) (:f31 4194362) (:f32 4194363) (:f33 4194364) (:f34 4194365)
 (:f35 4194366) (:kp-multiply 4194433) (:kp-divide 4194434)
 (:kp-subtract 4194435) (:kp-period 4194436) (:kp-add 4194437) (:kp-0 4194438)
 (:kp-1 4194439) (:kp-2 4194440) (:kp-3 4194441) (:kp-4 4194442)
 (:kp-5 4194443) (:kp-6 4194444) (:kp-7 4194445) (:kp-8 4194446)
 (:kp-9 4194447) (:menu 4194370) (:hyper 4194371) (:help 4194373)
 (:back 4194376) (:forward 4194377) (:stop 4194378) (:refresh 4194379)
 (:volumedown 4194380) (:volumemute 4194381) (:volumeup 4194382)
 (:mediaplay 4194388) (:mediastop 4194389) (:mediaprevious 4194390)
 (:medianext 4194391) (:mediarecord 4194392) (:homepage 4194393)
 (:favorites 4194394) (:search 4194395) (:standby 4194396) (:openurl 4194397)
 (:launchmail 4194398) (:launchmedia 4194399) (:launch0 4194400)
 (:launch1 4194401) (:launch2 4194402) (:launch3 4194403) (:launch4 4194404)
 (:launch5 4194405) (:launch6 4194406) (:launch7 4194407) (:launch8 4194408)
 (:launch9 4194409) (:launcha 4194410) (:launchb 4194411) (:launchc 4194412)
 (:launchd 4194413) (:launche 4194414) (:launchf 4194415) (:globe 4194416)
 (:keyboard 4194417) (:jis-eisu 4194418) (:jis-kana 4194419) (:unknown 8388607)
 (:space 32) (:exclam 33) (:quotedbl 34) (:numbersign 35) (:dollar 36)
 (:percent 37) (:ampersand 38) (:apostrophe 39) (:parenleft 40)
 (:parenright 41) (:asterisk 42) (:plus 43) (:comma 44) (:minus 45)
 (:period 46) (:slash 47) (:|0| 48) (:|1| 49) (:|2| 50) (:|3| 51) (:|4| 52)
 (:|5| 53) (:|6| 54) (:|7| 55) (:|8| 56) (:|9| 57) (:colon 58) (:semicolon 59)
 (:less 60) (:equal 61) (:greater 62) (:question 63) (:at 64) (:a 65) (:b 66)
 (:c 67) (:d 68) (:e 69) (:f 70) (:g 71) (:h 72) (:i 73) (:j 74) (:k 75)
 (:l 76) (:m 77) (:n 78) (:o 79) (:p 80) (:q 81) (:r 82) (:s 83) (:t 84)
 (:u 85) (:v 86) (:w 87) (:x 88) (:y 89) (:z 90) (:bracketleft 91)
 (:backslash 92) (:bracketright 93) (:asciicircum 94) (:underscore 95)
 (:quoteleft 96) (:braceleft 123) (:bar 124) (:braceright 125)
 (:asciitilde 126) (:yen 165) (:section 167))


(defgenum (key-modifier-mask :bitfield common-lisp:t) (:code-mask 8388607)
 (:modifier-mask 2130706432) (:mask-cmd-or-ctrl 16777216)
 (:mask-shift 33554432) (:mask-alt 67108864) (:mask-meta 134217728)
 (:mask-ctrl 268435456) (:mask-kpad 536870912) (:mask-group-switch 1073741824))


(defgenum key-location (:unspecified 0) (:left 1) (:right 2))


(defgenum mouse-button (:none 0) (:left 1) (:right 2) (:middle 3) (:wheel-up 4)
 (:wheel-down 5) (:wheel-left 6) (:wheel-right 7) (:xbutton1 8) (:xbutton2 9))


(defgenum (mouse-button-mask :bitfield common-lisp:t) (:left 1) (:right 2)
 (:middle 4) (:mb-xbutton1 128) (:mb-xbutton2 256))


(defgenum joy-button (:invalid -1) (:a 0) (:b 1) (:x 2) (:y 3) (:back 4)
 (:guide 5) (:start 6) (:left-stick 7) (:right-stick 8) (:left-shoulder 9)
 (:right-shoulder 10) (:dpad-up 11) (:dpad-down 12) (:dpad-left 13)
 (:dpad-right 14) (:misc1 15) (:paddle1 16) (:paddle2 17) (:paddle3 18)
 (:paddle4 19) (:touchpad 20) (:misc2 21) (:misc3 22) (:misc4 23) (:misc5 24)
 (:misc6 25) (:sdl-max 26) (:max 128))


(defgenum joy-axis (:invalid -1) (:left-x 0) (:left-y 1) (:right-x 2)
 (:right-y 3) (:trigger-left 4) (:trigger-right 5) (:sdl-max 6) (:max 10))


(defgenum midimessage (:none 0) (:note-off 8) (:note-on 9) (:aftertouch 10)
 (:control-change 11) (:program-change 12) (:channel-pressure 13)
 (:pitch-bend 14) (:system-exclusive 240) (:quarter-frame 241)
 (:song-position-pointer 242) (:song-select 243) (:tune-request 246)
 (:timing-clock 248) (:start 250) (:continue 251) (:stop 252)
 (:active-sensing 254) (:system-reset 255))


(defgenum error (:ok 0) (:failed 1) (:err-unavailable 2) (:err-unconfigured 3)
 (:err-unauthorized 4) (:err-parameter-range-error 5) (:err-out-of-memory 6)
 (:err-file-not-found 7) (:err-file-bad-drive 8) (:err-file-bad-path 9)
 (:err-file-no-permission 10) (:err-file-already-in-use 11)
 (:err-file-cant-open 12) (:err-file-cant-write 13) (:err-file-cant-read 14)
 (:err-file-unrecognized 15) (:err-file-corrupt 16)
 (:err-file-missing-dependencies 17) (:err-file-eof 18) (:err-cant-open 19)
 (:err-cant-create 20) (:err-query-failed 21) (:err-already-in-use 22)
 (:err-locked 23) (:err-timeout 24) (:err-cant-connect 25)
 (:err-cant-resolve 26) (:err-connection-error 27)
 (:err-cant-acquire-resource 28) (:err-cant-fork 29) (:err-invalid-data 30)
 (:err-invalid-parameter 31) (:err-already-exists 32) (:err-does-not-exist 33)
 (:err-database-cant-read 34) (:err-database-cant-write 35)
 (:err-compilation-failed 36) (:err-method-not-found 37) (:err-link-failed 38)
 (:err-script-failed 39) (:err-cyclic-link 40) (:err-invalid-declaration 41)
 (:err-duplicate-symbol 42) (:err-parse-error 43) (:err-busy 44) (:err-skip 45)
 (:err-help 46) (:err-bug 47) (:err-printer-on-fire 48))


(defgenum property-hint (:none 0) (:range 1) (:enum 2) (:enum-suggestion 3)
 (:exp-easing 4) (:link 5) (:flags 6) (:layers-2d-render 7)
 (:layers-2d-physics 8) (:layers-2d-navigation 9) (:layers-3d-render 10)
 (:layers-3d-physics 11) (:layers-3d-navigation 12) (:layers-avoidance 37)
 (:file 13) (:dir 14) (:global-file 15) (:global-dir 16) (:resource-type 17)
 (:multiline-text 18) (:expression 19) (:placeholder-text 20)
 (:color-no-alpha 21) (:object-id 22) (:type-string 23)
 (:node-path-to-edited-node 24) (:object-too-big 25)
 (:node-path-valid-types 26) (:save-file 27) (:global-save-file 28)
 (:int-is-objectid 29) (:int-is-pointer 30) (:array-type 31)
 (:dictionary-type 38) (:locale-id 32) (:localizable-string 33) (:node-type 34)
 (:hide-quaternion-edit 35) (:password 36) (:tool-button 39) (:oneshot 40)
 (:group-enable 42) (:input-name 43) (:file-path 44) (:max 45))


(defgenum (property-usage-flags :bitfield common-lisp:t) (:none 0) (:storage 2)
 (:editor 4) (:internal 8) (:checkable 16) (:checked 32) (:group 64)
 (:category 128) (:subgroup 256) (:class-is-bitfield 512)
 (:no-instance-state 1024) (:restart-if-changed 2048) (:script-variable 4096)
 (:store-if-null 8192) (:update-all-if-modified 16384)
 (:script-default-value 32768) (:class-is-enum 65536) (:nil-is-variant 131072)
 (:array 262144) (:always-duplicate 524288) (:never-duplicate 1048576)
 (:high-end-gfx 2097152) (:node-path-from-scene-root 4194304)
 (:resource-not-persistent 8388608) (:keying-increments 16777216)
 (:deferred-set-resource 33554432) (:editor-instantiate-object 67108864)
 (:editor-basic-setting 134217728) (:read-only 268435456) (:secret 536870912)
 (:default 6) (:no-editor 2))


(defgenum (method-flags :bitfield common-lisp:t) (:flag-normal 1)
 (:flag-editor 2) (:flag-const 4) (:flag-virtual 8) (:flag-vararg 16)
 (:flag-static 32) (:flag-object-core 64) (:flag-virtual-required 128)
 (:flags-default 1))


(defgenum variant+type (:nil 0) (:bool 1) (:int 2) (:float 3) (:string 4)
 (:vector2 5) (:vector2i 6) (:rect2 7) (:rect2i 8) (:vector3 9) (:vector3i 10)
 (:transform2d 11) (:vector4 12) (:vector4i 13) (:plane 14) (:quaternion 15)
 (:aabb 16) (:basis 17) (:transform3d 18) (:projection 19) (:color 20)
 (:string-name 21) (:node-path 22) (:rid 23) (:object 24) (:callable 25)
 (:signal 26) (:dictionary 27) (:array 28) (:packed-byte-array 29)
 (:packed-int32-array 30) (:packed-int64-array 31) (:packed-float32-array 32)
 (:packed-float64-array 33) (:packed-string-array 34)
 (:packed-vector2-array 35) (:packed-vector3-array 36) (:packed-color-array 37)
 (:packed-vector4-array 38) (:max 39))


(defgenum variant+operator (:equal 0) (:not-equal 1) (:less 2) (:less-equal 3)
 (:greater 4) (:greater-equal 5) (:add 6) (:subtract 7) (:multiply 8)
 (:divide 9) (:negate 10) (:positive 11) (:module 12) (:power 13)
 (:shift-left 14) (:shift-right 15) (:bit-and 16) (:bit-or 17) (:bit-xor 18)
 (:bit-negate 19) (:and 20) (:or 21) (:xor 22) (:not 23) (:in 24) (:max 25))