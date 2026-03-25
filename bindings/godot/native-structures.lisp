(common-lisp:in-package :%godot)


(cffi:defcstruct audio-frame
  (left float)
  (right float))(cffi:defctype audio-frame (:struct audio-frame))

(cffi:defcstruct caret-info
  (leading-caret rect-2)
  (trailing-caret rect-2)
  (leading-direction text-server+direction)
  (trailing-direction text-server+direction))(cffi:defctype caret-info
                                                            (:struct
                                                             caret-info))

(cffi:defcstruct glyph
  (start int)
  (end int)
  (count :uint8)
  (repeat :uint8)
  (flags :uint16)
  (x-off float)
  (y-off float)
  (advance float)
  (font-rid rid)
  (font-size int)
  (index :int32))(cffi:defctype glyph (:struct glyph))

(cffi:defcstruct object-id
  (id :uint64))(cffi:defctype object-id (:struct object-id))

(cffi:defcstruct physics-server-2dextension-motion-result
  (travel vector-2)
  (remainder vector-2)
  (collision-point vector-2)
  (collision-normal vector-2)
  (collider-velocity vector-2)
  (collision-depth :float)
  (collision-safe-fraction :float)
  (collision-unsafe-fraction :float)
  (collision-local-shape int)
  (collider-id object-id)
  (collider rid)
  (collider-shape int))(cffi:defctype physics-server-2dextension-motion-result
                                      (:struct
                                       physics-server-2dextension-motion-result))

(cffi:defcstruct physics-server-2dextension-ray-result
  (position vector-2)
  (normal vector-2)
  (rid rid)
  (collider-id object-id)
  (collider (:pointer object))
  (shape int))(cffi:defctype physics-server-2dextension-ray-result
                             (:struct physics-server-2dextension-ray-result))

(cffi:defcstruct physics-server-2dextension-shape-rest-info
  (point vector-2)
  (normal vector-2)
  (rid rid)
  (collider-id object-id)
  (shape int)
  (linear-velocity vector-2))(cffi:defctype
                              physics-server-2dextension-shape-rest-info
                              (:struct
                               physics-server-2dextension-shape-rest-info))

(cffi:defcstruct physics-server-2dextension-shape-result
  (rid rid)
  (collider-id object-id)
  (collider (:pointer object))
  (shape int))(cffi:defctype physics-server-2dextension-shape-result
                             (:struct physics-server-2dextension-shape-result))

(cffi:defcstruct physics-server-3dextension-motion-collision
  (position vector-3)
  (normal vector-3)
  (collider-velocity vector-3)
  (collider-angular-velocity vector-3)
  (depth :float)
  (local-shape int)
  (collider-id object-id)
  (collider rid)
  (collider-shape int))(cffi:defctype
                        physics-server-3dextension-motion-collision
                        (:struct physics-server-3dextension-motion-collision))

(cffi:defcstruct physics-server-3dextension-motion-result
  (travel vector-3)
  (remainder vector-3)
  (collision-depth :float)
  (collision-safe-fraction :float)
  (collision-unsafe-fraction :float)
  (collisions (:array physics-server-3dextension-motion-collision 32))
  (collision-count int))(cffi:defctype physics-server-3dextension-motion-result
                                       (:struct
                                        physics-server-3dextension-motion-result))

(cffi:defcstruct physics-server-3dextension-ray-result
  (position vector-3)
  (normal vector-3)
  (rid rid)
  (collider-id object-id)
  (collider (:pointer object))
  (shape int)
  (face-index int))(cffi:defctype physics-server-3dextension-ray-result
                                  (:struct
                                   physics-server-3dextension-ray-result))

(cffi:defcstruct physics-server-3dextension-shape-rest-info
  (point vector-3)
  (normal vector-3)
  (rid rid)
  (collider-id object-id)
  (shape int)
  (linear-velocity vector-3))(cffi:defctype
                              physics-server-3dextension-shape-rest-info
                              (:struct
                               physics-server-3dextension-shape-rest-info))

(cffi:defcstruct physics-server-3dextension-shape-result
  (rid rid)
  (collider-id object-id)
  (collider (:pointer object))
  (shape int))(cffi:defctype physics-server-3dextension-shape-result
                             (:struct physics-server-3dextension-shape-result))

(cffi:defcstruct script-language-extension-profiling-info
  (signature string-name)
  (call-count :uint64)
  (total-time :uint64)
  (self-time :uint64))(cffi:defctype script-language-extension-profiling-info
                                     (:struct
                                      script-language-extension-profiling-info))