(common-lisp:in-package :%godot)


(defgmethod
 (editor-export-plugin+%export-file :class 'editor-export-plugin :bind
  "_export_file" :hash 3533781844 :virtual common-lisp:t)
 :void (path string) (type string) (features packed-string-array))

(defgmethod
 (editor-export-plugin+%export-begin :class 'editor-export-plugin :bind
  "_export_begin" :hash 2765511433 :virtual common-lisp:t)
 :void (features packed-string-array) (is-debug bool) (path string) (flags int))

(defgmethod
 (editor-export-plugin+%export-end :class 'editor-export-plugin :bind
  "_export_end" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-export-plugin+%begin-customize-resources :class 'editor-export-plugin
  :bind "_begin_customize_resources" :hash 1312023292 :virtual common-lisp:t)
 bool (platform editor-export-platform) (features packed-string-array))

(defgmethod
 (editor-export-plugin+%customize-resource :class 'editor-export-plugin :bind
  "_customize_resource" :hash 307917495 :virtual common-lisp:t)
 resource (resource resource) (path string))

(defgmethod
 (editor-export-plugin+%begin-customize-scenes :class 'editor-export-plugin
  :bind "_begin_customize_scenes" :hash 1312023292 :virtual common-lisp:t)
 bool (platform editor-export-platform) (features packed-string-array))

(defgmethod
 (editor-export-plugin+%customize-scene :class 'editor-export-plugin :bind
  "_customize_scene" :hash 498701822 :virtual common-lisp:t)
 node (scene node) (path string))

(defgmethod
 (editor-export-plugin+%get-customization-configuration-hash :class
  'editor-export-plugin :bind "_get_customization_configuration_hash" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (editor-export-plugin+%end-customize-scenes :class 'editor-export-plugin :bind
  "_end_customize_scenes" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-export-plugin+%end-customize-resources :class 'editor-export-plugin
  :bind "_end_customize_resources" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-export-plugin+%get-export-options :class 'editor-export-plugin :bind
  "_get_export_options" :hash 488349689 :virtual common-lisp:t)
 array (platform editor-export-platform))

(defgmethod
 (editor-export-plugin+%get-export-options-overrides :class
  'editor-export-plugin :bind "_get_export_options_overrides" :hash 2837326714
  :virtual common-lisp:t)
 dictionary (platform editor-export-platform))

(defgmethod
 (editor-export-plugin+%should-update-export-options :class
  'editor-export-plugin :bind "_should_update_export_options" :hash 1866233299
  :virtual common-lisp:t)
 bool (platform editor-export-platform))

(defgmethod
 (editor-export-plugin+%get-export-option-visibility :class
  'editor-export-plugin :bind "_get_export_option_visibility" :hash 3537301980
  :virtual common-lisp:t)
 bool (platform editor-export-platform) (option string))

(defgmethod
 (editor-export-plugin+%get-export-option-warning :class 'editor-export-plugin
  :bind "_get_export_option_warning" :hash 3340251247 :virtual common-lisp:t)
 string (platform editor-export-platform) (option string))

(defgmethod
 (editor-export-plugin+%get-export-features :class 'editor-export-plugin :bind
  "_get_export_features" :hash 1057664154 :virtual common-lisp:t)
 packed-string-array (platform editor-export-platform) (debug bool))

(defgmethod
 (editor-export-plugin+%get-name :class 'editor-export-plugin :bind "_get_name"
  :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (editor-export-plugin+%supports-platform :class 'editor-export-plugin :bind
  "_supports_platform" :hash 1866233299 :virtual common-lisp:t)
 bool (platform editor-export-platform))

(defgmethod
 (editor-export-plugin+%get-android-dependencies :class 'editor-export-plugin
  :bind "_get_android_dependencies" :hash 1057664154 :virtual common-lisp:t)
 packed-string-array (platform editor-export-platform) (debug bool))

(defgmethod
 (editor-export-plugin+%get-android-dependencies-maven-repos :class
  'editor-export-plugin :bind "_get_android_dependencies_maven_repos" :hash
  1057664154 :virtual common-lisp:t)
 packed-string-array (platform editor-export-platform) (debug bool))

(defgmethod
 (editor-export-plugin+%get-android-libraries :class 'editor-export-plugin
  :bind "_get_android_libraries" :hash 1057664154 :virtual common-lisp:t)
 packed-string-array (platform editor-export-platform) (debug bool))

(defgmethod
 (editor-export-plugin+%get-android-manifest-activity-element-contents :class
  'editor-export-plugin :bind "_get_android_manifest_activity_element_contents"
  :hash 4013372917 :virtual common-lisp:t)
 string (platform editor-export-platform) (debug bool))

(defgmethod
 (editor-export-plugin+%get-android-manifest-application-element-contents
  :class 'editor-export-plugin :bind
  "_get_android_manifest_application_element_contents" :hash 4013372917
  :virtual common-lisp:t)
 string (platform editor-export-platform) (debug bool))

(defgmethod
 (editor-export-plugin+%get-android-manifest-element-contents :class
  'editor-export-plugin :bind "_get_android_manifest_element_contents" :hash
  4013372917 :virtual common-lisp:t)
 string (platform editor-export-platform) (debug bool))

(defgmethod
 (editor-export-plugin+%update-android-prebuilt-manifest :class
  'editor-export-plugin :bind "_update_android_prebuilt_manifest" :hash
  3304965187 :virtual common-lisp:t)
 packed-byte-array (platform editor-export-platform)
 (manifest-data packed-byte-array))

(defgmethod
 (editor-export-plugin+add-shared-object :class 'editor-export-plugin :bind
  "add_shared_object" :hash 3098291045)
 :void (path string) (tags packed-string-array) (target string))

(defgmethod
 (editor-export-plugin+add-file :class 'editor-export-plugin :bind "add_file"
  :hash 527928637)
 :void (path string) (file packed-byte-array) (remap bool))

(defgmethod
 (editor-export-plugin+add-apple-embedded-platform-project-static-lib :class
  'editor-export-plugin :bind "add_apple_embedded_platform_project_static_lib"
  :hash 83702148)
 :void (path string))

(defgmethod
 (editor-export-plugin+add-apple-embedded-platform-framework :class
  'editor-export-plugin :bind "add_apple_embedded_platform_framework" :hash
  83702148)
 :void (path string))

(defgmethod
 (editor-export-plugin+add-apple-embedded-platform-embedded-framework :class
  'editor-export-plugin :bind "add_apple_embedded_platform_embedded_framework"
  :hash 83702148)
 :void (path string))

(defgmethod
 (editor-export-plugin+add-apple-embedded-platform-plist-content :class
  'editor-export-plugin :bind "add_apple_embedded_platform_plist_content" :hash
  83702148)
 :void (plist-content string))

(defgmethod
 (editor-export-plugin+add-apple-embedded-platform-linker-flags :class
  'editor-export-plugin :bind "add_apple_embedded_platform_linker_flags" :hash
  83702148)
 :void (flags string))

(defgmethod
 (editor-export-plugin+add-apple-embedded-platform-bundle-file :class
  'editor-export-plugin :bind "add_apple_embedded_platform_bundle_file" :hash
  83702148)
 :void (path string))

(defgmethod
 (editor-export-plugin+add-apple-embedded-platform-cpp-code :class
  'editor-export-plugin :bind "add_apple_embedded_platform_cpp_code" :hash
  83702148)
 :void (code string))

(defgmethod
 (editor-export-plugin+add-ios-project-static-lib :class 'editor-export-plugin
  :bind "add_ios_project_static_lib" :hash 83702148)
 :void (path string))

(defgmethod
 (editor-export-plugin+add-ios-framework :class 'editor-export-plugin :bind
  "add_ios_framework" :hash 83702148)
 :void (path string))

(defgmethod
 (editor-export-plugin+add-ios-embedded-framework :class 'editor-export-plugin
  :bind "add_ios_embedded_framework" :hash 83702148)
 :void (path string))

(defgmethod
 (editor-export-plugin+add-ios-plist-content :class 'editor-export-plugin :bind
  "add_ios_plist_content" :hash 83702148)
 :void (plist-content string))

(defgmethod
 (editor-export-plugin+add-ios-linker-flags :class 'editor-export-plugin :bind
  "add_ios_linker_flags" :hash 83702148)
 :void (flags string))

(defgmethod
 (editor-export-plugin+add-ios-bundle-file :class 'editor-export-plugin :bind
  "add_ios_bundle_file" :hash 83702148)
 :void (path string))

(defgmethod
 (editor-export-plugin+add-ios-cpp-code :class 'editor-export-plugin :bind
  "add_ios_cpp_code" :hash 83702148)
 :void (code string))

(defgmethod
 (editor-export-plugin+add-macos-plugin-file :class 'editor-export-plugin :bind
  "add_macos_plugin_file" :hash 83702148)
 :void (path string))

(defgmethod
 (editor-export-plugin+skip :class 'editor-export-plugin :bind "skip" :hash
  3218959716)
 :void)

(defgmethod
 (editor-export-plugin+get-option :class 'editor-export-plugin :bind
  "get_option" :hash 2760726917)
 variant (name string-name))

(defgmethod
 (editor-export-plugin+get-export-preset :class 'editor-export-plugin :bind
  "get_export_preset" :hash 1610607222)
 editor-export-preset)

(defgmethod
 (editor-export-plugin+get-export-platform :class 'editor-export-plugin :bind
  "get_export_platform" :hash 282254641)
 editor-export-platform)