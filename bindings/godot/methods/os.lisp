(common-lisp:in-package :%godot)


(defgmethod (os+get-entropy :class 'os :bind "get_entropy" :hash 47165747)
 packed-byte-array (size int))

(defgmethod
 (os+get-system-ca-certificates :class 'os :bind "get_system_ca_certificates"
  :hash 2841200299)
 string)

(defgmethod
 (os+get-connected-midi-inputs :class 'os :bind "get_connected_midi_inputs"
  :hash 2981934095)
 packed-string-array)

(defgmethod
 (os+open-midi-inputs :class 'os :bind "open_midi_inputs" :hash 3218959716)
 :void)

(defgmethod
 (os+close-midi-inputs :class 'os :bind "close_midi_inputs" :hash 3218959716)
 :void)

(defgmethod (os+alert :class 'os :bind "alert" :hash 1783970740) :void
 (text string) (title string))

(defgmethod (os+crash :class 'os :bind "crash" :hash 83702148) :void
 (message string))

(defgmethod
 (os+set-low-processor-usage-mode :class 'os :bind
  "set_low_processor_usage_mode" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (os+is-in-low-processor-usage-mode :class 'os :bind
  "is_in_low_processor_usage_mode" :hash 36873697)
 bool)

(defgmethod
 (os+set-low-processor-usage-mode-sleep-usec :class 'os :bind
  "set_low_processor_usage_mode_sleep_usec" :hash 1286410249)
 :void (usec int))

(defgmethod
 (os+get-low-processor-usage-mode-sleep-usec :class 'os :bind
  "get_low_processor_usage_mode_sleep_usec" :hash 3905245786)
 int)

(defgmethod
 (os+set-delta-smoothing :class 'os :bind "set_delta_smoothing" :hash
  2586408642)
 :void (delta-smoothing-enabled bool))

(defgmethod
 (os+is-delta-smoothing-enabled :class 'os :bind "is_delta_smoothing_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (os+get-processor-count :class 'os :bind "get_processor_count" :hash
  3905245786)
 int)

(defgmethod
 (os+get-processor-name :class 'os :bind "get_processor_name" :hash 201670096)
 string)

(defgmethod
 (os+get-system-fonts :class 'os :bind "get_system_fonts" :hash 1139954409)
 packed-string-array)

(defgmethod
 (os+get-system-font-path :class 'os :bind "get_system_font_path" :hash
  626580860)
 string (font-name string) (weight int) (stretch int) (italic bool))

(defgmethod
 (os+get-system-font-path-for-text :class 'os :bind
  "get_system_font_path_for_text" :hash 197317981)
 packed-string-array (font-name string) (text string) (locale string)
 (script string) (weight int) (stretch int) (italic bool))

(defgmethod
 (os+get-executable-path :class 'os :bind "get_executable_path" :hash
  201670096)
 string)

(defgmethod
 (os+read-string-from-stdin :class 'os :bind "read_string_from_stdin" :hash
  723587915)
 string (buffer-size int))

(defgmethod
 (os+read-buffer-from-stdin :class 'os :bind "read_buffer_from_stdin" :hash
  3249455752)
 packed-byte-array (buffer-size int))

(defgmethod
 (os+get-stdin-type :class 'os :bind "get_stdin_type" :hash 1704816237)
 os+std-handle-type)

(defgmethod
 (os+get-stdout-type :class 'os :bind "get_stdout_type" :hash 1704816237)
 os+std-handle-type)

(defgmethod
 (os+get-stderr-type :class 'os :bind "get_stderr_type" :hash 1704816237)
 os+std-handle-type)

(defgmethod (os+execute :class 'os :bind "execute" :hash 1488299882) int
 (path string) (arguments packed-string-array) (output array)
 (read-stderr bool) (open-console bool))

(defgmethod
 (os+execute-with-pipe :class 'os :bind "execute_with_pipe" :hash 2851312030)
 dictionary (path string) (arguments packed-string-array) (blocking bool))

(defgmethod
 (os+create-process :class 'os :bind "create_process" :hash 2903767230) int
 (path string) (arguments packed-string-array) (open-console bool))

(defgmethod
 (os+create-instance :class 'os :bind "create_instance" :hash 1080601263) int
 (arguments packed-string-array))

(defgmethod
 (os+open-with-program :class 'os :bind "open_with_program" :hash 2848259907)
 error (program-path string) (paths packed-string-array))

(defgmethod (os+kill :class 'os :bind "kill" :hash 844576869) error (pid int))

(defgmethod (os+shell-open :class 'os :bind "shell_open" :hash 166001499) error
 (uri string))

(defgmethod
 (os+shell-show-in-file-manager :class 'os :bind "shell_show_in_file_manager"
  :hash 3565188097)
 error (file-or-dir-path string) (open-folder bool))

(defgmethod
 (os+is-process-running :class 'os :bind "is_process_running" :hash 1116898809)
 bool (pid int))

(defgmethod
 (os+get-process-exit-code :class 'os :bind "get_process_exit_code" :hash
  923996154)
 int (pid int))

(defgmethod
 (os+get-process-id :class 'os :bind "get_process_id" :hash 3905245786) int)

(defgmethod
 (os+has-environment :class 'os :bind "has_environment" :hash 3927539163) bool
 (variable string))

(defgmethod
 (os+get-environment :class 'os :bind "get_environment" :hash 3135753539)
 string (variable string))

(defgmethod
 (os+set-environment :class 'os :bind "set_environment" :hash 3605043004) :void
 (variable string) (value string))

(defgmethod
 (os+unset-environment :class 'os :bind "unset_environment" :hash 3089850668)
 :void (variable string))

(defgmethod (os+get-name :class 'os :bind "get_name" :hash 201670096) string)

(defgmethod
 (os+get-distribution-name :class 'os :bind "get_distribution_name" :hash
  201670096)
 string)

(defgmethod (os+get-version :class 'os :bind "get_version" :hash 201670096)
 string)

(defgmethod
 (os+get-version-alias :class 'os :bind "get_version_alias" :hash 201670096)
 string)

(defgmethod
 (os+get-cmdline-args :class 'os :bind "get_cmdline_args" :hash 2981934095)
 packed-string-array)

(defgmethod
 (os+get-cmdline-user-args :class 'os :bind "get_cmdline_user_args" :hash
  2981934095)
 packed-string-array)

(defgmethod
 (os+get-video-adapter-driver-info :class 'os :bind
  "get_video_adapter_driver_info" :hash 1139954409)
 packed-string-array)

(defgmethod
 (os+set-restart-on-exit :class 'os :bind "set_restart_on_exit" :hash
  3331453935)
 :void (restart bool) (arguments packed-string-array))

(defgmethod
 (os+is-restart-on-exit-set :class 'os :bind "is_restart_on_exit_set" :hash
  36873697)
 bool)

(defgmethod
 (os+get-restart-on-exit-arguments :class 'os :bind
  "get_restart_on_exit_arguments" :hash 1139954409)
 packed-string-array)

(defgmethod (os+delay-usec :class 'os :bind "delay_usec" :hash 998575451) :void
 (usec int))

(defgmethod (os+delay-msec :class 'os :bind "delay_msec" :hash 998575451) :void
 (msec int))

(defgmethod (os+get-locale :class 'os :bind "get_locale" :hash 201670096)
 string)

(defgmethod
 (os+get-locale-language :class 'os :bind "get_locale_language" :hash
  201670096)
 string)

(defgmethod
 (os+get-model-name :class 'os :bind "get_model_name" :hash 201670096) string)

(defgmethod
 (os+is-userfs-persistent :class 'os :bind "is_userfs_persistent" :hash
  36873697)
 bool)

(defgmethod
 (os+is-stdout-verbose :class 'os :bind "is_stdout_verbose" :hash 36873697)
 bool)

(defgmethod
 (os+is-debug-build :class 'os :bind "is_debug_build" :hash 36873697) bool)

(defgmethod
 (os+get-static-memory-usage :class 'os :bind "get_static_memory_usage" :hash
  3905245786)
 int)

(defgmethod
 (os+get-static-memory-peak-usage :class 'os :bind
  "get_static_memory_peak_usage" :hash 3905245786)
 int)

(defgmethod
 (os+get-memory-info :class 'os :bind "get_memory_info" :hash 3102165223)
 dictionary)

(defgmethod
 (os+move-to-trash :class 'os :bind "move_to_trash" :hash 2113323047) error
 (path string))

(defgmethod
 (os+get-user-data-dir :class 'os :bind "get_user_data_dir" :hash 201670096)
 string)

(defgmethod
 (os+get-system-dir :class 'os :bind "get_system_dir" :hash 3073895123) string
 (dir os+system-dir) (shared-storage bool))

(defgmethod
 (os+get-config-dir :class 'os :bind "get_config_dir" :hash 201670096) string)

(defgmethod (os+get-data-dir :class 'os :bind "get_data_dir" :hash 201670096)
 string)

(defgmethod (os+get-cache-dir :class 'os :bind "get_cache_dir" :hash 201670096)
 string)

(defgmethod (os+get-temp-dir :class 'os :bind "get_temp_dir" :hash 201670096)
 string)

(defgmethod (os+get-unique-id :class 'os :bind "get_unique_id" :hash 201670096)
 string)

(defgmethod
 (os+get-keycode-string :class 'os :bind "get_keycode_string" :hash 2261993717)
 string (code key))

(defgmethod
 (os+is-keycode-unicode :class 'os :bind "is_keycode_unicode" :hash 1116898809)
 bool (code int))

(defgmethod
 (os+find-keycode-from-string :class 'os :bind "find_keycode_from_string" :hash
  1084858572)
 key (string string))

(defgmethod
 (os+set-use-file-access-save-and-swap :class 'os :bind
  "set_use_file_access_save_and_swap" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (os+set-thread-name :class 'os :bind "set_thread_name" :hash 166001499) error
 (name string))

(defgmethod
 (os+get-thread-caller-id :class 'os :bind "get_thread_caller_id" :hash
  3905245786)
 int)

(defgmethod
 (os+get-main-thread-id :class 'os :bind "get_main_thread_id" :hash 3905245786)
 int)

(defgmethod (os+has-feature :class 'os :bind "has_feature" :hash 3927539163)
 bool (tag-name string))

(defgmethod (os+is-sandboxed :class 'os :bind "is_sandboxed" :hash 36873697)
 bool)

(defgmethod
 (os+request-permission :class 'os :bind "request_permission" :hash 2323990056)
 bool (name string))

(defgmethod
 (os+request-permissions :class 'os :bind "request_permissions" :hash
  2240911060)
 bool)

(defgmethod
 (os+get-granted-permissions :class 'os :bind "get_granted_permissions" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (os+revoke-granted-permissions :class 'os :bind "revoke_granted_permissions"
  :hash 3218959716)
 :void)

(defgmethod (os+add-logger :class 'os :bind "add_logger" :hash 4261188958)
 :void (logger logger))

(defgmethod
 (os+remove-logger :class 'os :bind "remove_logger" :hash 4261188958) :void
 (logger logger))