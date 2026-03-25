(common-lisp:in-package :%godot)


(defgmethod
 (string-name+casecmp-to :class 'string-name :bind "casecmp_to" :hash
  2920860731)
 int (to string))

(defgmethod
 (string-name+nocasecmp-to :class 'string-name :bind "nocasecmp_to" :hash
  2920860731)
 int (to string))

(defgmethod
 (string-name+naturalcasecmp-to :class 'string-name :bind "naturalcasecmp_to"
  :hash 2920860731)
 int (to string))

(defgmethod
 (string-name+naturalnocasecmp-to :class 'string-name :bind
  "naturalnocasecmp_to" :hash 2920860731)
 int (to string))

(defgmethod
 (string-name+filecasecmp-to :class 'string-name :bind "filecasecmp_to" :hash
  2920860731)
 int (to string))

(defgmethod
 (string-name+filenocasecmp-to :class 'string-name :bind "filenocasecmp_to"
  :hash 2920860731)
 int (to string))

(defgmethod
 (string-name+length :class 'string-name :bind "length" :hash 3173160232) int)

(defgmethod
 (string-name+substr :class 'string-name :bind "substr" :hash 787537301) string
 (from int) (len int))

(defgmethod
 (string-name+get-slice :class 'string-name :bind "get_slice" :hash 3535100402)
 string (delimiter string) (slice int))

(defgmethod
 (string-name+get-slicec :class 'string-name :bind "get_slicec" :hash
  787537301)
 string (delimiter int) (slice int))

(defgmethod
 (string-name+get-slice-count :class 'string-name :bind "get_slice_count" :hash
  2920860731)
 int (delimiter string))

(defgmethod
 (string-name+find :class 'string-name :bind "find" :hash 1760645412) int
 (what string) (from int))

(defgmethod
 (string-name+findn :class 'string-name :bind "findn" :hash 1760645412) int
 (what string) (from int))

(defgmethod
 (string-name+count :class 'string-name :bind "count" :hash 2343087891) int
 (what string) (from int) (to int))

(defgmethod
 (string-name+countn :class 'string-name :bind "countn" :hash 2343087891) int
 (what string) (from int) (to int))

(defgmethod
 (string-name+rfind :class 'string-name :bind "rfind" :hash 1760645412) int
 (what string) (from int))

(defgmethod
 (string-name+rfindn :class 'string-name :bind "rfindn" :hash 1760645412) int
 (what string) (from int))

(defgmethod
 (string-name+match :class 'string-name :bind "match" :hash 2566493496) bool
 (expr string))

(defgmethod
 (string-name+matchn :class 'string-name :bind "matchn" :hash 2566493496) bool
 (expr string))

(defgmethod
 (string-name+begins-with :class 'string-name :bind "begins_with" :hash
  2566493496)
 bool (text string))

(defgmethod
 (string-name+ends-with :class 'string-name :bind "ends_with" :hash 2566493496)
 bool (text string))

(defgmethod
 (string-name+is-subsequence-of :class 'string-name :bind "is_subsequence_of"
  :hash 2566493496)
 bool (text string))

(defgmethod
 (string-name+is-subsequence-ofn :class 'string-name :bind "is_subsequence_ofn"
  :hash 2566493496)
 bool (text string))

(defgmethod
 (string-name+bigrams :class 'string-name :bind "bigrams" :hash 747180633)
 packed-string-array)

(defgmethod
 (string-name+similarity :class 'string-name :bind "similarity" :hash
  2697460964)
 float (text string))

(defgmethod
 (string-name+format :class 'string-name :bind "format" :hash 3212199029)
 string (values variant) (placeholder string))

(defgmethod
 (string-name+replace :class 'string-name :bind "replace" :hash 1340436205)
 string (what string) (forwhat string))

(defgmethod
 (string-name+replacen :class 'string-name :bind "replacen" :hash 1340436205)
 string (what string) (forwhat string))

(defgmethod
 (string-name+replace-char :class 'string-name :bind "replace_char" :hash
  787537301)
 string (key int) (with int))

(defgmethod
 (string-name+replace-chars :class 'string-name :bind "replace_chars" :hash
  3535100402)
 string (keys string) (with int))

(defgmethod
 (string-name+remove-char :class 'string-name :bind "remove_char" :hash
  2162347432)
 string (what int))

(defgmethod
 (string-name+remove-chars :class 'string-name :bind "remove_chars" :hash
  3134094431)
 string (chars string))

(defgmethod
 (string-name+repeat :class 'string-name :bind "repeat" :hash 2162347432)
 string (count int))

(defgmethod
 (string-name+reverse :class 'string-name :bind "reverse" :hash 3942272618)
 string)

(defgmethod
 (string-name+insert :class 'string-name :bind "insert" :hash 248737229) string
 (position int) (what string))

(defgmethod
 (string-name+erase :class 'string-name :bind "erase" :hash 787537301) string
 (position int) (chars int))

(defgmethod
 (string-name+capitalize :class 'string-name :bind "capitalize" :hash
  3942272618)
 string)

(defgmethod
 (string-name+to-camel-case :class 'string-name :bind "to_camel_case" :hash
  3942272618)
 string)

(defgmethod
 (string-name+to-pascal-case :class 'string-name :bind "to_pascal_case" :hash
  3942272618)
 string)

(defgmethod
 (string-name+to-snake-case :class 'string-name :bind "to_snake_case" :hash
  3942272618)
 string)

(defgmethod
 (string-name+to-kebab-case :class 'string-name :bind "to_kebab_case" :hash
  3942272618)
 string)

(defgmethod
 (string-name+split :class 'string-name :bind "split" :hash 1252735785)
 packed-string-array (delimiter string) (allow-empty bool) (maxsplit int))

(defgmethod
 (string-name+rsplit :class 'string-name :bind "rsplit" :hash 1252735785)
 packed-string-array (delimiter string) (allow-empty bool) (maxsplit int))

(defgmethod
 (string-name+split-floats :class 'string-name :bind "split_floats" :hash
  2092079095)
 packed-float-64array (delimiter string) (allow-empty bool))

(defgmethod
 (string-name+join :class 'string-name :bind "join" :hash 3595973238) string
 (parts packed-string-array))

(defgmethod
 (string-name+to-upper :class 'string-name :bind "to_upper" :hash 3942272618)
 string)

(defgmethod
 (string-name+to-lower :class 'string-name :bind "to_lower" :hash 3942272618)
 string)

(defgmethod
 (string-name+left :class 'string-name :bind "left" :hash 2162347432) string
 (length int))

(defgmethod
 (string-name+right :class 'string-name :bind "right" :hash 2162347432) string
 (length int))

(defgmethod
 (string-name+strip-edges :class 'string-name :bind "strip_edges" :hash
  907855311)
 string (left bool) (right bool))

(defgmethod
 (string-name+strip-escapes :class 'string-name :bind "strip_escapes" :hash
  3942272618)
 string)

(defgmethod
 (string-name+lstrip :class 'string-name :bind "lstrip" :hash 3134094431)
 string (chars string))

(defgmethod
 (string-name+rstrip :class 'string-name :bind "rstrip" :hash 3134094431)
 string (chars string))

(defgmethod
 (string-name+get-extension :class 'string-name :bind "get_extension" :hash
  3942272618)
 string)

(defgmethod
 (string-name+get-basename :class 'string-name :bind "get_basename" :hash
  3942272618)
 string)

(defgmethod
 (string-name+path-join :class 'string-name :bind "path_join" :hash 3134094431)
 string (path string))

(defgmethod
 (string-name+unicode-at :class 'string-name :bind "unicode_at" :hash
  4103005248)
 int (at int))

(defgmethod
 (string-name+indent :class 'string-name :bind "indent" :hash 3134094431)
 string (prefix string))

(defgmethod
 (string-name+dedent :class 'string-name :bind "dedent" :hash 3942272618)
 string)

(defgmethod
 (string-name+md5-text :class 'string-name :bind "md5_text" :hash 3942272618)
 string)

(defgmethod
 (string-name+sha1-text :class 'string-name :bind "sha1_text" :hash 3942272618)
 string)

(defgmethod
 (string-name+sha256-text :class 'string-name :bind "sha256_text" :hash
  3942272618)
 string)

(defgmethod
 (string-name+md5-buffer :class 'string-name :bind "md5_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string-name+sha1-buffer :class 'string-name :bind "sha1_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string-name+sha256-buffer :class 'string-name :bind "sha256_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string-name+is-empty :class 'string-name :bind "is_empty" :hash 3918633141)
 bool)

(defgmethod
 (string-name+contains :class 'string-name :bind "contains" :hash 2566493496)
 bool (what string))

(defgmethod
 (string-name+containsn :class 'string-name :bind "containsn" :hash 2566493496)
 bool (what string))

(defgmethod
 (string-name+is-absolute-path :class 'string-name :bind "is_absolute_path"
  :hash 3918633141)
 bool)

(defgmethod
 (string-name+is-relative-path :class 'string-name :bind "is_relative_path"
  :hash 3918633141)
 bool)

(defgmethod
 (string-name+simplify-path :class 'string-name :bind "simplify_path" :hash
  3942272618)
 string)

(defgmethod
 (string-name+get-base-dir :class 'string-name :bind "get_base_dir" :hash
  3942272618)
 string)

(defgmethod
 (string-name+get-file :class 'string-name :bind "get_file" :hash 3942272618)
 string)

(defgmethod
 (string-name+xml-escape :class 'string-name :bind "xml_escape" :hash
  3429816538)
 string (escape-quotes bool))

(defgmethod
 (string-name+xml-unescape :class 'string-name :bind "xml_unescape" :hash
  3942272618)
 string)

(defgmethod
 (string-name+uri-encode :class 'string-name :bind "uri_encode" :hash
  3942272618)
 string)

(defgmethod
 (string-name+uri-decode :class 'string-name :bind "uri_decode" :hash
  3942272618)
 string)

(defgmethod
 (string-name+uri-file-decode :class 'string-name :bind "uri_file_decode" :hash
  3942272618)
 string)

(defgmethod
 (string-name+c-escape :class 'string-name :bind "c_escape" :hash 3942272618)
 string)

(defgmethod
 (string-name+c-unescape :class 'string-name :bind "c_unescape" :hash
  3942272618)
 string)

(defgmethod
 (string-name+json-escape :class 'string-name :bind "json_escape" :hash
  3942272618)
 string)

(defgmethod
 (string-name+validate-node-name :class 'string-name :bind "validate_node_name"
  :hash 3942272618)
 string)

(defgmethod
 (string-name+validate-filename :class 'string-name :bind "validate_filename"
  :hash 3942272618)
 string)

(defgmethod
 (string-name+is-valid-ascii-identifier :class 'string-name :bind
  "is_valid_ascii_identifier" :hash 3918633141)
 bool)

(defgmethod
 (string-name+is-valid-unicode-identifier :class 'string-name :bind
  "is_valid_unicode_identifier" :hash 3918633141)
 bool)

(defgmethod
 (string-name+is-valid-identifier :class 'string-name :bind
  "is_valid_identifier" :hash 3918633141)
 bool)

(defgmethod
 (string-name+is-valid-int :class 'string-name :bind "is_valid_int" :hash
  3918633141)
 bool)

(defgmethod
 (string-name+is-valid-float :class 'string-name :bind "is_valid_float" :hash
  3918633141)
 bool)

(defgmethod
 (string-name+is-valid-hex-number :class 'string-name :bind
  "is_valid_hex_number" :hash 593672999)
 bool (with-prefix bool))

(defgmethod
 (string-name+is-valid-html-color :class 'string-name :bind
  "is_valid_html_color" :hash 3918633141)
 bool)

(defgmethod
 (string-name+is-valid-ip-address :class 'string-name :bind
  "is_valid_ip_address" :hash 3918633141)
 bool)

(defgmethod
 (string-name+is-valid-filename :class 'string-name :bind "is_valid_filename"
  :hash 3918633141)
 bool)

(defgmethod
 (string-name+to-int :class 'string-name :bind "to_int" :hash 3173160232) int)

(defgmethod
 (string-name+to-float :class 'string-name :bind "to_float" :hash 466405837)
 float)

(defgmethod
 (string-name+hex-to-int :class 'string-name :bind "hex_to_int" :hash
  3173160232)
 int)

(defgmethod
 (string-name+bin-to-int :class 'string-name :bind "bin_to_int" :hash
  3173160232)
 int)

(defgmethod (string-name+lpad :class 'string-name :bind "lpad" :hash 248737229)
 string (min-length int) (character string))

(defgmethod (string-name+rpad :class 'string-name :bind "rpad" :hash 248737229)
 string (min-length int) (character string))

(defgmethod
 (string-name+pad-decimals :class 'string-name :bind "pad_decimals" :hash
  2162347432)
 string (digits int))

(defgmethod
 (string-name+pad-zeros :class 'string-name :bind "pad_zeros" :hash 2162347432)
 string (digits int))

(defgmethod
 (string-name+trim-prefix :class 'string-name :bind "trim_prefix" :hash
  3134094431)
 string (prefix string))

(defgmethod
 (string-name+trim-suffix :class 'string-name :bind "trim_suffix" :hash
  3134094431)
 string (suffix string))

(defgmethod
 (string-name+to-ascii-buffer :class 'string-name :bind "to_ascii_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string-name+to-utf8-buffer :class 'string-name :bind "to_utf8_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string-name+to-utf16-buffer :class 'string-name :bind "to_utf16_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string-name+to-utf32-buffer :class 'string-name :bind "to_utf32_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string-name+to-wchar-buffer :class 'string-name :bind "to_wchar_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string-name+to-multibyte-char-buffer :class 'string-name :bind
  "to_multibyte_char_buffer" :hash 3055765187)
 packed-byte-array (encoding string))

(defgmethod
 (string-name+hex-decode :class 'string-name :bind "hex_decode" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string-name+hash :class 'string-name :bind "hash" :hash 3173160232) int)