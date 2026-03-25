(common-lisp:in-package :%godot)


(defgmethod
 (string+casecmp-to :class 'string :bind "casecmp_to" :hash 2920860731) int
 (to string))

(defgmethod
 (string+nocasecmp-to :class 'string :bind "nocasecmp_to" :hash 2920860731) int
 (to string))

(defgmethod
 (string+naturalcasecmp-to :class 'string :bind "naturalcasecmp_to" :hash
  2920860731)
 int (to string))

(defgmethod
 (string+naturalnocasecmp-to :class 'string :bind "naturalnocasecmp_to" :hash
  2920860731)
 int (to string))

(defgmethod
 (string+filecasecmp-to :class 'string :bind "filecasecmp_to" :hash 2920860731)
 int (to string))

(defgmethod
 (string+filenocasecmp-to :class 'string :bind "filenocasecmp_to" :hash
  2920860731)
 int (to string))

(defgmethod (string+length :class 'string :bind "length" :hash 3173160232) int)

(defgmethod (string+substr :class 'string :bind "substr" :hash 787537301)
 string (from int) (len int))

(defgmethod
 (string+get-slice :class 'string :bind "get_slice" :hash 3535100402) string
 (delimiter string) (slice int))

(defgmethod
 (string+get-slicec :class 'string :bind "get_slicec" :hash 787537301) string
 (delimiter int) (slice int))

(defgmethod
 (string+get-slice-count :class 'string :bind "get_slice_count" :hash
  2920860731)
 int (delimiter string))

(defgmethod (string+find :class 'string :bind "find" :hash 1760645412) int
 (what string) (from int))

(defgmethod (string+findn :class 'string :bind "findn" :hash 1760645412) int
 (what string) (from int))

(defgmethod (string+count :class 'string :bind "count" :hash 2343087891) int
 (what string) (from int) (to int))

(defgmethod (string+countn :class 'string :bind "countn" :hash 2343087891) int
 (what string) (from int) (to int))

(defgmethod (string+rfind :class 'string :bind "rfind" :hash 1760645412) int
 (what string) (from int))

(defgmethod (string+rfindn :class 'string :bind "rfindn" :hash 1760645412) int
 (what string) (from int))

(defgmethod (string+match :class 'string :bind "match" :hash 2566493496) bool
 (expr string))

(defgmethod (string+matchn :class 'string :bind "matchn" :hash 2566493496) bool
 (expr string))

(defgmethod
 (string+begins-with :class 'string :bind "begins_with" :hash 2566493496) bool
 (text string))

(defgmethod
 (string+ends-with :class 'string :bind "ends_with" :hash 2566493496) bool
 (text string))

(defgmethod
 (string+is-subsequence-of :class 'string :bind "is_subsequence_of" :hash
  2566493496)
 bool (text string))

(defgmethod
 (string+is-subsequence-ofn :class 'string :bind "is_subsequence_ofn" :hash
  2566493496)
 bool (text string))

(defgmethod (string+bigrams :class 'string :bind "bigrams" :hash 747180633)
 packed-string-array)

(defgmethod
 (string+similarity :class 'string :bind "similarity" :hash 2697460964) float
 (text string))

(defgmethod (string+format :class 'string :bind "format" :hash 3212199029)
 string (values variant) (placeholder string))

(defgmethod (string+replace :class 'string :bind "replace" :hash 1340436205)
 string (what string) (forwhat string))

(defgmethod (string+replacen :class 'string :bind "replacen" :hash 1340436205)
 string (what string) (forwhat string))

(defgmethod
 (string+replace-char :class 'string :bind "replace_char" :hash 787537301)
 string (key int) (with int))

(defgmethod
 (string+replace-chars :class 'string :bind "replace_chars" :hash 3535100402)
 string (keys string) (with int))

(defgmethod
 (string+remove-char :class 'string :bind "remove_char" :hash 2162347432)
 string (what int))

(defgmethod
 (string+remove-chars :class 'string :bind "remove_chars" :hash 3134094431)
 string (chars string))

(defgmethod (string+repeat :class 'string :bind "repeat" :hash 2162347432)
 string (count int))

(defgmethod (string+reverse :class 'string :bind "reverse" :hash 3942272618)
 string)

(defgmethod (string+insert :class 'string :bind "insert" :hash 248737229)
 string (position int) (what string))

(defgmethod (string+erase :class 'string :bind "erase" :hash 787537301) string
 (position int) (chars int))

(defgmethod
 (string+capitalize :class 'string :bind "capitalize" :hash 3942272618) string)

(defgmethod
 (string+to-camel-case :class 'string :bind "to_camel_case" :hash 3942272618)
 string)

(defgmethod
 (string+to-pascal-case :class 'string :bind "to_pascal_case" :hash 3942272618)
 string)

(defgmethod
 (string+to-snake-case :class 'string :bind "to_snake_case" :hash 3942272618)
 string)

(defgmethod
 (string+to-kebab-case :class 'string :bind "to_kebab_case" :hash 3942272618)
 string)

(defgmethod (string+split :class 'string :bind "split" :hash 1252735785)
 packed-string-array (delimiter string) (allow-empty bool) (maxsplit int))

(defgmethod (string+rsplit :class 'string :bind "rsplit" :hash 1252735785)
 packed-string-array (delimiter string) (allow-empty bool) (maxsplit int))

(defgmethod
 (string+split-floats :class 'string :bind "split_floats" :hash 2092079095)
 packed-float-64array (delimiter string) (allow-empty bool))

(defgmethod (string+join :class 'string :bind "join" :hash 3595973238) string
 (parts packed-string-array))

(defgmethod (string+to-upper :class 'string :bind "to_upper" :hash 3942272618)
 string)

(defgmethod (string+to-lower :class 'string :bind "to_lower" :hash 3942272618)
 string)

(defgmethod (string+left :class 'string :bind "left" :hash 2162347432) string
 (length int))

(defgmethod (string+right :class 'string :bind "right" :hash 2162347432) string
 (length int))

(defgmethod
 (string+strip-edges :class 'string :bind "strip_edges" :hash 907855311) string
 (left bool) (right bool))

(defgmethod
 (string+strip-escapes :class 'string :bind "strip_escapes" :hash 3942272618)
 string)

(defgmethod (string+lstrip :class 'string :bind "lstrip" :hash 3134094431)
 string (chars string))

(defgmethod (string+rstrip :class 'string :bind "rstrip" :hash 3134094431)
 string (chars string))

(defgmethod
 (string+get-extension :class 'string :bind "get_extension" :hash 3942272618)
 string)

(defgmethod
 (string+get-basename :class 'string :bind "get_basename" :hash 3942272618)
 string)

(defgmethod
 (string+path-join :class 'string :bind "path_join" :hash 3134094431) string
 (path string))

(defgmethod
 (string+unicode-at :class 'string :bind "unicode_at" :hash 4103005248) int
 (at int))

(defgmethod (string+indent :class 'string :bind "indent" :hash 3134094431)
 string (prefix string))

(defgmethod (string+dedent :class 'string :bind "dedent" :hash 3942272618)
 string)

(defgmethod (string+hash :class 'string :bind "hash" :hash 3173160232) int)

(defgmethod (string+md5-text :class 'string :bind "md5_text" :hash 3942272618)
 string)

(defgmethod
 (string+sha1-text :class 'string :bind "sha1_text" :hash 3942272618) string)

(defgmethod
 (string+sha256-text :class 'string :bind "sha256_text" :hash 3942272618)
 string)

(defgmethod
 (string+md5-buffer :class 'string :bind "md5_buffer" :hash 247621236)
 packed-byte-array)

(defgmethod
 (string+sha1-buffer :class 'string :bind "sha1_buffer" :hash 247621236)
 packed-byte-array)

(defgmethod
 (string+sha256-buffer :class 'string :bind "sha256_buffer" :hash 247621236)
 packed-byte-array)

(defgmethod (string+is-empty :class 'string :bind "is_empty" :hash 3918633141)
 bool)

(defgmethod (string+contains :class 'string :bind "contains" :hash 2566493496)
 bool (what string))

(defgmethod
 (string+containsn :class 'string :bind "containsn" :hash 2566493496) bool
 (what string))

(defgmethod
 (string+is-absolute-path :class 'string :bind "is_absolute_path" :hash
  3918633141)
 bool)

(defgmethod
 (string+is-relative-path :class 'string :bind "is_relative_path" :hash
  3918633141)
 bool)

(defgmethod
 (string+simplify-path :class 'string :bind "simplify_path" :hash 3942272618)
 string)

(defgmethod
 (string+get-base-dir :class 'string :bind "get_base_dir" :hash 3942272618)
 string)

(defgmethod (string+get-file :class 'string :bind "get_file" :hash 3942272618)
 string)

(defgmethod
 (string+xml-escape :class 'string :bind "xml_escape" :hash 3429816538) string
 (escape-quotes bool))

(defgmethod
 (string+xml-unescape :class 'string :bind "xml_unescape" :hash 3942272618)
 string)

(defgmethod
 (string+uri-encode :class 'string :bind "uri_encode" :hash 3942272618) string)

(defgmethod
 (string+uri-decode :class 'string :bind "uri_decode" :hash 3942272618) string)

(defgmethod
 (string+uri-file-decode :class 'string :bind "uri_file_decode" :hash
  3942272618)
 string)

(defgmethod (string+c-escape :class 'string :bind "c_escape" :hash 3942272618)
 string)

(defgmethod
 (string+c-unescape :class 'string :bind "c_unescape" :hash 3942272618) string)

(defgmethod
 (string+json-escape :class 'string :bind "json_escape" :hash 3942272618)
 string)

(defgmethod
 (string+validate-node-name :class 'string :bind "validate_node_name" :hash
  3942272618)
 string)

(defgmethod
 (string+validate-filename :class 'string :bind "validate_filename" :hash
  3942272618)
 string)

(defgmethod
 (string+is-valid-ascii-identifier :class 'string :bind
  "is_valid_ascii_identifier" :hash 3918633141)
 bool)

(defgmethod
 (string+is-valid-unicode-identifier :class 'string :bind
  "is_valid_unicode_identifier" :hash 3918633141)
 bool)

(defgmethod
 (string+is-valid-identifier :class 'string :bind "is_valid_identifier" :hash
  3918633141)
 bool)

(defgmethod
 (string+is-valid-int :class 'string :bind "is_valid_int" :hash 3918633141)
 bool)

(defgmethod
 (string+is-valid-float :class 'string :bind "is_valid_float" :hash 3918633141)
 bool)

(defgmethod
 (string+is-valid-hex-number :class 'string :bind "is_valid_hex_number" :hash
  593672999)
 bool (with-prefix bool))

(defgmethod
 (string+is-valid-html-color :class 'string :bind "is_valid_html_color" :hash
  3918633141)
 bool)

(defgmethod
 (string+is-valid-ip-address :class 'string :bind "is_valid_ip_address" :hash
  3918633141)
 bool)

(defgmethod
 (string+is-valid-filename :class 'string :bind "is_valid_filename" :hash
  3918633141)
 bool)

(defgmethod (string+to-int :class 'string :bind "to_int" :hash 3173160232) int)

(defgmethod (string+to-float :class 'string :bind "to_float" :hash 466405837)
 float)

(defgmethod
 (string+hex-to-int :class 'string :bind "hex_to_int" :hash 3173160232) int)

(defgmethod
 (string+bin-to-int :class 'string :bind "bin_to_int" :hash 3173160232) int)

(defgmethod (string+lpad :class 'string :bind "lpad" :hash 248737229) string
 (min-length int) (character string))

(defgmethod (string+rpad :class 'string :bind "rpad" :hash 248737229) string
 (min-length int) (character string))

(defgmethod
 (string+pad-decimals :class 'string :bind "pad_decimals" :hash 2162347432)
 string (digits int))

(defgmethod
 (string+pad-zeros :class 'string :bind "pad_zeros" :hash 2162347432) string
 (digits int))

(defgmethod
 (string+trim-prefix :class 'string :bind "trim_prefix" :hash 3134094431)
 string (prefix string))

(defgmethod
 (string+trim-suffix :class 'string :bind "trim_suffix" :hash 3134094431)
 string (suffix string))

(defgmethod
 (string+to-ascii-buffer :class 'string :bind "to_ascii_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string+to-utf8-buffer :class 'string :bind "to_utf8_buffer" :hash 247621236)
 packed-byte-array)

(defgmethod
 (string+to-utf16-buffer :class 'string :bind "to_utf16_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string+to-utf32-buffer :class 'string :bind "to_utf32_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string+to-wchar-buffer :class 'string :bind "to_wchar_buffer" :hash
  247621236)
 packed-byte-array)

(defgmethod
 (string+to-multibyte-char-buffer :class 'string :bind
  "to_multibyte_char_buffer" :hash 3055765187)
 packed-byte-array (encoding string))

(defgmethod
 (string+hex-decode :class 'string :bind "hex_decode" :hash 247621236)
 packed-byte-array)

(defgmethod
 (string+num-scientific :class 'string :bind "num_scientific" :hash 2710373411
  :static common-lisp:t)
 string (number float))

(defgmethod
 (string+num :class 'string :bind "num" :hash 1555901022 :static common-lisp:t)
 string (number float) (decimals int))

(defgmethod
 (string+num-int64 :class 'string :bind "num_int64" :hash 2111271071 :static
  common-lisp:t)
 string (number int) (base int) (capitalize-hex bool))

(defgmethod
 (string+num-uint64 :class 'string :bind "num_uint64" :hash 2111271071 :static
  common-lisp:t)
 string (number int) (base int) (capitalize-hex bool))

(defgmethod
 (string+chr :class 'string :bind "chr" :hash 897497541 :static common-lisp:t)
 string (code int))

(defgmethod
 (string+humanize-size :class 'string :bind "humanize_size" :hash 897497541
  :static common-lisp:t)
 string (size int))