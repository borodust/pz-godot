(common-lisp:in-package :%godot)


(defgmethod
 (time+get-datetime-dict-from-unix-time :class 'time :bind
  "get_datetime_dict_from_unix_time" :hash 3485342025)
 dictionary (unix-time-val int))

(defgmethod
 (time+get-date-dict-from-unix-time :class 'time :bind
  "get_date_dict_from_unix_time" :hash 3485342025)
 dictionary (unix-time-val int))

(defgmethod
 (time+get-time-dict-from-unix-time :class 'time :bind
  "get_time_dict_from_unix_time" :hash 3485342025)
 dictionary (unix-time-val int))

(defgmethod
 (time+get-datetime-string-from-unix-time :class 'time :bind
  "get_datetime_string_from_unix_time" :hash 2311239925)
 string (unix-time-val int) (use-space bool))

(defgmethod
 (time+get-date-string-from-unix-time :class 'time :bind
  "get_date_string_from_unix_time" :hash 844755477)
 string (unix-time-val int))

(defgmethod
 (time+get-time-string-from-unix-time :class 'time :bind
  "get_time_string_from_unix_time" :hash 844755477)
 string (unix-time-val int))

(defgmethod
 (time+get-datetime-dict-from-datetime-string :class 'time :bind
  "get_datetime_dict_from_datetime_string" :hash 3253569256)
 dictionary (datetime string) (weekday bool))

(defgmethod
 (time+get-datetime-string-from-datetime-dict :class 'time :bind
  "get_datetime_string_from_datetime_dict" :hash 1898123706)
 string (datetime dictionary) (use-space bool))

(defgmethod
 (time+get-unix-time-from-datetime-dict :class 'time :bind
  "get_unix_time_from_datetime_dict" :hash 3021115443)
 int (datetime dictionary))

(defgmethod
 (time+get-unix-time-from-datetime-string :class 'time :bind
  "get_unix_time_from_datetime_string" :hash 1321353865)
 int (datetime string))

(defgmethod
 (time+get-offset-string-from-offset-minutes :class 'time :bind
  "get_offset_string_from_offset_minutes" :hash 844755477)
 string (offset-minutes int))

(defgmethod
 (time+get-datetime-dict-from-system :class 'time :bind
  "get_datetime_dict_from_system" :hash 205769976)
 dictionary (utc bool))

(defgmethod
 (time+get-date-dict-from-system :class 'time :bind "get_date_dict_from_system"
  :hash 205769976)
 dictionary (utc bool))

(defgmethod
 (time+get-time-dict-from-system :class 'time :bind "get_time_dict_from_system"
  :hash 205769976)
 dictionary (utc bool))

(defgmethod
 (time+get-datetime-string-from-system :class 'time :bind
  "get_datetime_string_from_system" :hash 1136425492)
 string (utc bool) (use-space bool))

(defgmethod
 (time+get-date-string-from-system :class 'time :bind
  "get_date_string_from_system" :hash 1162154673)
 string (utc bool))

(defgmethod
 (time+get-time-string-from-system :class 'time :bind
  "get_time_string_from_system" :hash 1162154673)
 string (utc bool))

(defgmethod
 (time+get-time-zone-from-system :class 'time :bind "get_time_zone_from_system"
  :hash 3102165223)
 dictionary)

(defgmethod
 (time+get-unix-time-from-system :class 'time :bind "get_unix_time_from_system"
  :hash 1740695150)
 float)

(defgmethod
 (time+get-ticks-msec :class 'time :bind "get_ticks_msec" :hash 3905245786) int)

(defgmethod
 (time+get-ticks-usec :class 'time :bind "get_ticks_usec" :hash 3905245786) int)