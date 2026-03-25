(common-lisp:in-package :%godot)


(defgmethod
 (worker-thread-pool+add-task :class 'worker-thread-pool :bind "add_task" :hash
  3745067146)
 int (action callable) (high-priority bool) (description string))

(defgmethod
 (worker-thread-pool+is-task-completed :class 'worker-thread-pool :bind
  "is_task_completed" :hash 1116898809)
 bool (task-id int))

(defgmethod
 (worker-thread-pool+wait-for-task-completion :class 'worker-thread-pool :bind
  "wait_for_task_completion" :hash 844576869)
 error (task-id int))

(defgmethod
 (worker-thread-pool+get-caller-task-id :class 'worker-thread-pool :bind
  "get_caller_task_id" :hash 3905245786)
 int)

(defgmethod
 (worker-thread-pool+add-group-task :class 'worker-thread-pool :bind
  "add_group_task" :hash 1801953219)
 int (action callable) (elements int) (tasks-needed int) (high-priority bool)
 (description string))

(defgmethod
 (worker-thread-pool+is-group-task-completed :class 'worker-thread-pool :bind
  "is_group_task_completed" :hash 1116898809)
 bool (group-id int))

(defgmethod
 (worker-thread-pool+get-group-processed-element-count :class
  'worker-thread-pool :bind "get_group_processed_element_count" :hash
  923996154)
 int (group-id int))

(defgmethod
 (worker-thread-pool+wait-for-group-task-completion :class 'worker-thread-pool
  :bind "wait_for_group_task_completion" :hash 1286410249)
 :void (group-id int))

(defgmethod
 (worker-thread-pool+get-caller-group-id :class 'worker-thread-pool :bind
  "get_caller_group_id" :hash 3905245786)
 int)