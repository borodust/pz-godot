(common-lisp:in-package :%godot)


(defgproperty udpserver+max-pending-connections 'udpserver :get
 'udpserver+get-max-pending-connections :set
 'udpserver+set-max-pending-connections)