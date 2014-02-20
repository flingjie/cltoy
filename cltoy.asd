(defpackage :cltoy-asd (:use :asdf :cl))

(in-package :cltoy-asd)

(defsystem cltoy
    :name "cltoy"
    :author "lingjie.fan <fanlingjie.cn@gmail.com>"
    :version "1.0"
    :description ""
    :components ((:file "packages")
                 (:file "cltoy" :depends-on ("packages")))
    :depends-on (:drakma
                 :cl-json
                 :cl-ppcre
                 :babel))
