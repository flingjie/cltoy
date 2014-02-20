(defpackage :cltoy
  (:use :cl
        :drakma
        :cl-json
        :cl-ppcre
        :babel)
  (:export
   fetch-following
   fetch-followers
   fetch-book-collection
   get-people
   get-book-collection
   find-common-books))
