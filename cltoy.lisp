(in-package :cltoy)

(defparameter *douban-user-api* "http://api.douban.com/shuo/v2/users/")
(defparameter *douban-book-api* "http://api.douban.com/v2/book/user/")
(defparameter *people* (make-hash-table :test 'equal))
(defparameter *common-db* nil)

(defun fetch-json (api uid request-type start count)
  "Fetch json string from douban."
  (labels ((request (url id type s cnt)
             (concatenate 'string
                          url id "/" type
                          "?start=" (write-to-string s)
                          "&count=" (write-to-string cnt))))
    (json:decode-json-from-string
     (babel:octets-to-string
      (drakma:http-request (request api uid request-type start count))))))

(defun fetch-following (uid &optional (start 0) (count 100))
  "Fetch user's following from douban."
  (fetch-json *douban-user-api* uid "following" start count))

(defun fetch-followers (uid &optional (start 0) (count 100))
  "Fetch user's followers from douban"
  (fetch-json *douban-user-api* uid "followers" start count))

(defun fetch-book-collection (uid &optional (start 0) (count 100))
  "Fetch user's book collection from douban."
  (fetch-json *douban-book-api* uid "collections" start count))

(defun add-to-people (type item)
  "If the item is a user type, then add to gobal variable *people*"
  (if (string= type "user")
      (setf
       (gethash (get-value-by-tag :screen--name item) *people*)
       (get-value-by-tag :uid item))))

(defmacro json-handle (key json-list &body body)
  `(mapcar #'(lambda (var)
               (let ((value (get-value-by-tag ,key var)))
                 ,@body)) 
           ,json-list))

;; macroexpand: get-people 
;;
;; (defun get-people (uid)
;;   (labels ((collect (s)
;;              (mapcar #'(lambda (x)
;;                          (let ((type (get-value-by-tag :type x)))
;;                            (funcall 'add-to-people type x)))
;;                      s)))
;;     (progn
;;       (collect (fetch-following uid))
;;       (collect (fetch-followers uid))
;;       *people*)))

(defun get-people (uid)
  "Find user's following and followers from douban, and add them to
the global variable *people*"
  (labels ((collect (s)
             (json-handle :type s
                 (add-to-people value var))))
    (progn
      ;;(collect (fetch-following uid))
      (collect (fetch-followers uid))
      *people*)))

(defun get-value-by-tag (tag s)
  "Get the value by tag from json string."
  (cdr (assoc tag s)))

;;macroexpand: get-books 
;;
;; (defun get-books (json-string)
;;   "Get books info from json string."
;;   (let ((s (get-value-by-tag :collections json-string)))
;;     (mapcar #'(lambda (x)
;;                 (let ((value (get-value-by-tag :book x)))
;;                   (get-value-by-tag :title value)))
;;             s)))

(defun get-books (json-string)
  "Get books info from json string."
  (let ((s (get-value-by-tag :collections json-string)))
    (json-handle :book s
      (get-value-by-tag :title value))))

(defun get-book-collection (uid)
  "Get user's book collection from douban."
  (do* ((count 300)
        (start 0 (+ start count))
        (collection nil)
        (books "book")
        (str nil))
       ((null books) collection)
    (progn
      (setq str (fetch-book-collection uid start ))
      (setq books (get-books str))
      (setq collection (nconc collection books)))))

(defclass common-item ()
  ((user :accessor common-item-user
         :initarg :user)
   (num :accessor common-item-num
        :initarg :num)
   (list :accessor common-item-list
         :initarg :list)))

(defun get-common-books (c1 c2)
  "Get the common books in two book collections."
  (intersection c1 c2 :test #'string-equal))

(defun make-common-item (user common)
  "Make common books item."
  (make-instance 'common-item
                 :user user
                 :num (length common)
                 :list common))


(defun find-common-books (uid people)
  "Find common books between people."
  (let ((c1 (get-book-collection uid)))
    (maphash #'(lambda (user id)
                 (let* ((c2 (get-book-collection id))
                        (common (get-common-books c1 c2))
                        (item (make-common-item user common)))
                   (format t "~A: ~A books in common.~%" user (length common))
                   (push item *common-db*)))
             people)
    *common-db*))

(defun print-people ()
  (maphash #'(lambda (user uri)
               (format t "~a ~a~%" user uri))
           *people*))

(defgeneric htmlize-data (data)
  (:documentation "Print lisp into html."))

(defmethod htmlize-data ((data common-item))
  (with-output-to-string (s)
    (format s "<tr>")
    (format s "<td>~A</td>" (common-item-user data))
    (format s "<td>~A</td>" (common-item-num data))
    (format s "<td>~{&lt~A&gt~^, ~}</td>" (common-item-list data))
    (format s "</tr>~%")))
 
(defun gen-html-file (db)
  "Print the data in html file."
  (with-open-file (s "output.htm" :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (progn
      (format s "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />")
      (format s "<html>~%<body>~%<table border>~%")
      (format s "<tr><th>Name</th><th>Num</th><th>Books</th>~%")
      (dolist (v db)
        (format s "~A" (htmlize-data v)))
      (format s "</body>~%</html>~%"))))
