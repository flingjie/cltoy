学习使用lisp获取豆瓣信息

* * * * *

#### 接口说明
*     fetch-following(uid &optional (start 0) (count 100))  
  获取用户following的豆友。  
  
*     fetch-followers(uid &optional (start 0) (count 100))  
  获取用户的followers。  
  
*     fetch-book-collection(uid &optional (start 0) (count 100))  
  获取用户收藏的图书。  
  
*     get-people(uid)  
  获取用户所有的following和followers。  
  
*     get-book-collection(uid)  
  获取用户收藏的所有图书。  
  
*     find-common-books(uid people)  
  查找用户和别人收藏的相同的书籍。
  
  
* * * * *

#### Examples(在sbcl下使用）

*1.安装quicklisp*

a.下载quicklisp文件
  [下载地址](http://beta.quicklisp.org/quicklisp.lisp)
  
b.打开sbcl进行安装

    $ sbcl --load quickload.lisp
    * (quicklisp-quickstart:install)
    
c.设置每次启动时自动加载quicklisp
    
    * (ql:add-to-init-file)
    
d.将package放到~/quicklisp/local-projects/文件夹下后使用(ql:quickload 包名)即可加载。

*2.加载package*  
  ;使用quickload加载

    * (ql:quickload :douban)
    To load "douban":
    Load 1 ASDF system:
    douban
    ; Loading "douban"
    .......
    (:DOUBAN-READER)
    
  ;切换package
  
    * (in-package :douban)

    #<PACKAGE "DOUBAN">
    
