#|
  This file is a part of migration project.
  Copyright (c) 2017 lambda_sakura
|#

#|
  migration tool

  Author: lambda_sakura
|#

(in-package :cl-user)
(defpackage tachikoma-asd
  (:use :cl :asdf))
(in-package :tachikoma-asd)

(defsystem tachikoma
  :version "0.1"
  :author "lambda_sakura"
  :license "LLGPL"
  :depends-on (:cl-dbi :cl-ppcre)
  :components ((:module "src"
                :serial t
                :components
                ((:file "migration")
                 (:file "tachikoma"))))
  :description "migration tool"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op tachikoma-test))))
