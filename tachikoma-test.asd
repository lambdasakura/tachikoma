#|
  This file is a part of migration project.
  Copyright (c) 2017 lambda_sakura
|#

(in-package :cl-user)
(defpackage tachikoma-test-asd
  (:use :cl :asdf))
(in-package :tachikoma-test-asd)

(defsystem tachikoma-test
  :author "lambda_sakura"
  :license "LLGPL"
  :depends-on (:tachikoma
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "tachikoma"))))
  :description "Test system for migration"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
