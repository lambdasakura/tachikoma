(defpackage :tachikoma.config
  (:use :cl
        :envy))
(in-package :tachikoma.config)

(setf (config-env-var) "APP_ENV")

(defparameter test-database
  (asdf:system-relative-pathname
   :tachikoma #P"t/test-database.db"))

(defconfig |development|
  `(:debug T
    :database-type :sqlite3
    :database-connection-spec (:database-name ,test-database)))

(defconfig |production|
  '(:database-type :mysql
    :database-connection-spec (:database-name "test"
                               :usename "whoami"
                               :password "1234")))

(defconfig |staging|
  `(:debug T
    ,@|production|))
