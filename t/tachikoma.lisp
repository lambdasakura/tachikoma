(in-package :cl-user)
(defpackage tachikoma-test
  (:use :cl
        :tachikoma
        :prove))
(in-package :tachikoma-test)

(defparameter test-database
  (asdf:system-relative-pathname
   :tachikoma #P"t/test-database.db"))

(defparameter test-data-dir
  (asdf:system-relative-pathname
   :tachikoma #P"t/test-migration/"))

(defparameter option `(:sqlite3 :database-name ,test-database))

(plan 9)

(subtest "MigrationMeta Test"
         (uiop:delete-file-if-exists test-database)
         (initialize option)
         (add-migration option "0000-migration")
         (add-migration option "0001-migration")
         (add-migration option "0002-migration")
         (add-migration option "0003-migration")
         (is (length (meta-data-list option)) 4 "can add metadata")

         (delete-migration option "0002-migration")
         (delete-migration option "0003-migration")
         (is (length (meta-data-list option)) 2 "can remove metadata")
         (uiop:delete-file-if-exists test-database))

(subtest "find migration file test"
         (is (length (find-migration-files test-data-dir)) 2
             "can find migration files")
         (is (first (find-migration-files test-data-dir))
             (asdf:system-relative-pathname
              :tachikoma #P"t/test-migration/0000-sample.sql"))
         (is (length (load-migrations-from-directory test-data-dir '())) 2)
         (is (migration-name (first (load-migrations-from-directory test-data-dir '())))
             "0000-sample")
         (is (migration-name (second (load-migrations-from-directory test-data-dir '())))
             "0001-sample"))

(subtest "Load migration file test"
         (uiop:delete-file-if-exists test-database)
         (initialize option)
         (let ((migration-sample (load-migration-from-file
                                  (asdf:system-relative-pathname
                                   :tachikoma #P"t/test-migration/0000-sample.sql")
                                  '())))
           (is (migration-name migration-sample) "0000-sample")
           (diag "up migration")
           (execute-up-migration migration-sample option)
           (let ((connection (apply #'dbi:connect option)))
             (dbi:execute (dbi:prepare connection
                                       "INSERT INTO User (name) VALUES ('sample'), ('sample2');"))
             (is (length (dbi:fetch-all (dbi:prepare connection "SELECT * FROM USER;"))) 2)
             (dbi:disconnect connection))

           (diag "down migration")
           (execute-down-migration migration-sample option)
           (let ((connection (apply #'dbi:connect option)))
             (is-error (dbi:execute (dbi:prepare connection
                                                 "INSERT INTO User (name) VALUES ('sample');"))
                       'DBI.ERROR:<DBI-PROGRAMMING-ERROR>
                       "User table was deleted")
             (dbi:disconnect connection)))
         (uiop:delete-file-if-exists test-database))

(subtest "execute migration"
         (uiop:delete-file-if-exists test-database)
         (initialize option)
         (let ((migrations (load-migrations-from-directory test-data-dir (meta-data-list option))))
           (mapc #'(lambda (x)
                     (execute-up-migration x option)) migrations)

           (is (length (meta-data-list option)) 2)))

(let ((meta-data-list '("00001.sql" "00002.sql")))
  (is (valid-migration-p "00003.sql" meta-data-list) t)
  (is (valid-migration-p "00002.sql" meta-data-list) nil)
  (is (valid-migration-p "00000.sql" meta-data-list) nil)
  (is (executable-up-migration-list `(,(make-migration :name "00001.sql")
                                    ,(make-migration :name "00003.sql")) meta-data-list) nil)
  (is (executable-up-migration-list `(,(make-migration :name "00001.sql")
                                    ,(make-migration :name "00002.sql")
                                    ,(make-migration :name "00003.sql"))
                                 meta-data-list) `(,(make-migration :name "00003.sql")) :test #'equalp))

(uiop:delete-file-if-exists test-database)
(finalize)
