(in-package :cl-user)
(defpackage tachikoma
  (:export :initialize
           :valid-migration-p
           :execute-up-migration
           :execute-down-migration
           :executable-migration-list
           :add-migration
           :delete-migration
           :meta-data-list)
  (:use :cl
        :cl-dbi
        :migration))
(in-package :tachikoma)

(defparameter meta-data-table-name "MigrationMeta")

(defun meta-data-list (option)
  (let* ((query "SELECT * FROM MigrationMeta")
         (connection (apply #'dbi:connect option))
         (result (sort (mapcar #'(lambda (x) (getf x :|migration_name|))
                               (dbi:fetch-all (dbi:prepare connection query)))
                       #'string<=)))
    (dbi:disconnect connection)
    result))

(defun add-migration (option migration)
  (let ((query "INSERT INTO MigrationMeta (migration_name) VALUES (?);")
        (connection (apply #'dbi:connect option)))
      (dbi:execute (dbi:prepare connection query) migration)
      (dbi:disconnect connection)))

(defun delete-migration (option migration)
  (let ((query "DELETE FROM MigrationMeta WHERE migration_name=?;")
        (connection (apply #'dbi:connect option)))
    (dbi:execute (dbi:prepare connection query) migration)
    (dbi:disconnect connection)))

(defun initialize (option)
  (let ((query "CREATE TABLE IF NOT EXISTS MigrationMeta(migration_name text);")
        (connection (apply #'dbi:connect option)))
    (dbi:execute (dbi:prepare connection query))
    (dbi:disconnect connection)))

(defun execute-up-migration (migration option)
  (let ((connection (apply #'dbi:connect option))
        (sql (migration-up-script migration)))
    (format t "========= ~A check    =========~%" (migration-name migration))
    (when (null (migration-applied migration))
      (mapc #'(lambda (x)
                (dbi:execute (dbi:prepare connection x))) sql)
      (setf (migration-applied migration) 't)
      (add-migration option (migration-name migration)))
    (format t "========= ~A executed =========~%" (migration-name migration))
    (dbi:disconnect connection)))

(defun execute-down-migration (migration option)
  (let ((connection (apply #'dbi:connect option))
        (sql (migration-down-script migration)))
    (format t "========= ~A check    =========~%" (migration-name migration))
    (when (migration-applied migration)
      (mapc #'(lambda (x)
                (dbi:execute (dbi:prepare connection x))) sql)
      (setf (migration-applied migration) 'nil)
      (delete-migration option (migration-name migration)))
    (format t "========= ~A executed =========~%" (migration-name migration))
    (dbi:disconnect connection)))


(defun executable-migration-list (migration-list meta-data-list)
  (if (and (subsetp meta-data-list migration-list :test #'string=)
           (every #'(lambda (x) (valid-migration-p x meta-data-list))
                  (set-difference migration-list meta-data-list :test #'string=)))
      (set-difference migration-list meta-data-list :test #'string=)))

(defun valid-migration-p (migration meta-data-list)
  (let ((last-migration (car (last meta-data-list))))
     (not (null (string> migration last-migration)))))
