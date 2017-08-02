(in-package :cl-user)
(defpackage tachikoma
  (:export :initialize
           :valid-migration-p
           :execute-up-migration
           :execute-down-migration
           :executable-up-migration-list
           :add-migration
           :delete-migration
           :meta-data-list
           :execute-command
           :find-migration-files
           :extract-migration-script
           :load-migration-from-file
           :load-migrations-from-directory
           :make-migration
           :migration
           :migration-name
           :migration-applied
           :migration-up-script
           :migration-down-script)
  (:use :cl
        :osicat
        :cl-dbi
        :envy))
(in-package :tachikoma)

(defun read-file-to-string (filepath)
  (with-open-file (s filepath :direction :input)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

(defun member-p (item list)
  (if (member item list :test #'string=)
      t
      nil))

;;
;; Manipulate MetaData Table
;;
(defparameter meta-data-table-name "MigrationMeta")

(defun meta-data-list (option)
  (let* ((query (format nil "SELECT * FROM ~A;" meta-data-table-name))
         (connection (apply #'dbi:connect option))
         (result (sort (mapcar #'(lambda (x) (getf x :|migration_name|))
                               (dbi:fetch-all (dbi:prepare connection query)))
                       #'string<=)))
    (dbi:disconnect connection)
    result))

(defun add-migration (option migration)
  (let ((query (format nil "INSERT INTO ~A (migration_name) VALUES (?);" meta-data-table-name ))
        (connection (apply #'dbi:connect option)))
      (dbi:execute (dbi:prepare connection query) migration)
      (dbi:disconnect connection)))

(defun delete-migration (option migration)
  (let ((query (format nil "DELETE FROM ~A WHERE migration_name=?;" meta-data-table-name))
        (connection (apply #'dbi:connect option)))
    (dbi:execute (dbi:prepare connection query) migration)
    (dbi:disconnect connection)))

(defun initialize (option)
  (let ((query (format nil "CREATE TABLE IF NOT EXISTS ~A (migration_name text);" meta-data-table-name))
        (connection (apply #'dbi:connect option)))
    (dbi:execute (dbi:prepare connection query))
    (dbi:disconnect connection)))

(defun applied-migration-p (name meta-data-list)
  (member-p name meta-data-list))

;;
;; Migration
;;
(defstruct migration
  (name nil)
  (applied nil)
  (up-script nil)
  (down-script nil))

(defun extract-migration-script (migration-script)
  (let* ((comment (ppcre:create-scanner "--.*$" :multi-line-mode t))
         (split-script (ppcre:create-scanner "^--\\s*tachikoma\\s*down\\s*$"
                          :single-line-mode t
                          :multi-line-mode t))
         ;; split up/down script
         (scripts (cl-ppcre:split split-script migration-script))
         ;; remove comments
         (up-script (cl-ppcre:split ";" (cl-ppcre:regex-replace-all comment (first scripts) "")))
         ;; remove comments
         (down-script (cl-ppcre:split ";" (cl-ppcre:regex-replace-all comment (second scripts) ""))))
    `(:up-script ,up-script :down-script ,down-script)))

(defun load-migration-from-file (filepath meta-data-list)
  (let* ((script (read-file-to-string filepath))
         (base-name (pathname-name filepath))
         (applied (applied-migration-p base-name meta-data-list)))
    (apply #'make-migration `(:name ,base-name :applied ,applied
                                    ,@(extract-migration-script script)))))

(defun is-migration-file (filepath)
  (string-equal "sql" (pathname-type filepath)))

(defun find-migration-files (base-dir)
  (sort (remove-if-not #'is-migration-file (uiop:directory-files base-dir))
        #'(lambda (x y) (string<= (namestring x) (namestring y)))))

(defun load-migrations-from-directory (base-dir meta-data-list)
  (mapcar #'(lambda (x) (load-migration-from-file x meta-data-list))
          (find-migration-files base-dir)))

;;
;; Execute migrations
;;
(defun execute-up-migration (migration option)
  (when (null (migration-applied migration))
    (format t "== ~A: migrating =======~%" (migration-name migration))
    (let ((connection (apply #'dbi:connect option))
          (sql (migration-up-script migration)))
      (loop for i in sql
         do (dbi:execute (dbi:prepare connection i)))
      (setf (migration-applied migration) 't)
      (add-migration option (migration-name migration))
      (dbi:disconnect connection))
    (format t "== ~A: migrated ~%" (migration-name migration))))

(defun execute-down-migration (migration option)
  (when (migration-applied migration)
    (format t "== ~A: reverting =======~%" (migration-name migration))
    (let ((connection (apply #'dbi:connect option))
          (sql (migration-down-script migration)))
      (loop for i in sql
         do (dbi:execute (dbi:prepare connection i)))
      (setf (migration-applied migration) 'nil)
      (delete-migration option (migration-name migration))
      (dbi:disconnect connection))
    (format t "== ~A: reverted ~%" (migration-name migration))))

;;
;; Get the files to be executed
;;
(defun valid-migration-p (migration meta-data-list)
  (cond ((null meta-data-list) 't)
        (t
         (let ((last-migration (car (last meta-data-list))))
           (not (null (string> migration last-migration)))))))

(defun executable-up-migration-list (migration-list meta-data-list)
  (let* ((migration-names (mapcar #'(lambda (x) (migration-name x)) migration-list))
         (candidate-list (set-difference migration-names meta-data-list :test #'string=)))
    (when (and (subsetp meta-data-list migration-names :test #'string=)
               (every #'(lambda (x) (valid-migration-p x meta-data-list))
                      candidate-list))
      (remove-if-not #'(lambda (x) (member-p (migration-name x)
                                             candidate-list))
                     migration-list))))

(defun executable-down-migration-list (migration-list meta-data-list)
  (remove-if-not #'(lambda (x) (member-p (migration-name x)
                                         meta-data-list))
                 migration-list))

;;
;; commands
;;
(defun execute-down-migrations (migration-dir db-option)
  "Execute all migrations."
  (initialize db-option)
  (let* ((meta-data (meta-data-list db-option))
         (migrations (load-migrations-from-directory
                      migration-dir
                      meta-data))
         (executable-list (executable-down-migration-list migrations meta-data)))
    (cond ((not (null executable-list))
           (loop for i in executable-list
              do (execute-down-migration i db-option)))
          ((null executable-list)
           (format t "~&No migrations were executed, database schema was already reverted.~%")))))

(defun execute-up-migrations (migration-dir db-option)
  "Revert all migrations."
  (initialize db-option)
  (let* ((meta-data (meta-data-list db-option))
         (migrations (load-migrations-from-directory
                      migration-dir
                      meta-data))
         (executable-list (executable-up-migration-list migrations meta-data)))
    (cond ((not (null executable-list))
           (loop for i in executable-list
                do (execute-up-migration i db-option)))
          ((null executable-list)
           (format t "~&No migrations were executed, database schema was already up to date.~%")))))

;;
;; Entry Point
;;
(defun execute-command (command &key (config-package-name :tachikoma.config)
                                  (environment "development")
                                  (migration-dir (pathname "./migrations"))
                                  (config-path (pathname "./config/config.lisp")))
  (when (null (osicat:environment-variable "APP_ENV"))
    (setf (osicat:environment-variable "APP_ENV") environment))
  (load config-path)

  (format t "~c[~am Tachikoma [Version 0.0.1]~c[~am~%~%" (code-char #o33) 4 (code-char #o33) 0)
  (format t "Loaded configuration file ~S~%" config-path)
  (format t "Loaded configuration package ~S~%" config-package-name)
  (format t "Using environment ~S~%" environment)

  (let* ((package-name (intern (string-upcase config-package-name) "KEYWORD"))
         (db-config `(,(getf (config package-name) :database-type)
                      ,@(getf (config package-name) :database-connection-spec))))
    (cond ((string= "up" command)
           (execute-up-migrations migration-dir db-config))
          ((string= "down" command)
           (execute-down-migrations migration-dir db-config))))
  )
