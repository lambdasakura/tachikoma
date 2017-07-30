(in-package :cl-user)
(defpackage migration
  (:export :find-migration-files
           :extract-migration-script
           :load-migration-from-file
           :load-migrations-from-directory
           :migration
           :migration-name
           :migration-applied
           :migration-up-script
           :migration-down-script)
  (:use :cl
        :cl-ppcre))
(in-package :migration)

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
    (list :up-script up-script :down-script down-script)))

(defun load-migration-from-file (filepath meta-data-list)
  (let* ((script (with-open-file (s filepath :direction :input)
                   (let ((buf (make-string (file-length s))))
                     (read-sequence buf s)
                     buf)))
         (base-name (pathname-name filepath))
         (applied (not (null (member base-name meta-data-list :test #'string=)))))
    (apply #'make-migration
           (concatenate 'list
                        (list :name base-name :applied applied)
                        (extract-migration-script script)))))

(defun is-migration-file (filepath)
  (let ((file-name-string (namestring filepath)))
    (string-equal ".sql" (subseq file-name-string (- (length file-name-string) 4)))))

(defun find-migration-files (base-dir)
  (sort (remove-if-not #'(lambda (x) (is-migration-file x)) (uiop:directory-files base-dir))
        #'(lambda (x y) (string<= (namestring x) (namestring y)))))

(defun load-migrations-from-directory (base-dir meta-data-list)
  (mapcar #'(lambda (x) (load-migration-from-file x meta-data-list)) (find-migration-files base-dir)))
