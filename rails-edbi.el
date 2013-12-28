;; Copyright (C) 2013 by Matt Sears

;; Author: Matt Sears <matt@mattsears.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; This `rails-edbi.el' simply attempts to open a database viewer using the
;; great emacs-edbi (https://github.com/kiwanami/emacs-edbi) tool in your rails
;; project by parsing the info in the database.yml file.
;;
;; So if you're working in your rails project, M-x `my-rails-database' will
;; automatically open emacs-edbi in the rails development database
;;
;; To use this package, add following code to your init.el or .emacs
;;   (require 'rails-edbi)
;;

(require 'edbi)

(defmacro* rails-project:with-root ((root) &body body)
  "If you use `rails-project:root' or functions related on it
several times in a block of code, you can optimize your code by
using this macro. Also, blocks of code will be executed only if
rails-root exist.
 (rails-project:with-root (root)
    (foo root)
    (bar (rails-core:file \"some/path\")))
 "
  `(let ((,root (rails-project:root)))
     (when ,root
       (flet ((rails-project:root () ,root))
         ,@body))))

(defun rails-project:root ()
  "Return RAILS_ROOT if this file is a part of a Rails application,
else return nil"
  (let ((curdir default-directory)
        (max 10)
        (found nil))
    (while (and (not found) (> max 0))
      (progn
        (if (file-exists-p (concat curdir "config/environment.rb"))
            (progn
              (setq found t))
          (progn
            (setq curdir (concat curdir "../"))
            (setq max (- max 1))))))
    (if found (expand-file-name curdir))))

(defun rails-core:file (file-name)
  "Return the full path for FILE-NAME in a Rails directory."
  (when file-name
    (if (file-name-absolute-p file-name)
        file-name
      (rails-project:with-root
       (root)
       (concat root file-name)))))

(defstruct rails-db-conf adapter host database username password)

(defun yml-value (name)
  "Return the value of the parameter named NAME in the current
buffer or an empty string."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp (format "%s:[ ]*\\(.*\\)[ ]*$" name) nil t)
        (match-string 1)
      "")))

(defun rails-db-parameters (env)
  "Return database parameters for enviroment ENV"
  (with-temp-buffer
    (shell-command
     (format "ruby -r yaml -r erb -e 'YAML.load(ERB.new(ARGF.read).result)[\"%s\"].to_yaml.display' %s"
             env
             (rails-core:file "config/database.yml"))
     (current-buffer))
    (let ((answer
           (make-rails-db-conf
            :adapter  (yml-value "adapter")
            :host     (yml-value "host")
            :database (yml-value "database")
            :username (yml-value "username")
            :password (yml-value "password"))))
      answer)))

(defun edbi:open-db-viewer-with-info (uri &optional user auth)
  "Open Database viewer buffer with data source info."
  (let ((data-source (edbi:data-source uri user auth))
        (conn (edbi:start)))
    (edbi:connect conn data-source)
    (edbi:dbview-open conn)))

(defun rails-dbi-driver ()
  "Find the appropriate dbi driver based on the database adapter
   dbi:SQL Platform:database_name:host_name:port"

  (let ((conf (rails-db-parameters "development"))
        (uri nil)
        (driver nil)
        (database nil)
        (host "localhost"))

    (setq adapter (rails-db-conf-adapter conf))
    (setq database (rails-db-conf-database conf))
    (setq host (rails-db-conf-host conf))

    (cond ((equal adapter "postgresql")
           (setq driver "Pg"))
          ((equal adapter "mysql2")
           (setq driver "mysql"))
          ((equal adapter "sqlite3")
           (setq driver "SQLite")))

    (concat "dbi:" driver ":dbname=" database)
    ))

(defun my-rails-database ()
  "Find a tag using ido"
  (interactive)
  (let ((conf (rails-db-parameters "development")))
    (setq password (rails-db-conf-password conf))
    (setq username (rails-db-conf-username conf))
    (edbi:open-db-viewer-with-info (rails-dbi-driver) username password)
    ))

(provide 'rails-edbi)
