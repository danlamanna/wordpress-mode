;;; wordpress-mode.el --- A minor mode for speeding up WordPress development in emacs.
;; Copyright (C) 2012 Dan LaManna

;; Author: Dan LaManna <dan.lamanna@gmail.com>
;; Keywords: wordpress mode

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

;;; Commentary:
;; Requires calling commands via `wp/php-executable'.
;; Untested in many environments.

;;; Todo
;; Implement some form of ctags, to allow inline documentation of any WP function,
;;  in case the user isn't using wpsh.
;; Some form of wrapper to wpsh.

;;; Code:

(require 'ido)
(require 'json)

(defcustom wp/php-executable "/usr/bin/php"
  "Path to PHP for calling WordPress functions.")

(defconst wp/config-file "wp-config.php")

(eval-after-load "sql"
  '(add-to-list 'sql-product-alist
                '(mysql-wp-noprompt
                  :name "WP/MySQL"
                  :font-lock sql-mode-mysql-font-lock-keywords
                  :sqli-login nil
                  :sqli-program sql-mysql-program
                  :sqli-options sql-mysql-options
                  :sqli-comint-func sql-comint-mysql
                  :sqli-prompt-regexp "^mysql> "
                  :sqli-prompt-length 6)))

(define-minor-mode wordpress-mode
  "Toggle WordPress mode."
  nil
  " WordPress"
  `((,(kbd "C-c w t") . wp/jump-to-template)
    (,(kbd "C-c w c") . wp/find-config)
    (,(kbd "C-c w p") . wp/jump-to-plugin)
    (,(kbd "C-c w r") . wp/change-password)
    (,(kbd "C-c w s") . wp/sql)
    (,(kbd "C-c w m") . wp/jump-to-mu-plugin)
    (,(kbd "C-c w d") . wp/duplicate-theme))
  :group 'wordpress)

;; Tramp helpers, these are "safe", meaning
;; the add/remove prefix functions only change
;; their arguments if they don't have/have the prefix.
(defun wp/--is-tramp-filename(&optional filename)
  "Returns `t' or `nil' based on whether or not FILENAME
   is a file using tramp. FILENAME defaults to `buffer-file-name'."
  (let ((filename (or filename (buffer-file-name))))
    (and (boundp 'tramp-file-name-structure)
         (string-match (nth 0 tramp-file-name-structure) filename))))

(defun wp/--tramp-prefix()
  (when (buffer-file-name)
    (file-remote-p (buffer-file-name))))

(defun wp/--add-tramp-prefix(&optional filename)
  (let ((filename (or filename (buffer-file-name))))
    (if (wp/--is-tramp-filename filename)
        filename
      (concat (file-remote-p filename) filename))))

(defun wp/--remove-tramp-prefix(&optional filename)
  (let ((filename (or filename (buffer-file-name))))
    (if (wp/--is-tramp-filename filename)
        (tramp-file-name-localname (tramp-dissect-file-name filename))
      filename)))

(defun wp/exists()
  "Returns the absolute path for the WordPress installation, or `nil'."
  (let ((wp-dir (locate-dominating-file default-directory wp/config-file)))
    (if wp-dir
        (expand-file-name wp-dir))))

(defun wp/shell-command(command)
  "Runs COMMAND using `wp/php-executable' -r after requiring wp-blog-header.php, COMMAND
   is run through `shell-command-to-string'."
  (when (wp/exists)
    (let* ((bootstrap-file (concat (wp/--remove-tramp-prefix (wp/exists)) "/wp-blog-header.php"))
           (begin (format "%s -r \"require_once('%s');" wp/php-executable bootstrap-file))
           (full-command (concat begin command "\"")))
      (shell-command-to-string full-command))))

(defun wp/jump-in-dir(dir)
  (if (or (equal ido-mode 'file) (equal ido-mode 'both))
      (ido-find-file-in-dir dir)
    (let ((default-directory dir))
      (call-interactively 'find-file))))

(defun wp/find-config()
  "Calls `find-file' on the existing `wp/config-file'."
  (interactive)
  (when (wp/exists)
    (find-file (concat (wp/exists) wp/config-file))))

(defun wp/jump-to-template()
  "Calls `wp/jump-in-dir' on the active template path."
  (interactive)
  (when (wp/exists)
    (wp/jump-in-dir (concat (wp/--tramp-prefix) (wp/shell-command "echo TEMPLATEPATH;")))))

(defun wp/jump-to-plugin()
  "Calls `wp/jump-in-dir' on the plugins directory."
  (interactive)
  (when (wp/exists)
    (wp/jump-in-dir (concat (wp/exists) "wp-content/plugins"))))

(defun wp/jump-to-mu-plugin()
  "Calls `wp/jump-in-dir' on the mu-plugins directory, if it exists."
  (interactive)
  (when (wp/exists)
    (let ((mu-plugin-dir (concat (wp/exists) "wp-content/mu-plugins")))
      (if (file-directory-p mu-plugin-dir)
          (wp/jump-in-dir mu-plugin-dir)))))

(defun wp/available-themes()
  "Returns list of strings with themes found with no errors."
  (when (wp/exists)
    (let* ((themes-json (wp/shell-command "exit(json_encode(wp_get_themes(array('errors' => false))));"))
           (themes-list (json-read-from-string themes-json)))
      (mapcar 'symbol-name (mapcar 'car themes-list)))))

(defun wp/change-password(&optional user-id)
  "Prompts the user for a new password, and changes it using
   wp_set_password. If it doesn't work, or fails somehow, no
   error is thrown.

   If USER-ID is passed, that will be passed to wp_set_password,
   otherwise, it defaults to 1.

   TODO: Error handling, escaping single quotes in `new-pass'."
  (interactive)
  (when (wp/exists)
    (let ((uid (or user-id 1))
          (new-pass (read-passwd "New Password: ")))
      (wp/shell-command (format "@wp_set_password('%s', %d);" new-pass uid)))))

(defun wp/sql()
  "Runs MySQL as an inferior process, using the credentials
   defined as constants in `wp/config-file' in a buffer.

   Uses `mysql-wp-noprompt' which is defined above as a sql product."
  (interactive)
  (when (wp/exists)
    (let* ((db-creds (wp/db-credentials))
           (sql-user (cdr (assoc "DB_USER" db-creds)))
           (sql-password (cdr (assoc "DB_PASSWORD" db-creds)))
           (sql-database (cdr (assoc "DB_NAME" db-creds)))
           (sql-server (cdr (assoc "DB_HOST" db-creds))))
      (sql-product-interactive 'mysql-wp-noprompt))))

(defun wp/shell()
  (interactive)
  (when (wp/exists)
    (let* ((phpsh (shell-command-to-string "which phpsh"))
           (bootstrap (concat (wp/--remove-tramp-prefix (wp/exists)) "/wp-blog-header.php"))
           (explicit-shell-file-name "phpsh")
           (default-directory (wp/exists))
           (explicit-phpsh-args `(,bootstrap)))
      (if (string-equal phpsh "")
          (message "'which phpsh' evaluted to nothing, can't run WP shell.")
        (call-interactively 'shell)))))

(defun wp/duplicate-theme()
  "Prompts the user to select a theme from `wp/available-themes' using ido,
   then prompts for a new directory name for the theme to be duplicated into.

   Unless the directory already exists, the theme files will be copied into the
   directory.

   TODO: Replace the theme name in style.css to match the directory name?"
  (interactive)
  (when (wp/exists)
    (let ((themes (wp/available-themes)))
      (when (listp themes)
        (let* ((base-theme-dir (concat (wp/exists) "wp-content/themes/"))
               (orig-theme-dir (concat base-theme-dir (ido-completing-read "Theme to Duplicate: " themes)))
               (new-theme-dir  (concat base-theme-dir (read-from-minibuffer "Directory Name: "))))
          (unless (file-directory-p new-theme-dir)
            (copy-directory orig-theme-dir new-theme-dir nil nil t)))))))

(defun wp/db-credentials()
  "Fetches database credentials from `wp/config-file' by way of regular
   expression matching. Not the most accurate, but easier than running
   another PHP command and bootstrapping WP."
  (let ((db-credentials ()))
    (with-temp-buffer
      (insert-file-contents (concat (wp/exists) wp/config-file))
      (goto-char (point-min))
      (while (search-forward-regexp "define\s*(\s*['\"]\\(DB_\\(HOST\\|NAME\\|PASSWORD\\|USER\\)\\)['\"]\s*,\s*['\"]\\([^'\"]*\\)['\"]\s*)" nil t)
        (add-to-list 'db-credentials `(,(match-string 1) . ,(match-string 3)))))
    db-credentials))

(defun wp/describe-function(function)
  (interactive "sFunction: ")
  (with-help-window (help-buffer)
    (let* ((found-func-buffer (find-tag-noselect function))
           (found-func-fname  (buffer-file-name found-func-buffer))
           (found-func-usage  (wp/--get-function-usage-in-line (wp/--get-line-in-buffer found-func-buffer))))
      (princ (format "%s is a function in `%s`.\n\n" function (file-name-nondirectory found-func-fname)))
      (princ (format "%s\n\n" (replace-in-string found-func-usage "function" "")))
      (princ (format "documentation..\n\n")))))

(defun wp/--get-function-usage-in-line(line)
  (let* ((usage-match (string-match "[a-zA-Z\_]+(.*+)" line))
         (usage-match-end (match-end 0)))
    (if (and usage-match
             usage-match-end)
        (substring line usage-match usage-match-end))))

(defun wp/--get-line-in-buffer(buffer)
  (with-current-buffer buffer
    (beginning-of-line)
    (call-interactively 'set-mark-command)
    (end-of-line)
    (buffer-substring-no-properties (mark) (point))))

(provide 'wordpress-mode)

;;; wordpress-mode.el ends here
