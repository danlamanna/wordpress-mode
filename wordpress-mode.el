(require 'jump)
(require 'ido)
(require 'json)

(define-minor-mode wordpress-mode
  "Toggle WordPress mode."
  nil
  " WordPress"
  `((,(kbd "C-c w t") . wp/jump-to-template)
    (,(kbd "C-c w c") . wp/find-config)
    (,(kbd "C-c w p") . wp/jump-to-plugin)
    (,(kbd "C-c w r") . wp/change-password)
    (,(kbd "C-c w m") . wp/jump-to-mu-plugin)
    (,(kbd "C-c w d") . wp/duplicate-theme))
  :group 'wordpress)

(defconst wp/config-file "wp-config.php")

(defun wp/exists()
  "Given the current buffer contains a file, this returns
   the absolute path for the WordPress installation, or `nil'."
  (when (buffer-file-name)
    (locate-dominating-file (file-name-directory (buffer-file-name)) wp/config-file)))

(defun wp/shell-command(command)
  "Runs COMMAND using php -r after requiring wp-blog-header.php, COMMAND
   is run through `shell-command-to-string'."
  (when (wp/exists)
    (let* ((beg (format "php -r \"require('%s');" (concat (wp/exists) "wp-blog-header.php")))
	   (full-command (concat beg command "\"")))
      (shell-command-to-string full-command))))

(defun wp/find-config()
  "Calls `find-file' on the existing `wp/config-file'."
  (interactive)
  (when (wp/exists)
    (find-file (concat (wp/exists) wp/config-file))))

(defun wp/jump-to-template()
  "Calls `jump-find-file-in-dir' on the active template path."
  (interactive)
  (when (wp/exists)
    (jump-find-file-in-dir (wp/shell-command "echo TEMPLATEPATH;"))))

(defun wp/jump-to-plugin()
  "Calls `jump-find-file-in-dir' on the plugins directory."
  (interactive)
  (when (wp/exists)
    (jump-find-file-in-dir (concat (wp/exists) "wp-content/plugins"))))

(defun wp/jump-to-mu-plugin()
  "Calls `jump-find-file-in-dir' on the mu-plugins directory, if it exists."
  (interactive)
  (when (wp/exists)
    (let ((mu-plugin-dir (concat (wp/exists) "wp-content/mu-plugins")))
      (if (file-directory-p mu-plugin-dir)
	  (jump-find-file-in-dir mu-plugin-dir)))))

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

(provide 'wordpress-mode)
