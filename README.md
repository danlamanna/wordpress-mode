wordpress-mode
====================

Minor mode for editing WordPress files with Emacs.


Installation    
----
**Note: wordpress-mode uses ido for all of it's jump functions, ido is required in emacs 22 and above.**    
     
1) `git clone https://github.com/asdasDan/wordpress-mode.git`    
2)  Add to load path, using something like this (differs on path):  
  
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/wordpress-mode"))
    (require 'wordpress-mode)   

3) Ensure `wordpress-mode` is called in the right place, I use `php-mode-hook`

    (add-hook 'php-mode-hook '(lambda()
                                (if (wp/exists)
                                    (wordpress-mode))))
     
Usage     
----
Quick Navigation   
`C-c w t` opens ido with the template files in your WP installations active template.   
`C-c w p` opens ido with the files in your plugin path.   
`C-c w m` opens ido with the files in your must use plugin path, if one exists.   
`C-c w c` opens the wp-config.php from your installation.   
    
Utilities     
`C-c w d` prompts you to select a theme that exists in WordPress to **duplicate**.    
`C-c w r` prompts you for a new password for the user with an ID of 1.    
`C-c w s` opens an inferior MySQL process in a buffer logged in with the credentials defined in wp-config.php   