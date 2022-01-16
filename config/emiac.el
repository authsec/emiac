;; If EMIAC_EXTERNALIZE_CONFIGURATION is set to one, the user does not
;; wanto to use the built in init.el file, in this case we do reference
;; the init.el file from a .emacs.d folder available on the user's file
;; system.
(if (string= (getenv "EMIAC_EXTERNALIZE_CONFIGURATION") "1")
  (progn
  ;; The user mounts the research directory from the host file system
  (message "Loading externalized configuration")
  (setq user-init-file (concat (getenv "EMIAC_USER_INIT_DIR") "/init.el"))
  (setq user-emacs-directory "/home/emiac/research/.emacs.d/"))

  (progn
  ;; Use baked in configuration
  (message "Loading factory configuration")
  (setq user-init-file (concat (getenv "EMIAC_USER_INIT_DIR") "/emiac-init.el"))
  (setq user-emacs-directory "/home/emiac/.emacs.d/"))
)

(setq default-directory (getenv "EMIAC_RESEARCH_DIR"))
(setenv "HOME" "/home/emiac")

(load user-init-file)