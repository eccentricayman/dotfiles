;;; fortune-cookie-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "fortune-cookie" "fortune-cookie.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from fortune-cookie.el

(autoload 'fortune-cookie "fortune-cookie" "\
Get a fortune cookie (maybe with cowsay).

\(fn)" t nil)

(autoload 'fortune-cookie-comment "fortune-cookie" "\
Comment ARG with PREFIX.

ARG is the input string.
PREFIX is prepended to each line of ARG.

\(fn ARG PREFIX)" t nil)

(defvar fortune-cookie-mode nil "\
Non-nil if Fortune-Cookie mode is enabled.
See the `fortune-cookie-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `fortune-cookie-mode'.")

(custom-autoload 'fortune-cookie-mode "fortune-cookie" nil)

(autoload 'fortune-cookie-mode "fortune-cookie" "\
Set `initial-scratch-message' to a commented fortune cookie.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fortune-cookie" '("fortune-cookie-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; fortune-cookie-autoloads.el ends here
