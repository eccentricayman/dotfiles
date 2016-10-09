;Custom emacs configuration by eccentricayman on Github

;(let ((file-name-handler-alist nil))
;(run-with-idle-timer
; 5 nil
; (lambda ()
;   (setq gc-cons-threshold 1000000)
;   (message "gc-cons-threshold restored to %S"
;            gc-cons-threshold)))
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-safe-themes
   (quote
    ("06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "38f48e62e16e2c8f178c7e9de00aab382bc92d84ea382822907ed4e762388ae0" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "4ab95b35f7720043592b49d890003874aa1954a3cf299edde13657c6a9182d85" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "b61c55259c639a54628f91452b060b99c550a1269eb947e372321b806b68f114" "1160f5fc215738551fce39a67b2bcf312ed07ef3568d15d53c87baa4fd1f4d4e")))
 '(fancy-splash-image "~/.emacs.d/emacs.png")
 '(fci-rule-color "#3E4451")
 '(org-startup-truncated t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(package-selected-packages
   (quote
    (web-mode hl-line+ nlinum-relative doom-themes ample-theme spacemacs-theme multiple-cursors dracula-theme aurora-theme windresize ido-better-flex ido-vertical-mode smex recentf-ext rainbow-delimiters popup highlight-parentheses fsm atom-one-dark-theme)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map
   (quote
    ((20 . "#dd5542")
     (40 . "#CC5542")
     (60 . "#fb8512")
     (80 . "#baba36")
     (100 . "#bdbc61")
     (120 . "#7d7c61")
     (140 . "#6abd50")
     (160 . "#6aaf50")
     (180 . "#6aa350")
     (200 . "#6a9550")
     (220 . "#6a8550")
     (240 . "#6a7550")
     (260 . "#9b55c3")
     (280 . "#6CA0A3")
     (300 . "#528fd1")
     (320 . "#5180b3")
     (340 . "#6380b3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

;;color theme
(load-theme 'atom-one-dark)

;;enable parens
(require 'highlight-parentheses)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :height 120))))
 '(button ((t (:inherit default))))
 '(company-scrollbar-bg ((t (:background "#3232ff"))))
 '(company-scrollbar-fg ((t (:background "#1919ff"))))
 '(company-tooltip ((t (:inherit default :background "#0a0aff"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))

;because ido-speed-hacks is terribly coded
(setq warning-minimum-level :error)
;; Preset width nlinum
;(add-hook 'nlinum-mode-hook
;          (lambda ()
;            (setq nlinum--width
;                  (length (number-to-string
;                           (count-lines (point-min) (point-max)))))))

 ;;;;;;;;;;;; misc ;;;;;;;;;;;;;;;;;;;;;;;
;; enable clipboard
(setq x-select-enable-clipboard t)
;; autocomplete paired brackets
(electric-pair-mode 1)
(global-set-key (kbd "<C-tab>") 'other-window)
;; tramp mode for editing through ssh
(setq tramp-default-method "ssh")
;; show paren mode
(show-paren-mode 1)
;;disabled because emacsclient is weird with it
(global-nlinum-mode 1)
;; enable lines mode
;(global-linum-mode 1)
(setq backup-directory-alist `(("." . "~/.emacsbackups")))
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)
;;;;;;;;;;;; misc ;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;byebye menu;;;;;;;;;;;;;;;;;
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
    
;better terminal line, faster
(if (display-graphic-p)
    (progn
      (require 'spaceline-config)
      (spaceline-emacs-theme)
      ;this fixes the spaceline issues on os x, but makes emacs look uglier
      ;(setq ns-use-srgb-colorspace nil)
      (spaceline-compile)
      (global-hl-line-mode 1))
  (progn
    ;(powerline-default-theme)
    (require 'airline-themes)
    (load-theme 'airline-distinguished)
    (global-set-key (kbd "<mouse-4>") 'previous-line)
    (global-set-key (kbd "<mouse-5>") 'next-line)
    (xterm-mouse-mode)))

;adios crappy terminal background
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "#222" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;also useless, was to fix powerline messing up but we have spaceline now!
;(add-hook 'desktop-after-read-hook 'powerline-reset)
;;;;;;;;;;;;;;;;;;;;;byebye menu;;;;;;;;;;;;;;;;;

;;;;;;;better mode-line ;;;;;;;;;;;;;;;;;;

;;;;;;;better mode-line;;;;;;;;;;;;;;;;;

;;;;;;;;starting dir;;;;;;;;;;;
(setq default-directory "~")

;(setq desktop-buffers-not-to-save
;    (concat "\\("
;            "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^tags"
;            "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;        "\\)$"))
;(add-to-list 'desktop-modes-not-to-save 'dired-mode)
;(add-to-list 'desktop-modes-not-to-save 'info-mode)
;(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
;(defun emacs-process-p (pid)
;  "if pid is the process id of an emacs process, return t, else nil.
;also returns nil if pid is nil."
;  (when pid
;    (let ((attributes (process-attributes pid)) (cmd))
;      (dolist (attr attributes)
;        (if (string= "comm" (car attr))
;            (setq cmd (cdr attr))))
; q      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

;(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
;  "don't allow dead emacsen to own the desktop file."
;  (when (not (emacs-process-p ad-return-value))
;    (setq ad-return-value nil)))

;;(require 'sublimity)
;;(require 'sublimity-scroll)
;;(require 'sublimity-map)
;;(require 'sublimity-attractive)
;;(sublimity-mode 1)


;grr @ difficult autocompletes.. (sike replaced with company mode)
;; (global-set-key [(tab)] 'smart-tab)

;; (defun smart-tab ()
;;   "this smart tab is minibuffer compliant: it acts as usual in
;;     the minibuffer. else, if mark is active, indents region. else if
;;     point is at the end of a symbol, expands it. else indents the
;;     current line."
;;   (interactive)
;;   (if (minibufferp)
;;       (unless (minibuffer-complete)
;;         (dabbrev-expand nil))
;;     (if mark-active
;;         (indent-region (region-beginning)
;;                        (region-end))
;;       (if (looking-at "\\_>")
;;           (dabbrev-expand nil)
;;         (indent-for-tab-command)))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4) ; just in case
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq indent-line-function 'insert-tab)
;change block to line cursor if enabled
;(setq-default cursor-type 'bar)

;(add-hook 'window-setup-hook 'on-after-init)
(require 'neotree)
(global-set-key (kbd "C-c f") 'neotree-toggle)
;fix neotree link color
(setq frame-background-mode 'dark)

;magit
(global-set-key (kbd "C-c g") 'magit-status)

;hides abbrev-mode, company-mode, and yas/minor-mode
(when (require 'diminish nil 'noerror)
  (eval-after-load "company"
    '(diminish 'company-mode ""))
  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode ""))
  (eval-after-load "yasnippet"
    '(diminish 'yas/minor-mode ""))
  (eval-after-load "flycheck"
    '(diminish 'flycheck-mode))
)

;random testing, makes cursor go through entire rainbow spectrum
; (defvar blink-cursor-colors (list  "#ff0000" "#ff7f00" "#ffff00" "#00ff00" "0000ff" "551a8b" "4b0082"))
; (setq blink-cursor-count 0)
; (defun blink-cursor-timer-function ()
;  (when (not (internal-show-cursor-p))
;    (when (>= blink-cursor-count (length blink-cursor-colors))
;      (setq blink-cursor-count 0))
;    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
;    (setq blink-cursor-count (+ 1 blink-cursor-count))
;    )
;  (internal-show-cursor nil (not (internal-show-cursor-p)))
;  )
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))
(setq doc-view-ghostscript-program "gswin64c")
(setq org-log-done 'time)
(defadvice doc-view-display (after fit-width activate)
  (doc-view-fit-width-to-window))
(defun always-use-fancy-splash-screens-p () 1)
(defalias 'use-fancy-splash-screens-p 'always-use-fancy-splash-screens-p)
(global-set-key (kbd "M-x") 'smex)
;(global-set-key (kbd "M-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(smex-initialize)
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)
(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall ,ido-cannot-complete-command)))))
    ad-do-it))
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-show-count t)
(ido-better-flex/enable)
(setq neo-smart-open t)
;non elpa/melpa/marmalade packages, like the m-x speed fixy thingy
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "~/.emacs.d/lisp/ido-speed-hack/ido-speed-hack.elc")

;miscellanous new stuff!
;(evil-define-key 'normal emacs-lisp-mode-map (kbd ";") 'smex)

;moving lines up and down
(defun move-line-up ()
  "move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)

;might slowdown, but allows vim + emacs
;(require 'powerline)

;;;;;;temp;;;;;;
;(spaceline-emacs-theme)
;;;;;;temp;;;;;;

;;;;;;;;;;fix this;;;;;;;;;;
;dat vim
;(defun to-evil ()
;    (evil-mode 1)
;    (powerline-default-theme)
;    (load-theme 'airline-base16-shell-dark)
;    )

;(defun to-emacs ()
;    (evil-mode 0)
;    ;(powerline-reset)
;    (spaceline-emacs-theme))

;defining keys for emacs-mode and vim-mode
;(global-set-key (kbd "c-c e") 'to-emacs())
;(global-set-key (kbd "c-c v") 'to-vim())

;start in emacs mode
;(add-hook 'after-init 'to-emacs())
;;;;;;;;;;fix this;;;;;;;;;;;
(setq redisplay-dont-pause t)
;)

;no gnu emacs buffer when opening file
;; (when (and (string= "*scratch*" (buffer-name)) (not (buffer-file-name)))
;;   (progn
;;     (setq inhibit-startup-screen t)))

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)


;flycheck 4 code check
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(set-face-attribute 'flycheck-warning nil
                    :underline "yellow")
(set-face-attribute 'flycheck-error nil
                    :underline "red")

(require 'company)

;comapny tab completion
(add-hook 'after-init-hook 'global-company-mode)

;comapny then indent
(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(global-set-key "\t" 'indent-or-complete)

;dark background for company
(require 'color)
  
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
    
;eclim 4 java (hot damn this takes time to load)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

;irony for c / c++
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;jedi for python
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;autocomplete html
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;processing
(autoload 'processing-mode "processing-mode" "processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))

(setq processing-location "/home/eccentricayman/github/processing-3.1.1/processing-java")
(setq processing-application-dir "/home/eccentricayman/github/processing-3.1.1/processing")
(setq processing-sketchbook-dir "/home/eccentricayman/sketchbook")
(global-set-key (kbd "C-c P") 'processing-sketch-run)

;loading f.el (os x is the load one)
;(require 'f.el)
(load "~/.emacs.d/lisp/f/f.elc")

;java sht (install f.el manually on os x)
(defun java-compile-and-run ()
  "compile and run java files."
  (interactive)
  (shell-command-on-region
  ;seperate buffer
   (point-min)
   (point-max)
   ;da command
   (concat "javac " (buffer-file-name) " && " "java " (f-base buffer-file-name))
   ;name of buffer
   "java compile and run"
   ))

(global-set-key (kbd "C-c j") 'java-compile-and-run)

(defun python-run ()
  "easier pythoning than c-c c-c, also only for simple shit"
  (interactive)
  (shell-command-on-region
   ;seperate buffr
   (point-min)
   (point-max)
   ;running file
   (concat "python " (buffer-file-name))
   ;buffr name
   "python run"
   ))

(global-set-key (kbd "C-c p") 'python-run)    

(defun script-run ()
  "Run shell script."
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   (concat ". " (buffer-file-name))
   "Script Run"
   ))

(global-set-key (kbd "C-c s") 'script-run)

(defun c-compile-and-run ()
  "c compile and run."
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   (concat "gcc " (buffer-file-name) " && " "./a.out"))
  "c compile and run"
  )

(global-set-key (kbd "C-c c") 'c-compile-and-run) 

;osx stuff
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(if (eq system-type 'darwin)
    (progn
      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx))
  )
;
