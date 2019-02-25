

;; Version: $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: See credit at EOF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard requirements
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'f)
(require 's)
(require 'bookmark)
(require 'recentf)
(require 'page-break-lines)
(require 'all-the-icons)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard faces
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface dashboard-git-face     '((t (:height 1.0 :foreground "green"   :bold nil))) "")
(defface dashboard-hash-face    '((t (:height 1.0 :foreground "peru"    :bold nil))) "")
(defface dashboard-info-face    '((t (:height 1.0 :foreground "#bc6ec5" :bold t)))   "")
(defface dashboard-banner-face  '((t (:height 1.5 :foreground "#4f97d7" :bold t)))   "")
(defface dashboard-section-face '((t (:height 1.1 :foreground "#4f97d7" :bold t)))   "")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dashboard-subseq (seq start end)
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end) (min len end)))))

(defvar dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map (kbd "g") #'dashboard-insert-startupify-lists)
    map))

(define-derived-mode dashboard-mode special-mode "Dashboard" ""
  :group 'dashboard
  :syntax-table nil
  :abbrev-table nil
  (display-line-numbers-mode -1)
  (whitespace-mode -1)
  (page-break-lines-mode 1)
  (setq inhibit-startup-screen t)
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defgroup dashboard nil ""
  :group 'dashboard)

(defcustom dashboard-page-separator "\n\n\f\n\n" ""
  :type 'string
  :group 'dashboard)

(defconst dashboard-banner-margin 25 "")

(defvar dashboard-item-generators '((recents    . dashboard-insert-recents)))
                                    ;;(bookmarks  . dashboard-insert-bookmarks)
                                    ;;(projects   . dashboard-insert-projects)
                                    ;;(workspaces . dashboard-insert-workspaces)
                                    ;;(info       . dashboard-insert-info)))

(defvar dashboard-items '((recents    . 20)) "")
                          ;;(bookmarks  . 10)
                          ;;(projects   . 10)
                          ;;(workspaces . 10)
                          ;;(info       . 10)) "")

(defvar dashboard-items-default-length 20 "")

(defun dashboard-get-string-from-file (file) ""
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string)))

(defun dashboard-insert-ascii-banner-centered (file) ""
       (insert
        (with-temp-buffer
          (setq banner (propertize (dashboard-get-string-from-file file) 'face 'dashboard-banner-face))
          (insert banner)
          (let ((banner-width 0))
            (while (not (eobp))
              (let ((line-length (- (line-end-position) (line-beginning-position))))
                (if (< banner-width line-length)
                    (setq banner-width line-length)))
              (forward-line 1))
            (goto-char 0)
            (let ((margin dashboard-banner-margin))
              (while (not (eobp))
                (when (not (looking-at-p "$"))
                  (insert (make-string margin ?\ )))
                (forward-line 1))))
          (buffer-string))))

(defun center-body ()
  (let* ((max-text-width 70)
         (margin (max 0 (/ (- (window-width) max-text-width) 2))))
    (setq-local left-margin-width margin)
    (setq-local right-margin-width margin)
    (set-window-buffer nil (current-buffer))))

(defun dashboard-insert-banner () ""
       (goto-char (point-max))
       (dashboard-insert-ascii-banner-centered
        (expand-file-name "~/.emacs.d/lisp/dashboard/dashboard-banner.txt")))

(defun dashboard-insert-badge () ""
       (goto-char (point-max))
       (insert "\n\t")
       (insert (make-string (- dashboard-banner-margin 11) ?\ ))
       (insert-image (create-image (expand-file-name "~/.emacs.d/emacs.png")))
       (insert "\n"))

(defun dashboard-insert-file-list (list-display-name list) ""
       (setq list-display-name-faced (propertize list-display-name 'face 'dashboard-section-face))
       (insert list-display-name-faced)
       (when (car list)
         (mapc (lambda (el)
                 (insert "\n    ")
                 (widget-create 'push-button
                                :action `(lambda (&rest ignore) (find-file-existing ,el))
                                :mouse-face 'highlight
                                :follow-link "\C-m"
                                :button-prefix ""
                                :button-suffix ""
                                :format "%[%t%]"
                                (abbreviate-file-name el)))
               list)))

(defun dashboard-insert-project-list (list-display-name list) ""
       (setq list-display-name-faced (propertize list-display-name 'face 'dashboard-section-face))
       (insert list-display-name-faced)
       (when (car list)
         (mapc (lambda (el)
                 (insert "\n    ")
                 (widget-create 'push-button
                                :action `(lambda (&rest ignore)
                                           (projectile-switch-project-by-name ,el))
                                :mouse-face 'highlight
                                :follow-link "\C-m"
                                :button-prefix ""
                                :button-suffix ""
                                :format "%[%t%]"
                                (abbreviate-file-name el)))
               list)))

(defun dashboard-insert-bookmark-list (list-display-name list) ""
       (setq list-display-name-faced (propertize list-display-name 'face 'dashboard-section-face))
       (insert list-display-name-faced)
       (when (car list)
         (mapc (lambda (el)
                 (insert "\n    ")
                 (widget-create 'push-button
                                :action `(lambda (&rest ignore) (bookmark-jump ,el))
                                :mouse-face 'highlight
                                :follow-link "\C-m"
                                :button-prefix ""
                                :button-suffix ""
                                :format "%[%t%]"
                                (format "%s - %s" el (abbreviate-file-name (bookmark-get-filename el)))))
               list)))

(defun dashboard-insert-workspace-list (list-display-name list) ""
       (setq list-display-name-faced (propertize list-display-name 'face 'dashboard-section-face))
       (insert list-display-name-faced)
       (when (car list)
         (mapc (lambda (el)
                 (insert "\n    ")
                 (widget-create 'push-button
                                :action `(lambda (&rest ignore) (persp-load-state-from-file ,el))
                                :mouse-face 'highlight
                                :follow-link "\C-m"
                                :button-prefix ""
                                :button-suffix ""
                                :format "%[%t%]"
                                (abbreviate-file-name el)))
               list)))

(defun dashboard-insert-info-list (list-display-name list) ""
       (setq list-display-name-faced (propertize list-display-name 'face 'dashboard-section-face))
       (insert list-display-name-faced)
       (when (car list)
         (mapc (lambda (el)
                 (insert "\n    ")
                 (widget-create 'push-button
                                :action `(lambda (&rest ignore) (find-file-existing ,el))
                                :mouse-face 'highlight
                                :follow-link "\C-m"
                                :button-prefix ""
                                :button-suffix ""
                                :format "%[%t%]"
                                (abbreviate-file-name el)))
               list)))

(defun dashboard-insert-page-break () ""
       (dashboard-append dashboard-page-separator))

(defun dashboard-append (msg &optional messagebuf) ""
       (with-current-buffer (get-buffer-create "*start*")
         (goto-char (point-max))
         (let ((buffer-read-only nil))
           (insert msg))))

(defmacro dashboard-insert--shortcut (shortcut-char search-label &optional no-next-line) ""
          `(define-key dashboard-mode-map ,shortcut-char
             (lambda ()
               (interactive)
               (unless (search-forward ,search-label (point-max) t)
                 (search-backward ,search-label (point-min) t))
               ,@(unless no-next-line
                   '((forward-line 1)))
               (back-to-indentation))))

(defun dashboard-goto-link-line () ""
       (interactive)
       (with-current-buffer "*dashboard*"
         (goto-char (point-min))
         (re-search-forward "Homepage")
         (beginning-of-line)
         (widget-forward 1)))

(defun dashboard-insert-recents (list-size) ""
       (recentf-mode)
       (when (dashboard-insert-file-list
              "\tRecent Files:"
              (dashboard-subseq recentf-list 0 list-size))
         (dashboard-insert--shortcut "r" "\tRecent Files:")))

(defun dashboard-insert-bookmarks (list-size) ""
       (when (dashboard-insert-bookmark-list
              "Bookmarks:"
              (dashboard-subseq (bookmark-all-names) 0 list-size))
         (dashboard-insert--shortcut "m" "Bookmarks:")))

(defun dashboard-insert-projects (list-size) ""
       (if (bound-and-true-p projectile-mode)
           (progn
             (projectile-load-known-projects)
             (when (dashboard-insert-project-list
                    "Projects:"
                    (dashboard-subseq (projectile-relevant-known-projects) 0 list-size))
               (dashboard-insert--shortcut "p" "Projects:")))
         (error "Projects list depends on 'projectile-mode` to be activated")))

(defun dashboard-insert-workspaces (list-size) ""
       (if (bound-and-true-p persp-mode)
           (progn
             (when (dashboard-insert-workspace-list
                    "Workspaces:"
                    (dashboard-subseq (f-glob (expand-file-name "*workspace*" save-dir)) 0 list-size))
               (dashboard-insert--shortcut "w" "Workspaces:")))))

(defun dashboard-insert-info (list-size) ""
       (when (dashboard-insert-info-list
              "Info:"
              (dashboard-subseq (f-glob (expand-file-name "*" info-dir)) 0 list-size))
         (dashboard-insert--shortcut "i" "Info:")))

(defun dashboard-insert-startupify-lists () ""
       (interactive)
       (with-current-buffer (get-buffer-create "*Emacs*")
         (let ((buffer-read-only nil)
               (list-separator "\n\n"))
           (erase-buffer)
           (dashboard-insert-badge)
           ;(center-body)
           (insert "\n")
           (dashboard-insert-banner)
           (insert "\n")
           (setq version-faced (propertize "\t\t\t\t\t\t\t    Version: " 'face 'dashboard-info-face))
           (insert version-faced)
           (insert (all-the-icons-faicon "tag") (format " %d.%d\n" emacs-major-version emacs-minor-version))
           (setq version-faced (propertize "  \t\t\t\t\t\t    Init Time: " 'face 'dashboard-info-face))
           (insert version-faced)
           (insert (all-the-icons-faicon "clock-o") (format " %s\n\n\n" (emacs-init-time)))
           (dashboard-insert-page-break)
           (mapc (lambda (els)
                   (let* ((el (or (car-safe els) els))
                          (list-size
                           (or (cdr-safe els)
                               dashboard-items-default-length))
                          (item-generator
                           (cdr-safe (assoc el dashboard-item-generators))))
                     (funcall item-generator list-size)
                     (dashboard-insert-page-break)))
                 dashboard-items))
         (dashboard-mode)
         (goto-char (point-min))))

;;;###autoload

(defun dashboard-setup-startup-hook () ""
       (if (< (length command-line-args) 2 )
           (progn
             (add-hook 'after-init-hook (lambda () (dashboard-insert-startupify-lists)))
             (add-hook 'emacs-startup-hook
                       '(lambda ()
                          (switch-to-buffer "*Emacs*")
                          (goto-char (point-min))
                          (redisplay)
						  (hidden-mode-line-mode))))))

(declare-function projectile-load-known-projects "ext:projectile.el")
(declare-function projectile-relevant-known-projects "ext:projectile.el")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (display-graphic-p)
  (dashboard-setup-startup-hook))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dashboard)

;;; dashboard.el --- A startup screen extracted from Spacemacs

;; taken from https://github.com/rakanalh/emacs-dashboard, modified by https://github.com/eccentricayman

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dashboard.el ends here
