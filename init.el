;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; load Org-mode from source when the ORG_HOME environment variable is set
(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq starter-kit-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p (expand-file-name "lisp"
                                                      (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))))



;;========Personal Global========= 


(setq ring-bell-function 'ignore)       ;;ignores ALL Alarm Bells; no flash when scrolling 
(tool-bar-mode -1)                      ;;no toolbar
(menu-bar-mode -1)                      ;;no menu bar
;;(global-linum-mode 1)                   ;;global line numbers
(column-number-mode 1)                  ;;columns
(scroll-bar-mode -1)                    ;;no scrollbar
(global-set-key "\C-z" nil)             ;;no minimize
(transient-mark-mode 1)                 ;;required for comment/uncomment
(global-auto-revert-mode t)             ;;auto refresh buffers


;; C-x C-f creates directory if needed
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))


;;========= Python Mode ==========

;;http://ergoemacs.org/emacs/emacs_set_keys_for_major_mode.html

(defun python-mode-keys ()
  "Modify keymaps used by python mode."
  (local-set-key (kbd "C-c ;") 'comment-dwim)
  )

;; add to html-mode-hook
(add-hook 'python-mode-hook 'python-mode-keys)


;;========Fullscreen Mode========= 

;; F11 = Full Screen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
      (if (equal 'fullboth current-value)
        (if (boundp 'old-fullscreen) old-fullscreen nil)
        (progn (setq old-fullscreen current-value)
          'fullboth)))))3
(global-set-key [f11] 'toggle-fullscreen)



;;======== Buffer Move ========= 
(add-to-list 'load-path "~/.emacs.d/manual/")
(load "buffer-move.el")
(require 'buffer-move)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)



;;=========== Flyspell ========== 

;;enables per-file basis diabling of flyspell
;;use:

;;  /* Local Variables: */
;;  /* mode:org           */
;;  /* mode:my-no-flyspell */
;;  /* End:             */

(defun my-no-flyspell-mode (&optional rest)
  (flyspell-mode -1))




;;========Color Themes========= 

(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)

; (add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs/")

(add-to-list 'load-path "~/.emacs.d/color-theme-sanityinc-solarized")
(require 'color-theme-sanityinc-solarized)

;; Not working
;;(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
;;(require 'color-theme-solarized)


;;============Orgmode===========

(setq org-startup-indented t)   ;;default indent mode    
(setq org-log-done 'time)       ;;logging when tasks are done

;;(add-to-list 'load-path "~/.emacs.d/org-mode-customs")
;;(require 'org-expiry) 
;;(org-expiry-insinuate) 
;;(setq org-expiry-inactive-timestamps t)

;;enables RefTeX, from http://orgmode.org/worg/org-faq.html
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c [") 'reftex-citation))
(add-hook 'org-mode-hook 'org-mode-reftex-setup)



;;============AucTex=========== 

;;(load "auctex.el" nil t t)

(setq TeX-auto-save t)
;;(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)

;;enables RefTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("~/library/references/main.bib"))

;;Biblatex citing
;;http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands
(eval-after-load 'reftex-vars
  '(progn
     ;; (also some other reftex-related customizations)
     (setq reftex-cite-format
           '((?\C-m . "\\cite[]{%l}")
             (?f . "\\footcite[][]{%l}")
             (?t . "\\textcite[]{%l}")
             (?p . "\\parencite[]{%l}")
             (?o . "\\citepr[]{%l}")
             (?n . "\\nocite{%l}")))))



;;============Latexmk=========== 


(defun run-latexmk ()
  (interactive)
  (let ((TeX-save-query nil)
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file)))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk"
                 (TeX-command-expand "latexmk %t" 'TeX-master-file)
                 master-file)
    (if (plist-get TeX-error-report-switches (intern master-file))
        (TeX-next-error t)
      (minibuffer-message "latexmk done"))))

(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "C-0") #'run-latexmk)))


;;from http://stackoverflow.com/questions/7860547/is-there-a-way-to-prevent-font-locking-from-changing-the-font-family-and-change 
;; Only change sectioning colour
(setq font-latex-fontify-sectioning 'color)
;; super-/sub-script on baseline
(setq font-latex-script-display (quote (nil)))
;; Do not change super-/sub-script font
(custom-set-faces
 '(font-latex-subscript-face ((t nil)))
 '(font-latex-superscript-face ((t nil)))
 )
;; Exclude bold/italic from keywords
(setq font-latex-deactivated-keyword-classes
    '("italic-command" "bold-command" "italic-declaration" "bold-declaration"))



;;============Package Manager=========== 

;;http://ergoemacs.org/emacs/emacs_package_system.html


(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )



;;============ Web Mode =========== 

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode)) 
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode)) 
;; (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode)) 
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) 
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode)) 
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;; (defun my-web-mode-hook () 
;;   "Hooks for Web mode." 
;;   ;;(setq web-mode-markup-indent-offset 2)
;;   (define-key web-mode-map (kbd "C-c ;") 'web-mode-comment-or-uncomment) 
;; )
;; (add-hook 'web-mode-hook 'my-web-mode-hook)



;;Defunct:

;;(defun python-mode-theme-hook ()
  ;;(color-theme-initialize)
  ;;(color-theme-sanityinc-solarized))

;;(add-hook 'python-mode-hook
   ;;'ptyhon-mode-theme-hook)









;;; init.el ends here


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
;;  '(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
