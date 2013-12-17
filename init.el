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
(global-linum-mode 1)                   ;;global line numbers
(column-number-mode 1)                  ;;columns
(scroll-bar-mode -1)                    ;;no scrollbar
(global-set-key "\C-z" nil)             ;;no minimize



;;========Color Themes========= 

(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)

(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs/")

(add-to-list 'load-path "~/.emacs.d/color-theme-sanityinc-solarized")
(require 'color-theme-sanityinc-solarized)


;;============Orgmode===========

(setq org-startup-indented t)   ;;default indent mode    




;;============AucTex=========== 

;;(load "auctex.el" nil t t)

(setq TeX-auto-save t)
;;(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)


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



;;Defunct:

;;(defun python-mode-theme-hook ()
  ;;(color-theme-initialize)
  ;;(color-theme-sanityinc-solarized))

;;(add-hook 'python-mode-hook
   ;;'ptyhon-mode-theme-hook)

;;(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
;;(require 'color-theme-solarized)

;;(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized-emacs/")





;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
