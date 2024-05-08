
;; jeff's universal emacs file
;; this works with emacs 25 and 26. probably not any earlier.
;;
;; ~/.emacs file should look like:

;; (add-to-list 'load-path "~/emacs-jeff")
;; ;; uncomment line below to skip check
;; ;;(setq jc-skip-install-package-dependencies t)
;; (load-library "jeff")

;; 25MB threshold for gc; this can speed up startup time a bit
(set 'gc-cons-threshold 25000000)

(defun jc-is-cygwin-p ()
  "returns t if cygwin version of emacs"
  (equal system-type 'cygwin))

(defun jc-is-native-windows-p ()
  "returns t if native windows emacs"
  (equal system-type 'windows-nt))

(defun jc-is-windows-p ()
  (or (jc-is-cygwin-p) (jc-is-native-windows-p)))


(defun jc-require-if-exists (library)
  (condition-case nil
      (require library)
    (error "Could not load library %s" (symbol-name library))))

;; do this very early on
(let* ((git-bash-bin-dir "C:\\Program Files\\Git\\usr\\bin"))
  (when (and (jc-is-windows-p) (file-exists-p git-bash-bin-dir))
    (setenv "PATH" (concat "C:\\Program Files\\Git\\usr\\bin;" (getenv "PATH")))
    (setq epg-gpg-program "C:/Program Files/Git/usr/bin/gpg.exe")))

;; when installing, some packages whine about not being able to find
;; cl even though it's part of stock emacs23; this might help?
(require 'cl)

(if (<= emacs-major-version 23)
    (add-to-list 'load-path (concat (file-name-directory load-file-name) "emacs23")))
;; melpa is supposedly the latest and greatest; flycheck needs let-alist from elpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; emacs 25 on ubuntu 18.04 has outdated URL for 'gnu' so override it
(if
    (equal emacs-major-version 25)
    (setq package-archives
          (mapcar
           (lambda (item)
             (if (equal (car item) "gnu")
                 '("gnu" . "https://elpa.gnu.org/packages/")
               item))
           package-archives)))

(package-initialize)

(defun jc-package-exists-p (package-name)
  "Returns non-nil if package exists in any archive"
  (member package-name
          (mapcar (lambda (elt) (first elt)) package-archive-contents)))

(defun jc-install-package-dependencies (packages)
  "I adapted this from somewhere I can't remember"

  ;; if signature checking fails, disable keyring and try again
  ;; hacky error checking, b/c I couldn't get condition-case to catch the error from package-refresh-contents
  (progn
      (package-refresh-contents)
      (let*
          ((error-buffer (get-buffer "*Error*"))
           (error-string (when error-buffer (with-current-buffer error-buffer (buffer-string)))))
        (if (string-match-p "Failed to verify signature archive-contents.sig" (or error-string ""))
            (progn
              (message "trying package-refresh-contents again with signature checking turned off")
              (setq package-check-signature nil)
              (package-refresh-contents)
              (package-install 'gnu-elpa-keyring-update)
              (gnu-elpa-keyring-update)
              (setq package-check-signature 'allow-unsigned)))))

  (mapcar
   (lambda (package)
     (if (jc-package-exists-p package)
         (if (package-installed-p package)
             package
;           (if (y-or-n-p (format "Package %s is missing. Install it? " package))
               (package-install package)
					;             nil))
	       )
       (message "WARNING: package %s doesn't exist" (list package))))
   packages))

(defcustom jc-use-eshell nil
  "use eshell for F-key shells"
  :type 'boolean
  :group 'jc)

(defcustom jc-use-windows-git-bash t
  "use git-bash (Git for Windows) for shells"
  :type 'boolean
  :group 'jc)

(defcustom jc-font "Source Code Pro 11"
  "Font to use"
  :type '(string)
  :group 'jc)

(defcustom jc-use-per-hostname-session-files t
  "Keep per-hostname files for modules that keep state (desktop, recentf, etc). Set to nil on machines whose hostnames change frequently."
  :type 'boolean
  :group 'jc)

(defcustom jc-skip-install-package-dependencies nil
  "Install package dependencies required by jeff.el when loading that library"
  :type 'boolean
  :group 'jc)

(defcustom jc-package-dependencies 
  '(cl-lib
    yasnippet 
    rnc-mode
    expand-region
    auto-complete
    color-theme-modern
    tt-mode
    psvn
    markdown-mode
    markdown-toc
    bash-completion
    restclient
    yaml-mode
    js2-mode
    web-mode
    flycheck
    php-mode
    haskell-mode
    scala-mode
    ruby-mode
    rvm
    robe
    web
    ess
    powershell
    )
  "package dependencies required by jeff.el"
  :type '(repeat symbol)
  :group 'jc)

(defgroup jc nil
  "Jeff's customizations"
  :group 'emacs)

(if (not jc-skip-install-package-dependencies)
    (jc-install-package-dependencies jc-package-dependencies))


;;;; Key bindings

;; for emacs on Mac OS: make Command key be meta instead of stupid
;; Option key
(setq mac-command-modifier 'meta)

;; use a login shell to get full environment
(require 'shell)
(add-to-list 'explicit-bash-args "--login")
(when (equal system-type 'darwin)
  (setq explicit-shell-file-name "/bin/bash"))

;; for emacs running in Debian VM in Mac OS
(setq x-super-keysym 'meta)

(defun jc-shell (shell-buffer-name)
  (let* ((shell-function (if jc-use-eshell 'jc-eshell 'shell)))
    (apply shell-function (list shell-buffer-name))))
       
(defun jc-eshell (shell-buffer-name) (interactive)
       (if (get-buffer shell-buffer-name)
           (switch-to-buffer shell-buffer-name)
         (let* ((eshell-buffer-name shell-buffer-name))
           (eshell)
           (rename-buffer shell-buffer-name))))

(defun jc-shell-number1 nil (interactive) (jc-shell "*shell1*"))
(defun jc-shell-number2 nil (interactive) (jc-shell "*shell2*"))
(defun jc-shell-number3 nil (interactive) (jc-shell "*shell3*"))

;; adapted from https://archive.casouri.cc/note/2021/clean-exit/index.html
(defun jc-clean-exit ()
  "Exit Emacs cleanly.
If there are unsaved buffer, pop up a list for them to be saved
before existing. Replaces ‘save-buffers-kill-terminal’."
  (interactive)
  (if (frame-parameter nil 'client)
      (server-save-buffers-kill-terminal arg)
    (if-let ((buf-list (seq-filter (lambda (buf)
                                     (and (buffer-modified-p buf)
                                          (not (string-match "^\*" (string-trim-left (buffer-name buf)))))
                                     )
                                   (buffer-list))))
        (progn
          (pop-to-buffer (list-buffers-noselect t buf-list))
          (message "s to save, C-k to kill, x to execute"))
      (save-buffers-kill-emacs))))

;; for explanation of cryptic format for specifying keys, see:
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-Rebinding.html
(dolist
    (record 
     ;; two keybindings each: one works locally on Apple keyboard at
     ;; work, the other from home laptop
     (list '([XF86MonBrightnessDown] jc-shell-number1)
           '([f1] jc-shell-number1)
           '([XF86MonBrightnessUp] jc-shell-number2)
           '([f2] jc-shell-number2)
           '([XF86LaunchA] jc-shell-number3)
           '([f3] jc-shell-number3)))
  ;; can't call (kbd) function because it expects a string constant
  (global-set-key (first record) (second record)))

;; ctrl-tab and ctrl-shift-tab = cycle through buffers
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)

;; diff modified buffer against current file
(global-set-key "\C-x=" 'jc-diff-buffer-with-file-current)

;; Meta-Up deletes indentation on current line, wrapping it to line above
(global-set-key (kbd "M-<up>") 'delete-indentation)

;; show modified buffers (including non-file buffers) before exiting
(global-set-key (kbd "C-x C-c") 'jc-clean-exit)


;;;; Tweak built-in emacs settings

(setq-default column-number-mode t)

(setq-default tab-width 4)

(setq cperl-indent-level 4)

(setq c-basic-offset 4)

(setq-default indent-tabs-mode nil)

;; don't make silly backup files
(setq make-backup-files nil) 

;; make copy/paste play nice with other apps in X11)
(setq x-select-enable-clipboard t)

;; show matching parentheses
(show-paren-mode t)

;; why not?
(setq max-lisp-eval-depth 1000)

;; prevent stupid bell noise on windows
(setq ring-bell-function 'ignore)

;; sort by filename
(if (<= emacs-major-version 23)
  (setq Buffer-menu-sort-column 5))

;; update buffers automatically when file changes on disk
(global-auto-revert-mode t)

(tool-bar-mode 0)

(menu-bar-mode 0)

(scroll-bar-mode 0)

(global-hl-line-mode 1)

(blink-cursor-mode 1)

;; this makes yanking overwrite text in region, if region is active
(delete-selection-mode 1)

(setq-default buffer-file-coding-system 'utf-8-unix)

;; always split windows vertically
(setq split-height-threshold nil)
(setq split-width-threshold 120)

;; make "Ctrl-k" kill an entire line if the cursor is at the beginning of line
(setq kill-whole-line t)

;; make keypad enter numbers
(setq keypad-numlock-setup 2)

(setq message-log-max 5000)

(setq recentf-max-saved-items 500)

(setq grep-program "egrep")

;; trust all themes (gulp)
(setq custom-safe-themes t)

;;(defalias 'perl-mode 'cperl-mode)
(custom-set-variables
 '(cperl-indent-parens-as-block t))

;; when True, cperl-indent-parens-as-block does what I want for
;; indenting contents of literal hashes, but doesn't do what I want
;; for indenting
(setq cperl-continued-statement-offset 4)


;;;; Require and configure packages

(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 4)))

(defun jc-nxml-setup ()
  (setq tab-width 4
        indent-tabs-mode nil
		nxml-child-indent 4
        sgml-indent-step 4
		sgml-basic-offset 4
        nxml-slash-auto-complete-flag t))

(add-hook 'nxml-mode-hook 'jc-nxml-setup)

(defun jc-c++-indent-setup ()
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil))

(add-hook 'c++-mode-hook 'jc-c++-indent-setup)

(require 'nxml-mode)
(require 'rng-nxml) ;; emacs23 needs this separate require to get rng-schema-locating-files variable
(add-to-list 'rng-schema-locating-files
             (concat (file-name-directory load-file-name) "schemas.xml"))

;; emacs23.1.1 weirdly associates xml-mode with some file types by
;; default, instead of nxml-mode, so override them
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))

(require 'tt-mode)
(add-to-list 'auto-mode-alist '("\\.tt$" . tt-mode))

(require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory
;;  (concat (file-name-directory load-file-name) "snippets"))

;; for viewing svn history
(require 'log-view)

;; make buffer list show unique entries for identical filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; for this to work, do NOT call ansi-color-for-comint-mode-on directly.
;; not sure why that messes up colors.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; extras for dired mode
(require 'dired-x)

(when jc-use-per-hostname-session-files
  (setq ido-save-directory-list-file (concat "~/.emacs.d/.ido.last." system-name)))

;; tab completion for find-file and buffers
(ido-mode t)

(defadvice ido-find-file (before my-advice)
  "if we're in a shell buffer (as determined by name of shell), switch to other window"
  (let ((buffer-name-length (length (buffer-name))))
    (when (and (> buffer-name-length 6)
               (equal (substring (buffer-name) 0 6) "*shell"))
      (other-window 1))))
(ad-activate 'ido-find-file)

(add-hook 'python-mode-hook
          (lambda ()
              (setq show-trailing-whitespace t)))

(add-hook 'perl-mode-hook
          (lambda ()
              (setq show-trailing-whitespace t)))

(add-to-list 'auto-mode-alist '("\\.rabl$" . ruby-mode))
(add-hook 'ruby-mode-hook
          (lambda ()
              (setq show-trailing-whitespace t)))

(add-hook 'ruby-mode-hook 'flycheck-mode)

(add-hook 'ruby-mode-hook 'robe-mode)

(add-hook 'robe-mode-hook 'ac-robe-setup)

(add-hook 'web-mode-hook
          (lambda ()
              (setq show-trailing-whitespace t)))

;; after list-buffers is called, switch to it
(defadvice list-buffers (after jc-switch-to-other-win)
  (if (not (equalp (buffer-name (current-buffer))
                   "*Buffer List*"))
      (other-window 1))
  (goto-char (+ 4 (point))))

;; emacs24 doesn't recognize Buffer-menu-sort-column so we do this
;; nonsense: after list-buffers is called and we've switched to it,
;; check whether the buffer matches what's stored in
;; jc-buffer-menu. If it doesn't match, it means it's new, so call
;; Buffer-menu-sort and update jc-buffer-menu so we don't sort again
;; on subsequent calls.
(when (>= emacs-major-version 24)
  (setq jc-buffer-menu nil)
  (defadvice list-buffers (after jc-buffer-menu-sort last)
    (when (not (equal jc-buffer-menu (current-buffer)))
      (setq jc-buffer-menu (current-buffer))
      ;; for debugging:
      ;;(message "sorting!")
      (Buffer-menu-sort 6))))
(ad-activate 'list-buffers)

;; for viewing svn history
(require 'log-view)

;; make buffer list show unique entries for identical filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq interpreter-mode-alist (cons '("python" . python-mode)
								   interpreter-mode-alist))

(defun jc-python-indent-setup ()
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil))
(add-hook 'python-mode-hook 'jc-python-indent-setup)

(setq python-fill-docstring-style 'django)

;; subversion integration
(jc-require-if-exists 'psvn)

(require 'recentf)
(when jc-use-per-hostname-session-files
  (setq recentf-save-file (concat "~/.emacs.d/.recentf." system-name)))
(recentf-mode t)
(global-set-key "\C-x\C-r" 'recentf-open-files)

(defun jc-htmlconv ()
  (interactive)
  (goto-char 1)
  (replace-string "<" "&lt;")
  (goto-char 1)
  (replace-string ">" "&gt;")
  (goto-char 1))

(defun jc-diff-buffer-with-file-current nil
  (interactive)
  (if (buffer-modified-p (current-buffer))
      (diff-buffer-with-file (current-buffer))
    (message "Buffer %s hasn't been modified, nothing to diff" 
             (buffer-name (current-buffer)))))

(defun jc-is-emacs-daemon-mode () 
  (interactive)
  ;; returns t if current process is 'emacs --daemon' or 'emacsclient'
  (let ((ps-output
        (shell-command-to-string 
         (concat "ps -f -p " (int-to-string (emacs-pid))))))
    (not (null
          (or (string-match "--daemon" ps-output)
              (string-match "emacsclient" ps-output))))))

;; THIS DOESNT WORK IN DAEMON MODE
(defun jc-is-vpn-connection ()
  ;; IP address prefix assigned when connecting via vpn
  (let* ((ssh_connection (getenv "SSH_CONNECTION"))
         (ip_substring (when ssh_connection (substring ssh_connection 0 7))))
    (equal "10.20.1" ip_substring)))

;; (require 'ack)
;; (autoload 'pcomplete/ack "pcmpl-ack")
;; (autoload 'pcomplete/ack-grep "pcmpl-ack")
;; (defun jc-ack-default-directory (arg)
;;   "Always prompt for directory"
;;   (let* ((dir (read-directory-name "In directory: " default-directory nil t))
;;         (dir-with-slash (if (equal (substring dir -1) "/")
;;                             dir
;;                           (concat dir "/"))))
;;     (message (concat "DIR: " dir-with-slash))
;;     dir-with-slash))
;; (setq ack-default-directory-function 'jc-ack-default-directory)

(defun x-server-is-connected ()
  "Returns t if emacs is connected to an X display"
  (> (length (x-display-list)) 0))

(defun x-server-is-xming ()
  "Returns t if X server is XMing (ie if X display is Windows) 

This function is useful because x-server-vendor gives warning if no X, so we test for X first."
  (when (x-server-is-connected)
    (equal 0 (string-match "Colin Harrison" (x-server-vendor)))))

;; NOTE: js2 has some nice features, like syntax checking but it has
;; HORRIBLE indentation.
;;(load "js2")
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; python-mode is different from python.el packaged with emacs, but it
;; doesnt seem to work all that well. sigh.
;;(setq py-install-directory "~/.emacs.jeff/")
;;(require 'python-mode)

(defun scheme-eval-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (call-interactively 'scheme-send-region)))

(setq desktop-restore-frames nil)

;; save emacs buffers state
(when (null (getenv "EMACS_DISABLE_DESKTOP_SAVE_MODE"))
  (setq desktop-path (list "~/.emacs.d"))

  (when jc-use-per-hostname-session-files
    ;; use diff desktop files per host
    (setq desktop-base-file-name (concat ".emacs.desktop." system-name))
    (setq desktop-base-lock-name (concat ".emacs.desktop.lock." system-name)))
  
  (desktop-save-mode 1)

  (defun jc-autosave-desktop ()
    ;; don't call desktop-save-in-desktop-dir b/c that annoyingly
    ;; prints a message
    (desktop-save desktop-dirname))
  (add-hook 'auto-save-hook 'jc-autosave-desktop))

;; for editing rnc validation files
(jc-require-if-exists 'rnc-mode)
(add-to-list 'auto-mode-alist '("\\.rnc$" . rnc-mode))

;; this doesn't seem actually useful
;;(require 'django-html-mode)

(when (file-exists-p "~/go/misc/emacs")
  (add-to-list 'load-path "~/go/misc/emacs")
  (load-library "go-mode-load")
  (defun go ()
    "run current buffer"
    (interactive) 
    (compile (concat "go run " (buffer-file-name)))))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(defun jc-which-python ()
  "returns location of the python executable in PATH"
  (let*
      ((executables '("python" "python3")))
    (seq-reduce
     (lambda (acc exec)
       (or acc
           (let*
               ((raw-result
                 (string-trim (shell-command-to-string (concat "bash -c 'which " exec "'")))))
             (if (> (length raw-result) 0) raw-result nil))))
     executables nil)))

(jc-which-python)

(defun sql-parse (&optional b e) 
  (interactive "r")
  (shell-command-on-region
   b e
   (concat (jc-which-python) " -c \"import os; os.environ['PYTHONIOENCODING'] = 'UTF-8'; import sqlparse, sys; print(sqlparse.format(sys.stdin.read(), reindent=True, keyword_case='upper'))\"")
   nil t))
(defalias 'sql-format 'sql-parse)

(require 'grep)
(add-to-list 'grep-find-ignored-files "*.pdf")

;(add-to-list 'grep-find-ignored-files "*.xsd")

;; (defun jc-grep-finish (buffer finish-state)
;;   (when (equal "*grep*" (buffer-name buffer))
;;     (let* ((grep-output
;;             (with-current-buffer buffer (buffer-string)))
;;            (filtered-output
;;             (with-temp-buffer
;;               (insert grep-output)
;;               (goto-line 1) (kill-line)
;;               (goto-line 3) (kill-line)
;;               (buffer-string))))
;;       (bds-feed-builder "WHOA" filtered-output)
;;       (kill-buffer "*grep*")
;;       (with-current-buffer (get-buffer "WHOA")
;;         (grep-mode)))))

;; (add-to-list 'compilation-finish-functions 'jc-grep-finish)

(defun json-prettify (&optional b e) 
  (interactive "r")
  (shell-command-on-region b e (concat (jc-which-python) " -m json.tool") nil t ))

(defun xml-prettify (&optional b e) 
  (interactive "r")
  (shell-command-on-region b e "xmllint --format -" nil t ))

;; only need this advice if we want to be able to switch grep-program
;; dynamically
;;
;; (defadvice grep-compute-defaults 
;;   (before grep-compute-defaults-everytime ())
;;   " reset grep-find-template and grep-host-defaults-alist every time grep-compute-defaults runs "
;;   (setq grep-find-template nil)
;;   (setq grep-host-defaults-alist nil))

;; (ad-activate 'grep-compute-defaults)

(defalias 'perldoc 'cperl-perldoc)

(defun jc-perl-package-from-path (path)
  "If specified path is a perl package (.pm file), returns the perl package name. otherwise returns nil."
  (let* ((start-pos-lib (string-match "v2_main/lib" path))
         (start-pos-pm (string-match "\.pm" path))
         (perl-package-slashes 
          (when (and start-pos-lib start-pos-pm) 
            (substring path (+ start-pos-lib 12) start-pos-pm)))
         (perl-package 
          (when perl-package-slashes
            (replace-regexp-in-string "/" "::" perl-package-slashes))))
    perl-package))

(defadvice rename-buffer (after rename-buffer-bds-perl last activate)
  "advice for rename-buffer"
  (when (not (null (ad-get-arg 1)))
    (let* ((filename (buffer-file-name (get-buffer (ad-get-arg 0))))
           (package-name (when filename (jc-perl-package-from-path filename))))
      (when package-name
        (setq ad-return-value package-name)))))

(defadvice create-file-buffer (after create-file-buffer-bds-perl last activate)
  "advice for create-file-buffer"
  (let* ((filename (expand-file-name (directory-file-name (ad-get-arg 0))))
         (package-name (when filename (jc-perl-package-from-path filename))))
    (when package-name
      (with-current-buffer ad-return-value
        (rename-buffer package-name t)))))

;; WARNING: csharp-mode caused LOTS of problems for me; emacs would
;; freeze sometimes

;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;; (setq auto-mode-alist
;;       (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; (defun my-csharp-mode-fn ()
;;   "function that runs when csharp-mode is initialized for a buffer."
;;   (turn-on-auto-revert-mode)
;;   (setq indent-tabs-mode nil)
;;   (require 'flymake)
;;   (flymake-mode 1)
;;   (require 'yasnippet)
;;   (yas/minor-mode-on))
;; (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

(defun jc-projector ()
  "Do stuff to make emacs display reasonably on overhead projector"
  (interactive)
  (global-hl-line-mode 0)
  (color-theme-emacs-21))

(defun jc-list-fonts ()
  "List available font family names in a buffer called *fonts*"
  (interactive)
  (switch-to-buffer "*fonts*")
  (erase-buffer)
  (insert
   (apply 'concat
          (mapcar
           (lambda (item) (concat item "\n"))
           (font-family-list))))
  (sort-lines nil 1 (buffer-size (get-buffer "*fonts*")))
  (shell-command-on-region
   1 (buffer-size (get-buffer "*fonts*"))
   "uniq"
   "*fonts*" t))

(require 'auto-complete)
(global-auto-complete-mode t)

(defun autopep8 (&optional b e)
  (interactive "r")
  (shell-command-on-region
   b e
   "autopep8 -"
   nil t))

;; (defun fix-unused-imports (&optional b e)
;;   (interactive "r")
;;   (shell-command-on-region
;;    b e
;;    "autoflake - --remove-unused-variables"
;;    nil t))

(require 'web-mode)
;; make these faces resemble html-mode b/c it's what I'm used to
(set-face-attribute 'web-mode-html-tag-face nil :foreground "red")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#729fcf")

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(setq web-mode-engines-alist
      '(("blade"  . "\\.blade\\."))
)

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;;;; Startup initialization

(defun jc-after-make-frame (frame)
  ;; function to run as part of after-make-frame-functions hook

  (if (x-server-is-connected)
      (progn
        (select-frame frame)

        (let* ((display-width (x-display-pixel-width)))
          (cond 
           ;; HD desktop
           ((member display-width '(1680 1920))
            (progn
              (set-frame-height frame 52)
              (set-frame-width frame 164)))
           ;; pc laptop
           ((= 1366 display-width) 
            (progn
              (set-frame-height frame 35)
              (set-frame-width frame 90)))
           ;; macbook
           ((and (equal system-type 'darwin) (= 1280 display-width)) 
            (progn
              (set-frame-height frame 45)
              (set-frame-width frame 100)))))

        ;; pre-builtin theme support in emacs
        ;;(load-library "color-theme-tango")
        ;;(color-theme-tango)

        ;; this only looks good in X windows
        (load-theme 'deeper-blue)
        ;; set window fringe to have same color as background for 'default' face
        ;; many themes set it to some other color, which is annoying
        (set-face-background 'fringe (face-attribute 'default :background))

        ;; NOTE: .ttf and .otf font files should live in ~/.fonts/
        ;;
        ;; Other reasonable fonts:
        ;;
        ;; (set-frame-font "Anonymous Pro-12")
        ;; (set-frame-font "DejaVu Sans Mono-12")
        ;; (set-frame-font "Hack-12")
        ;; (set-frame-font "Inconsolata-13")
        ;; (set-frame-font "Monospace-11")
        ;; (set-frame-font "Source Code Pro-12")
        ;;
        ;; run this on Mac OS to disable antialiasing; set the value low (e.g. 10) to ENABLE antialiasing
        ;; defaults write org.gnu.Emacs AppleAntiAliasingThreshold 100
        (condition-case nil
            (cond 
             ((x-server-is-xming)
              (set-frame-font "Inconsolata-15"))
             ((jc-is-windows-p)
              (set-frame-font "Consolas-11"))
             ((and (equal system-type 'darwin) (= (x-display-pixel-width) 1680))
              ;; external display at work
              (set-frame-font "Source Code Pro-13")
              )
             ((equal system-type 'darwin)
              ;; I like this best for Retina display; it's not so good
              ;; on an external monitor
              (setq mac-allow-anti-aliasing nil)
              (set-face-italic-p 'italic nil)
              (set-frame-font "Monaco-13"))
             (t
              (set-frame-font jc-font)))
          (error (message "ERROR: failed to set frame font, probably because font isn't installed. Ignoring."))))
    ;; else

    ;; TODO: if TERM isn't 256 color, show warning.but we can't use
    ;; getenv on TERM because emacs always returns 'dumb'. ugh.

    (require 'ansi-color)
    (load-theme 'tty-dark))

  ;; for this to work, do NOT call ansi-color-for-comint-mode-on directly in extras.el.
  ;; not sure why that messes up colors.
  ;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (message (concat "This emacs has been running " (emacs-uptime))))

(if (jc-is-emacs-daemon-mode)
    (add-hook 'after-make-frame-functions 'jc-after-make-frame)
  ;; else
  (jc-after-make-frame (selected-frame)))


;; for bash completion in shell mode
(autoload 'bash-completion-dynamic-complete 
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
  'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
  'bash-completion-dynamic-complete)

(setq enable-local-variables :all)

(setq ruby-deep-indent-paren nil)

(setenv "PAGER" "cat")

(defun unescape-newlines (&optional b e)
  "replaces '\n' in a region with actual newlines"
  (interactive "r")
  (replace-string "\\n" "\n" nil b e))

;; decrement index and drop entries where level == 0
(custom-set-variables
 '(markdown-toc-user-toc-structure-manipulation-fn
   (lambda (toc-structure)
     (delq nil
           (mapcar (lambda (record)
                     (when (> (car record) 0)
                       (cons (- (car record) 1) (cdr record))))
                   toc-structure)))))

;; can't remember what these were for, but windows git-bash works better without it
;; (if (jc-is-windows-p)
;;     (progn
;;       (setq vc-git-diff-switches nil)
;;       (setq vc-diff-switches "-u -w")))

(setq vc-git-print-log-follow t)

(when (and (jc-is-native-windows-p) jc-use-windows-git-bash)
  ;; don't need to set explicitly if git-bash install put git in the
  ;; path; TODO: put a check for that here
  ;;(setq vc-git-program "C:\\Program Files\\Git\\bin\\git.exe")
  (setq explicit-shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe")
  (setq bash-completion-prog "C:\\Program Files\\Git\\bin\\bash.exe")
  ;; -i isn't in bash-completion-args by default but without it, bash doesn't respond
  (setq bash-completion-args '("--noediting" "-i")))

;; when AutoHotkey is used to map the Windows Key to Alt system-wide,
;; Windows still intercepts certain Windows Key combos unless we make
;; these register calls.
;;
;; The AutoHotkey .ahk file can simply have these 2 lines:
;; LWin::LAlt
;; LAlt::LWin
(when (jc-is-native-windows-p)
  (w32-register-hot-key [M-w]) ;; most important one: bound to kill-ring-save
  (w32-register-hot-key [M-r])
  (w32-register-hot-key [M-t])
  (w32-register-hot-key [M-g])
  (w32-register-hot-key [M-m])
  ;; this one doesn't seem to work, not sure if it's bound in emacs anyway
  ;;(w32-register-hot-key [M-b])
  )

;; prevent annoying mods to ~/.emacs
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
       (load custom-file))
