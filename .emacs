;; show time
(display-time-mode t)

;; disable bell function
(setq ring-bell-function 'ignore)

;; disable toolbar
(tool-bar-mode -1)

;; disable scrollbar
(toggle-scroll-bar -1)

;; disable splash screen
(custom-set-variables '(inhibit-startup-screen t))

;; current buffer name in title bar
(setq frame-title-format "%b")

;; show paren
(show-paren-mode t)

;; toggle-truncate-lines
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)

;; start emacs server
(server-start)

;; set font
(if (>= emacs-major-version 23)
    (progn
      ;(set-default-font "Monospace-9")
      ;(set-default-font "Monaco-10")
      ;(set-default-font "Meslo LG M DZ-10")
      (set-default-font "Inconsolata-12")
      ))

;; clojure-mode
;; git clone git://github.com/technomancy/clojure-mode.git
;; cd ~/clojure-mode/git pull 
(add-to-list 'load-path "~/clojure-mode")
(require 'clojure-mode)

;; paredit
;; wget http://mumble.net/~campbell/emacs/paredit.el
;; 
(add-to-list 'load-path "~/paredit")
(require 'paredit)
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook
	  'override-slime-repl-bindings-with-paredit)

;; SLIME 
;; ~$ sudo add-apt-repository "deb http://archive.canonical.com/ lucid partner"
;; ~$ sudo apt-get update
;; ~$ sudo apt-get install sun-java6-jdk
;; ~$ sudo apt-get install ant
;; ~$ sudo su
;; # cd /bin
;; # wget https://github.com/technomancy/leiningen/raw/stable/bin/lein
;; # chmod +x lein
;; # exit
;; ~$ lein self-install
;; http://alexott.net/ru/clojure/ClojureLein.html
;; http://technomancy.us/149
;; http://zef.me/2470/building-clojure-projects-with-leiningen
;; http://data-sorcery.org/2009/11/20/leiningen-clojars/
(setq slime-net-coding-system 'utf-8-unix)
;; M-x eldoc-mode
(setq slime-use-autodoc-mode t) ; nil

;;; http://stackoverflow.com/questions/2474804/is-there-a-colored-repl-for-clojure
;;; https://gist.github.com/337280

;;; all code in this function lifted from the clojure-mode function
;;; from clojure-mode.el
(defun clojure-font-lock-setup ()
  (interactive)
  (set (make-local-variable 'lisp-indent-function)
       'clojure-indent-function)
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'clojure-doc-string-elt)
  (set (make-local-variable 'font-lock-multiline) t)

  (add-to-list 'font-lock-extend-region-functions
               'clojure-font-lock-extend-region-def t)

  (when clojure-mode-font-lock-comment-sexp
    (add-to-list 'font-lock-extend-region-functions
                 'clojure-font-lock-extend-region-comment t)
    (make-local-variable 'clojure-font-lock-keywords)
    (add-to-list 'clojure-font-lock-keywords
                 'clojure-font-lock-mark-comment t)
    (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil))

  (setq font-lock-defaults
        '(clojure-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function))))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            ;(font-lock-mode nil)
            (clojure-font-lock-setup)
            ;(font-lock-mode t)
            ))

(defadvice slime-repl-emit (after sr-emit-ad activate)
  (with-current-buffer (slime-output-buffer)
    (add-text-properties slime-output-start slime-output-end
                         '(font-lock-face slime-repl-output-face
                                          rear-nonsticky (font-lock-face)))))

(defadvice slime-repl-insert-prompt (after sr-prompt-ad activate)
  (with-current-buffer (slime-output-buffer)
    (let ((inhibit-read-only t))
      (add-text-properties slime-repl-prompt-start-mark (point-max)
                           '(font-lock-face slime-repl-prompt-face
                                            rear-nonsticky
                                            (slime-repl-prompt
                                             read-only
                                             font-lock-face
                                             intangible))))))
;; $ sudo apt-get install emacs-goodies-el
(add-to-list 'load-path "~/emacs-color-theme-solarized")
(require 'color-theme)
;; git clone git://github.com/plathrop/emacs-color-theme-solarized.git
;; http://ethanschoonover.com/solarized
(add-to-list 'load-path "~/dotfiles/solarized/emacs-colors-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-dark)
;(require 'color-theme-billw)
;; 
(add-to-list 'load-path "~")
(require 'line-num)			; M-x linum-mode
;;To make html files from clojure files, use M-x htmlize-file

;;(global-hl-line-mode 1)






