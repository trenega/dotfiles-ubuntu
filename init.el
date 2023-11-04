;;;; nis's ~/.emacs.d/init.el
;; Takashi Niijima
;; 2023-09-05

;;;;------------------------------------------------------
;;;; Define function
;;;;------------------------------------------------------

;;; load-pathを追加する関数を定義
;;  refer: 「Emacs実践入門」大竹智也 p.61
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
             (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
;; ~/.emacs.d/straight/repos/melpa/recipes
;; ~/.emacs.d/public-repos
(add-to-load-path "straight/repos/melpa/recipes"
                  "public-repos")

;;; other-window-backwordを定義する
;; refer: 「GNU Emacs 拡張ガイド」p.16
(defun other-window-backword (&optional n)    ; version 3
  "Select Nth previous window."
  (interactive "p")
  (if n
      (other-window (- n))  ; nがnilでないとき
    (other-window -1)))     ; nがnilのとき

;;; 一回に1行のスクロールする関数を定義する
;; 関数の名前をわかりやすい名前で参照する
;; refer: 「GNU Emacs 拡張ガイド」p.24
(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun scroll-one-line-ahead ()
  "Scroll ahead one line."
  (interactive)
  (scroll-ahead 1))

(defun scroll-one-line-behind ()
  "Scroll behind one line."
  (interactive)
  (scroll-behind 1))

;;; カーソルをウィンドウの左上隅にジャンプさせる関数
;; refer: 「GNU Emacs 拡張ガイド」p.26
(defun point-to-top ()
  "Put point on top line of window."
  (interactive)
  (move-to-window-line 0))

;;; カーソルをウィンドウの左下隅にジャンプさせる関数
;; refer: 「GNU Emacs 拡張ガイド」p.26
(defun point-to-bottom ()
  "Put point at beginning of last visible line."
  (interactive)
  (move-to-window-line -1))

;;; 現在カーソルのある行がウィンドウの最初の行になるようにスクロールさせる
;; refer: 「GNU Emacs 拡張ガイド」p.26
(defun line-to-top ()
  "Move current line to top of window."
  (interactive)
  (recenter 0))

;;;; End Define function----------------------------------

;;;;------------------------------------------------------
;;;; Initialization
;;;;------------------------------------------------------

;; font
(add-to-list 'default-frame-alist
             '(font . "UDEV Gothic NF-18"))  ; font size:18


;; Alt key -> Meta key setting
;; refer: https://qiita.com/hayamiz/items/0f0b7a012ec730351678
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))


;; 起動画面を表示しない
(setq inhibit-startup-screen t)

;; ビープ音禁止
;; http://yohshiy.blog.fc2.com/blog-entry-324.html
(setq ring-bell-function 'ignore)

;; splash screenを無効にする
(setq inhibitrsplash-screen t)

;; メニューバーとツールバーとスクロールバーを消す
;;(menu-bar-mode -1)
(menu-bar-mode t)
(tool-bar-mode -1)
;;(tool-bar-mode t)
(scroll-bar-mode -1)

;; scratch の初期メッセージ消去
(setq initial-scratch-message "")

;; タイトルバーにファイルのフルパスを表示する
(setq frame-title-format "%f")

;; 現在ポイントがある関数名をモードラインに表示
(which-function-mode 1)

;; カラム番号も表示する
(column-number-mode t)

;; png, jpg などのファイルを画像として表示
(setq auto-image-file-mode t)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; リージョンのハイライト
(transient-mark-mode 1)

;; インデントにTabを使わないようにする
(setq-default indent-tabs-mode nil)

;; ediff 時にフレームを使わない
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Diredを２画面ファイラーとして使う
(setq dired-dwim-target t)

;; rers: emacs.rubikitch.com/sd1407/
;; 右から左に読む言語に対応させないことで描写高速化
(setq-default bidi-display-reordering nil)

;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; I use 'eval-expressin'
;; ミニバッファでLisp式の入力を促し、与えられた式を評価して結果を表示する
(put 'eval-expression 'disabled nil)

;; C-U C-SPC C-SPC ...でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)

;; ログの記録行数を増やす
(setq message-log-max 10000)

;; 履歴をたくさん保存する
(setq history-length 1000)

;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; スクロールのステップ量
;; http://yohshiy.blog.fc2.com/blog-entry-320.html
(setq scroll-conservatively 1)

;; スクロール時の重複行数
(setq next-screen-context-lines 5)

;; ページスクロール時に画面上におけるカーソルの位置をなるべく変えない
(setq scroll-preserve-screen-position t)

;; 以前開いたファイルを再度開いた時、元のカーソル位置を復元する
;; refer: http://www.emacswiki.org/emacs/SavePlace
;; refer: emacs.rubikitch.com/save-place-mode-emacs25/
;; sakashita-net.jp/2017/08/emacs.html
(save-place-mode 1)

;; 複数のディレクトリで同じファイル名のファイルを開いた時のバッファ名を調整する
(require 'uniquify)  ;filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "[^*]+")

;;; relative numbering
;; refer: https://www.reddit.com/r/emacs/comments/l7f85b/how_to_toggle_absolute_and_relative_numbering_in/
(defun my/display-set-relative ()
  (setq display-line-numbers 'relative))    ; or 'visual

(defun my/display-set-absolute ()
  (setq display-line-numbers t))

(add-hook 'evil-insert-state-entry-hook #'my/display-set-absolute)
(add-hook 'evil-insert-state-exit-hook #'my/display-set-relative)


;;; clipboard Setting
;; Emacsから他のエディターにAlt+vでペーストはできるが、その逆にEmacsへは
;; ペーストできない。
;; refer: saitodev.co/article/Emacsでクリップボードを使ってコピペしたい/
(cond (window-system
  (setq x-select-enable-clipboard t)))

;;;; End Initialization------------------------------------

;;;;------------------------------------------------------
;;;; Package Manager Settings
;;;;------------------------------------------------------

;;; straight.el
;;  Next-generation, purely functional package manager for the Emacs hacker.
;;  refer: github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Emacs version 27 以上のユーザーは以下を追加
(setq package-enable-at-startup nil)

;; use-package との統合
(straight-use-package 'use-package)

;; el-patch
;; Future-proof your Emacs Lisp customizations!
;; Like the advice system, el-patch provides a way to customize
;; the behavior of Emacs Lisp functions that do not provide
;; enough variables and hooks to let you make them do what you
;; want.
(straight-use-package 'el-patch)

;;--------------------------------------------------------
;; To install a package Write Here!
;;--------------------------------------------------------


;; zenburn-theme
(straight-use-package 'zenburn-theme)

;; Evil
;; Evil is an extensible vi layer for Emacs.
;; Also see our page on EmacsWiki.
(straight-use-package 'evil)

;; ivy
;; Ivy - a generic completion frontend for Emacs,
(straight-use-package 'ivy)

;; swiper
;; Swiper - isearch with an overview, and more.
(straight-use-package 'swiper)

;; counsel
;; Just call one of the interactive functions in this file
;; to complete the corresponding thing using `ivy'.
(straight-use-package 'counsel)

;; dired-recent
;; A history of paths visited with Emacs dired.
(straight-use-package 'dired-recent)

;; bind-key
(straight-use-package 'bind-key)

;; key-chord
;; Map pairs of simultaneously pressed keys to commands
(straight-use-package 'key-chord)

;; evil-leader
(straight-use-package 'evil-leader)

;; company
(straight-use-package 'company)

;; comment-dwim-2
(straight-use-package 'comment-dwim-2)

;; flycheck
(straight-use-package 'flycheck)

;; flycheck-pos-tip
(straight-use-package 'flycheck-pos-tip)

;; flycheck-haskell
(straight-use-package 'flycheck-haskell)

;; darkroom
(straight-use-package 'darkroom)

;; tempbuf.el
;; 不要なバッファを自動的にkillしてくれる
;; (straight-use-package 'tempbuf)

;; smex
;; M-xを超強化するsmexパッケージ
(straight-use-package 'smex)

;; ido-vertical-mode
;; smexパッケージといっしょに使う
(straight-use-package 'ido-vertical-mode)

;; smartrep
;; プレフィクスキーを省略させる
;; ウィンドウ操作をひとまとめにする
(straight-use-package 'smartrep)

;; evil-surround
(straight-use-package 'evil-surround)

;; ediprolog
(straight-use-package 'ediprolog)

;; clojure-mode
(straight-use-package 'clojure-mode)

;; cider
;; CIDER extends Emacs with support for interactive programming
;; in Clojure.
(straight-use-package 'cider)

;; rainbow-delimiters
(straight-use-package 'rainbow-delimiters)

;; ParEdit
(straight-use-package 'paredit)

;; gauche-mode
(straight-use-package 'gauche-mode)

;; highlight-indent-guides
(straight-use-package 'highlight-indent-guides)

;; dired-detalias
;; http://emacs.rubikitch.com/sd1411-dired-wdired/
(straight-use-package 'dired-details)

;; dired-toggle
;; http://emacs.rubikitch.com/sd1411-dired-wdired/
(straight-use-package 'dired-toggle)


;;--------------------------------------------------------
;; End To install a package Write Here!
;;--------------------------------------------------------

;;; End straight.el---------------------------------------

;;; End Package Manager Settings--------------------------

;;;;------------------------------------------------------
;;;; Pagckage Settings
;;;;------------------------------------------------------

;;;「Emacs実践入門」大竹智也[著]
;; 行の折り返し表示を切り替える
;; refer: 「Emacs実践入門」大竹智也[著] p.81
(require 'bind-key)
(bind-key "C-c l" 'toggle-truncate-lines)

;; Evil Settig
(evil-mode 1)

;; dired-recent
(dired-recent-mode 1)

;;; ivy Settings------------------------------------------
;; refer: https://takaxp.github.io/articles/qiita-helm2ivy.html
(when (require 'ivy nil t)

  ;; M-o を ivy-hydra-read-action に割り当てる．
  (when (require 'ivy-hydra nil t)
    (setq ivy-read-action-function #'ivy-hydra-read-action))

  ;; `ivy-switch-buffer' (C-x b) のリストに recent files と bookmark を含める．
  (setq ivy-use-virtual-buffers t)

  ;; ミニバッファでコマンド発行を認める
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1)) ;; 何回層入ったかプロンプトに表示．

  ;; ESC連打でミニバッファを閉じる
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)

  ;; プロンプトの表示が長い時に折り返す（選択候補も折り返される）
  (setq ivy-truncate-lines nil)

  ;; リスト先頭で `C-p' するとき，リストの最後に移動する
  (setq ivy-wrap t)

  ;; アクティベート
  (ivy-mode 1))

;; 検索語のハイライト
;; rers: https://takaxp.github.io/articles/qiita-helm2ivy.html

;;; End ivy Settings--------------------------------------

;; counsel Settings---------------------------------------
(when (require 'counsel nil t)
  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-M-z") 'counsel-fzf)
  (global-set-key (kbd "C-M-r") 'counsel-recentf)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
;;  (global-set-key (kbd "C-M-f") 'counsel-ag)  ; C-M-f は元々はS式を単位としてカーソル移動するバインド
  ;; アクティベート
  (counsel-mode 1))

;; swiper Settings
(when (require 'swiper nil t)

  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))

;; counsel-recentf 再定義
;; ファイルの表示を`~`から初める設定
;; refer: https://takaxp.github.io/articles/qiita-helm2ivy.html#org87d665a3
(defun ad:counsel-recentf ()
  "Find a file on `recentf-list'."
  (interactive)
  (require 'recentf)
  (recentf-mode)
  (ivy-read "Recentf: "
            (progn
              (mapcar #'substring-no-properties recentf-list) ;; no need?
              (mapcar #'abbreviate-file-name recentf-list)) ;; ~/
            :action (lambda (f)
                      (with-ivy-window
                        (find-file f)))
            :require-match t
            :caller 'counsel-recentf))
(advice-add 'counsel-recentf :override #'ad:counsel-recentf)

;; End counsel Settings-----------------------------------

;;; Evil Leader-----------------------------------------
;; Evil Leader provides the <leader> feature from Vim that
;; provides an easy way to bind keys under a variable prefix key.
;; For an experienced Emacs User it is nothing more than
;; a convoluted key map, but for a Evil user coming from
;; Vim it means an easier start.
;; refer: htps://github.com/cofi/evil-leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-leader/set-key
  "s" 'switch-to-buffer             ; Switch to buffer
  "t" 'find-file                    ; find file Table
  "r" 'insert-file                  ; insert-file
  "w" 'save-buffer                  ; Wrote <file>
  "k" 'kill-buffer                  ; Kill buffer
  "q" 'save-buffers-kill-emacs      ; Quit save buffers kill emacs
  "e" 'eval-last-sexp               ; Eval last sexp
  "c" 'slime-compile-defun          ; slime Compile defun
  "l" 'slime-compile-and-load-file  ; slime compile and Load file
  "n" 'other-window                 ; move to next window
  "p" 'other-window-backword        ; move to previous window
  "2" 'split-window-vertically      ; split window vertically
  "3" 'split-window-horizontally    ; vertically split
  "0" 'delete-window                ; delete window
  "1" 'delete-other-windows         ; delete other window "only one"
  "d" 'scroll-one-line-ahead        ; one line scroll up
  "u" 'scroll-one-line-behind       ; one line scroll down
  ">" 'scroll-right                 ; window scroll to right
  "<" 'scroll-left                  ; window scroll to left
  "," 'point-to-top                 ; cursor goto top left corner
  "." 'point-to-bottom              ; cursor goto bottom left corner
  "!" 'line-to-top                  ; Move current line to top of window
  "^" 'enlarge-window               ; window hight up one line
  "-" 'shrink-window                ; window hight down one line
  "}" 'enlarge-window-horizontally  ; window wide one enlargefd
  "{" 'shrink-window-horizontally   ; window wide one shrink
  "+" 'balance-windows              ; windows same size
  "f" 'flycheck-list-errors         ; pop-up errors list
  "a" 'beginning-of-line            ; go to beginning of line
  ";" 'end-of-line                  ; got to end of line
  "(" 'backward-kill-sexp           ; カーソルの前にあるS式を削除する
  ")" 'kill-sexp                    ; カーソルの後にあるS式を削除する
  )

;;; End Evil Leader-------------------------------------

;; smartrep
;; プレフィクスキーを省略させる
;; ウィンドウ操作をひとまとめにする
;; 始めに"C-x"キーを押してから、"一文字"を入力する
(require 'smartrep)
(smartrep-define-key global-map "C-x"
  '(("x" . (other-window))
    ("0" . (delete-window))
    ("1" . (delete-other-windows))
    ("2" . (split-window-below))
    ("3" . (split-window-right))
    ("{" . (shrink-window-horizontally))
    ("}" . (enlarge-window-horizontally))
    ("+" . (balance-windows))
    ("^" . (enlarge-window))
    ("-" . (shrink-window))))


