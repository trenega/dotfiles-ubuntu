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

;;; 起動時に fullscreen にする
;; (if (or (eq window-system 'ns) (eq window-system 'darwin))
;;     (add-hook 'window-setup-hook
;;               (lambda ()
;;                 (set-frame-parameter nil 'fullscreen 'fullboth))))

;; Unixコマンド エミュレーションを無効にする
;; http://emacs.rubikitch.com/sd1412-eshell/
(eval-after-load "esh-module"
    '(setq eshell-modules-list (delq 'eshell-ls (delq 'eshell-unix eshell-modules-list))))

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

;;;「Emacs実践入門」大竹智也[著]
;; 行の折り返し表示を切り替える
;; refer: 「Emacs実践入門」大竹智也[著] p.81
(require 'bind-key)
(bind-key "C-c l" 'toggle-truncate-lines)

;;; clipboard Setting
;; Emacsから他のエディターにAlt+vでペーストはできるが、その逆にEmacsへは
;; ペーストできない。
;; refer: saitodev.co/article/Emacsでクリップボードを使ってコピペしたい/
(cond (window-system
  (setq x-select-enable-clipboard t)))

;;;; End Initialization------------------------------------


