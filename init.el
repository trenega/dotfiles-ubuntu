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
