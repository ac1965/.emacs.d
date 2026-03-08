# Emacs Config Bug Fixes

10箇所のバグ修正済みファイルセットです。

## 適用方法 (推奨)

```bash
# アーカイブを展開してから
cd ~/.emacs.d
bash apply-fixes.sh
```

`apply-fixes.sh` は：
1. `README.org` のバックアップを作成
2. 各ソースブロックを修正済み内容で置換
3. `make tangle` を実行して全ファイルを再生成

## 手動適用

各 `.el` ファイルを `~/.emacs.d/` 以下の対応するパスにコピーしてください。

## 修正一覧

| # | ファイル | 深刻度 | 修正内容 |
|---|---------|--------|---------|
| Fix 1 | `lisp/core/core-session-private.el` | 🔴 Critical | `(require 'core-session nil t)` 追加。単体ロード・バイトコンパイル時の `void-variable` リスク解消 |
| Fix 2 | `lisp/utils/utils-org-agenda.el` | 🔴 Critical | `(require 'cl-lib)` 追加。`cl-loop` がインタープリタ実行時に未定義になる問題を解消 |
| Fix 3 | `lisp/orgx/orgx-fold.el` | 🔴 Critical | `org-fold-folded-p` を `fboundp` でガード。Emacs 29 未満の互換性確保 |
| Fix 4 | `lisp/core/core-persistence.el` | 🟠 Medium | `my:d:var` を優先参照。`no-littering` パッケージのロード順に依存しない安定した参照に変更 |
| Fix 5 | `lisp/completion/completion-lsp.el` | 🟠 Medium | 内部関数 `eglot--project` を `fboundp` でガード。Emacs 31+ での変更に対して安全に |
| Fix 7 | `lisp/orgx/orgx-extensions.el` | 🟠 Medium | `orgx--org-roam-autosync-enable` に `condition-case` 追加。DB スキャン時のエラーがクラッシュしなくなる |
| Fix 8 | `lisp/ui/ui-font.el` | 🟡 Minor | `global-ligature-mode` を `prog-mode-hook` から外し `:config` に移動。グローバルモードをフックから繰り返し呼ぶ冗長さを解消 |
| Fix 9 | `lisp/core/core-buffers.el` | 🟡 Minor | `run-at-time 0` → `run-with-idle-timer 0`。バッファ kill 完了後に scratch を再生成 |
| Fix 10 | `lisp/dev/dev-ai.el` | 🟡 Minor | `aidermacs-extra-args` の `defvar` を `:config` 内からトップレベルへ移動。パッケージ側の `defvar` による値リセットを防止 |
