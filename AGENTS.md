# AGENTS.md
# ac1965/.emacs.d — Agent Instructions

> 本ファイルの記述言語: 見出しは英語、本文は日本語。

## 1. Project Overview

このリポジトリは YAMASHITA Takao (ac1965) の個人用 Emacs 設定である。
Org 文書とソースコードを一体として管理する literate program であり、
`README.org` を設計・仕様・実装の正本（Source of Truth）とする。
ただし正本は 1 枚ではない。2026-10-25 に、次のとおり複数の Org 文書へ分けた。
どの文書も、説明とコードを同じ節に同居させる（この性質は、分割しても保つ）。

| 生成物 | 正本 |
|---|---|
| `early-init.el`、`init.el`、`lisp/modules.el` | `org/boot.org` |
| `lisp/<layer>/*.el` | `org/<layer>.org`（`core` / `ui` / `auth` / `completion` / `orgx` / `dev` / `vcs` / `utils`） |
| `personal/*.el`、`user-lisp/*.el` | `org/personal.org` |
| `scripts/*`（補助スクリプト） | `org/tools.org` |
| `Makefile`、`LICENSE`、`ChangeLog`、`svg/*.dot` / `svg/*.mmd` | `README.org` |

以降の本書で `README.org` の source block と書いた箇所は、特に断らない限り、上の表のどの Org 文書にも当てはまる。
`org/` の各ファイルは、`:tangle ../lisp/...` のように、`.emacs.d/` からの相対パスに `../` を付ける。
`org-babel-tangle` により、厳格な 10 層アーキテクチャにまたがる
102 個の `.el` ターゲット（`lisp/` 配下 86 個〔`lisp/modules.el` を含む〕、
`personal/` 配下 7 個、`user-lisp/` 配下 2 個、ルートの `early-init.el` と `init.el`、
`scripts/demo-layout.el`、`scripts/check-bytecomp.el`、`scripts/cheatsheet.el`、`scripts/orphans.el`、`scripts/smoketest.el`）が生成される。
（`scripts/` の 5 個の `.el` は `org/tools.org` から、`early-init.el`・`init.el` は `org/boot.org` から、`lisp/` と `personal/` の `.el` は上の表の各 Org 文書から tangle される。）

```
Org 文書 (README.org / org/*.org が正本)
    ↓
Org-Babel tangle
    ↓
Source files (派生物)
    ↓
Lint / Test
```

`README.org` の散文はすべて日本語で書く（`#+LANGUAGE: ja`）。
英語にするのは次のものに限る: 見出し、`:CUSTOM_ID:` の値、
`;;; <file>.el ends here` フッター、`lexical-binding` クッキー、
Copyright/Author/License ヘッダー、docstring、コード上のシンボル、
パッケージ名。散文を英訳しない。新しい散文を英語で書かない。

### Primary source

`README.org` はドキュメントではなく、実行される設定そのものである。
次を含む。

- `.el` ファイルへ tangle される Emacs Lisp の source block
- `leaf` によるパッケージ宣言（本プロジェクトは `use-package` ではなく
  `leaf` を使う）
- 変数、カスタマイズ、キーバインド、フック、関数、マクロ
- Changelog セクション
- Appendix の図ソース（Graphviz `.dot` / Mermaid `.mmd`）

ただし Installation 節のビルド情報の表は、`# build-info:begin` と `# build-info:end`
の間を `scripts/update_build_info.py` が書き換える（Emacs の再ビルド後に実行する）。
この 2 行の間は手で編集しない。

Emacs 設定に手を入れる前に、`README.org` 内の該当する
`#+begin_src emacs-lisp ... #+end_src` ブロックを特定して読むこと。
ブロックを「動かない説明文」として扱ってはならない。

### Derived files

次は tangle の出力（派生物）である。

- `early-init.el`, `init.el`, `Makefile`, `LICENSE`, `lisp/modules.el`
- `lisp/{core,ui,auth,completion,orgx,vcs,dev,utils}/` 配下のすべて
- `personal/` と `user-lisp/` 配下のすべて（`user-lisp/` には、Emacs 自身が書く `.user-lisp-autoloads.el` と `.elc` も入る。`.gitignore` 対象で、孤立の検査からも除いてある）
- `ChangeLog`（GNU 標準形式。README.org の Changelog 節にある
  `#+NAME: changelog` の `text` ブロックから tangle。`.gitignore` 対象）
- `scripts/*.py`（`check_emphasis.py`, `check_fboundp_guards.py`, `check_modules.py`,
  `check_docs.py`, `bench_startup.py`, `smoketest_daemon.py`, `test_fresh.py`, `pre-commit`, `reload.py`, `claude_org_roam_export.py`, `compose.py`, `stats.py`,
  `update_build_info.py`）、
  `scripts/shot.sh`、`scripts/demo-layout.el`、`scripts/check-bytecomp.el`、`scripts/cheatsheet.el`、`scripts/orphans.el`、`scripts/smoketest.el`:
  **`org/tools.org`**（`README.org` ではない）の Python / zsh / emacs-lisp ブロックから tangle
  （`compose.py`、`shot.sh`、`demo-layout.el` は `demo.png` の撮影用で `Appendix: demo.png` 節に、
  `check-bytecomp.el` は `make check-bytecomp` 用で `Appendix: check-bytecomp.el`
  節に、`cheatsheet.el` は `svg/emacs_keybindings_cheatsheet.svg` を実際のキーマップから作る用
  （`make cheatsheet`）で `Appendix: cheatsheet.el` 節に、`update_build_info.py` は `README.org` の
  Installation 節のビルド情報の表を更新する用で `Appendix: update_build_info.py` 節にある。
  `check_modules.py` は `make check-modules`、`stats.py` は `make stats`、`orphans.el` は `make check-orphans` / `make clean-orphans` 用、
  `smoketest.el` は `make smoketest` 用で `Appendix: smoketest.el` 節にある）。
  `org/tools.org` 自体は手で編集する正本で、`make tangle` が他の Org 文書とあわせて tangle する
- `svg/*.dot` / `svg/*.mmd`: tangle される図ソース（git 管理外の中間
  ファイル。`make dot-tangle` / `make mmd-tangle`）
- `svg/*.svg`: 上記から描画してコミットするもの
  （`make dot-svg` / `make mmd-svg`）。例外として、`svg/emacs_keybindings_cheatsheet.svg` は
  図ソースではなく `scripts/cheatsheet.el` が実際のキーマップから作る（`make cheatsheet`）

これらを手で編集してはならない（`Makefile` 自身も同様）。
`README.org` の対応する source block を編集し、`make reload` で再生成する。
生成済みファイルだけを直接変更して問題を解決してはならない。

`personal/` は `.emacs.d/` 直下にあり、`lisp/` の下ではない。
他のモジュールディレクトリ（`core/`, `ui/`, `auth/`, `completion/`,
`orgx/`, `vcs/`, `dev/`, `utils/`）は `lisp/` の下にある
（`lisp/core/`, `lisp/ui/` ...）。トップレベルにあると仮定しないこと。

---

## 2. Source of Truth

### Edit priority

コードを変更する場合、まず対象のソースが `README.org` のどの
`#+begin_src` ブロックから生成されているかを確認する。
優先順位は次のとおり。

1. `README.org` 内の source block
2. `README.org` 内の設計・仕様の記述（Commentary を含む）
3. テスト・検証（`make lint` など）
4. tangle で生成されたソースファイル

例外として、次の場合に限り、生成済みソースを直接 *調査* してよい
（編集は不可）。

- 現在の生成結果を確認する場合
- コンパイルエラーを調査する場合
- tangle 結果と実ファイルの差分を確認する場合
- `README.org` と生成物の不整合を検出する場合

### Header args

`README.org` で現在有効な header args:

```
#+PROPERTY: header-args:emacs-lisp :lexical t :noweb no-export :mkdirp yes :comments no
```

`:comments no`（`:comments link` ではない）に注意。`:comments link` は
`lexical-binding` クッキーを 2 行目に押し下げて壊したため、意図的に
変更した。**emacs-lisp ブロックでは** `:comments link` を再導入しないこと。

この禁止は emacs-lisp ブロックにのみ適用する。`org/tools.org` の Appendix の Python ブロック
（`scripts/check_emphasis.py`, `scripts/check_fboundp_guards.py`,
`scripts/reload.py`, `scripts/claude_org_roam_export.py`）は、各節の
`:header-args:python:` で現在も `:comments link` を使っている。これは
意図された設定であり、emacs-lisp の禁止を理由に変更しないこと。
ただし `org/tools.org` の `Appendix: demo.png` 節の `compose.py` と `shot.sh`、`Appendix: update_build_info.py`
節の `update_build_info.py`、`check_modules.py`、`stats.py` は `:comments no` である（1 行目のシェバンを保つため）。

### Directory layout

```
.emacs.d/
├── README.org      (設計・索引・Makefile・図のソース・Changelog)
├── org/            (層ごとの正本: boot, core, ui, auth, completion, orgx, dev, vcs, utils, personal。補助スクリプトの tools)
├── Makefile        (README.org から tangle — 手編集禁止)
├── early-init.el
├── init.el
├── lisp/
│   ├── modules.el
│   ├── core/       (tangle ターゲット 12)
│   ├── ui/         (16)
│   ├── auth/       (3)
│   ├── completion/ (9)
│   ├── orgx/       (14; 標準 10 + 任意 4)
│   ├── vcs/        (4)
│   ├── dev/        (18)
│   └── utils/      (9)
├── personal/       (7) — ユーザー/デバイス別オーバーレイ。lisp/modules.el より前にロード
├── user-lisp/      (2) — 個人用ライブラリ（personal-llm / personal-messages）。Emacs 31 の user-lisp。起動時に Emacs がバイトコンパイルと autoload の収集を行う。org/personal.org から tangle
├── scripts/        (19) — org/tools.org から tangle されたヘルパー (lint, reload, export, demo 撮影, bytecomp, ビルド情報, check_modules, stats, orphans, smoketest)
├── svg/            図ソース (.dot/.mmd, tangle) と描画済み .svg
├── puppeteer-config.json   mmdc (Mermaid CLI) 設定。手で管理
├── demo.png        README.org から参照するスクリーンショット（手で管理。scripts/shot.sh と scripts/compose.py で撮り直す）
├── .var/           実行時の状態 — 削除禁止
└── .etc/           外部リソース
```

`.var/`, `.etc/`（ドット始まり）は `.emacs.d/` 直下にあり、
`lisp/` や `personal/` と並ぶ。キャッシュ（`eln-cache/`, `straight/` を含む。
削除可、自動再生成）は `.emacs.d/.cache/` ではなく、`early-init.el` の
`my:d:cache` が決める。値は環境変数で変わり、`$XDG_CACHE_HOME` があれば
`$XDG_CACHE_HOME/emacs/`、なければ `$HOME/.cache/emacs/` である（固定の
パスではない。パスを仮定せず `my:d:cache` を参照すること）。
`.emacs.d/eln-cache/`（`my:d:cache` 側ではなく `.emacs.d` 直下）にも、
`subr--trampoline-*.eln` だけが作られることがある。これは Emacs の既定の保存先
（`native-comp-eln-load-path` の 2 番目に残している）に、この設定が効く前、
または設定を読み込まない Emacs が書いたもので、削除してよい（`advice` を付けた
時点で自動的に作り直される）。設定のモジュールの eln は `my:d:cache` 側にある。
詳細は `org/boot.org` の `early-init.el` の「7) ネイティブコンパイル」節を参照。
ディレクトリ作成の標準ヘルパーは
`early-init.el` の `my/ensure-directory-exists` である。

`design_spec.org` は廃止され、完全に削除された。13 個の図 `.dot` ソース（番号 03 は欠番）は
README.org の Appendix（`Appendix: 01_boot_flow.dot` から
`14_llm_overview.dot` まで）に、それぞれ対象モジュールの
隣へ埋め込まれている。Makefile の `DESIGN_SPEC` 変数と、それが駆動して
いた 2 ファイル前提の `dot-tangle`/`mmd-tangle` ロジックも削除済みである。
`DESIGN_SPEC` 型のガードを再導入しないこと。`design_spec.org` が
存在するものと仮定しないこと。

ただし、`design_spec.org` への言及は次の場所に履歴的な記述として残って
いる（確認済み）。これらはコメントであり、`DESIGN_SPEC` 変数やそれを使う
ロジックは存在しない。コメントの言及を根拠に、`design_spec.org` や
`DESIGN_SPEC` が生きていると判断しないこと。

- `README.org` の Elisp コメント（SVG 取り扱いに関する Inkscape /
  `\includesvg` の説明）
- tangle 後の `Makefile` の `dot-tangle` 付近のコメント

---

## 3. Architecture

上から下へ流れる厳格な 10 層の依存構造:

```
early-init → core → ui → auth → completion → orgx → vcs → dev → utils → personal
```

不変条件:

- 上位層は下位層に依存してよい。逆は禁止
- 依存関係の自動探索は行わない。副作用はすべて明示的に書く
- モジュールのロードは決定的で、`lisp/modules.el` の `my:modules` が駆動する
- 任意の拡張モジュールは `my:modules-extra` 経由でロードする。現在は
  `(ui-visual-aids orgx-typography orgx-brain orgx-citar ui-macos)` で、
  `personal/user.el` で設定している。macOS では `personal/device-darwin.el` が
  `dev-apple-music` を加える
- `orgx-roam-ui` は、標準では extras リストから意図的に除外している。
  含めると起動時に `org-roam`/`org` が強制ロードされ、Org の遅延ロードの
  意味がなくなる
- 一部のモジュール（`dev-lsp-eglot`, `dev-lsp-mode`, `ui-doom-modeline`,
  `ui-nano-modeline`, `ui-doom-nano-modeline`, `ui-nano-palette`）は autoload 専用で、
  オンデマンドにロードされ、どちらのリストにも現れない。UI バンドルは `ui-bundles`、
  LSP バックエンドは `dev-lsp-bridge` が `core-switches-register` で登録する（`core-switches`
  は core 層にあり、これらのモジュール名を知らない）。`ui-nano-palette` は `ui-theme` 経由
- LSP バックエンド（`eglot` / `lsp-mode`。切り替えを吸収するファサードが `dev-lsp-bridge`）は
  `core-switches` で選択する。コードはバックエンド非依存を保つこと
- キー操作のモジュールは、2026-10-25 に `ui-leader` / `ui-keymap` / `ui-dirvish-keys` / `ui-global-keys` / `ui-hydra` / `ui-which-key` / `ui-keyboard` の 7 つを `ui-keys` に集約した（`org/ui.org` の `* ui/ui-keys.el`）。公開 API（`ui-keymap-bind`、`ui-leader-key`、`ui-leader-map`、`ui-leader-*-map`）の名前は、`personal/`・`dev/`・`utils/`・`orgx/` が参照するため変えていないので、接頭辞（`ui-leader-` / `ui-keymap-` / `ui-hydra-` / `ui-which-key-` / `ui-keyboard-` など）はモジュール名と一致しない。ファイル内の並びは、リーダー基盤 → Hydra → which-key → キーボード変換 → 割り当て → dirvish → グローバルの順で、ロード時の実行（`ui-leader--bind-root`、`ui-keyboard--apply`）と `after-init-hook` への登録順（`ui-which-key--startup-init` → `ui-keymap--startup-init` → `ui-dirvish-keys--startup-init` → `ui-global-keys--startup-init`）は暗黙の契約なので入れ替えない。feature 名は `ui-keys` だけで、旧 feature 名は存在しない（`with-eval-after-load` では `'ui-keys` を使う）
- LLM の API キーの取得は personal 層（`personal/personal-credentials.el`）だけが知る。下位層は
  関数を受け取る変数だけを持つ: `utils-claude-api-key-function`（utils）と
  `dev-ai-anthropic-key-function`（dev。agent-shell を API キーで認証するときだけ）に、personal 層が
  `personal-credentials-anthropic-key` を渡す。下位層に `auth-source-search` を書かないこと
  （上位層への依存を避けるため。`make smoketest` が `utils-claude` を検査する）。検索は 1 回に
  まとめ、使う瞬間まで遅らせる（GPG / Touch ID の連続認証が失敗するため）。Claude API と
  ローカルルーターの呼び出しは `user-lisp/personal-llm.el` 1 つにまとめてあり、すべて
  `;;;###autoload` で、Org をロードしない（Org Babel の `claude` ブロックの
  `org-babel-execute:claude` も autoload）。`org-babel-load-languages` に `claude` を足してはならない
  （`ob-claude` は存在しない）
- AI の役割分担（`dev/dev-ai.el` の Commentary にも同じ表がある）: `agent-shell`（Claude Code。
  `claude-agent-acp`。`M-SPC a c`）が主役で、複数ファイルの作業・コマンドやテストの実行を担い、
  認証は `claude` のログイン（`dev-ai-agent-authentication` の既定は `login`。`api-key` にすると
  `dev-ai-anthropic-key-function` が返すキーを使う。personal 層が `personal-credentials-anthropic-key` を
  渡す）。単発の処理は `user-lisp/personal-llm.el`。`aidermacs` は、OpenRouter を使わなくなったため
  2026-10-31 に削除した（再び入れるときは、OpenRouter のキーの受け渡しも作り直すこと）
- メール系モジュール（`auth-mail`, `dev-mail`, `utils-notmuch`）は
  このリポジトリには存在しない。存在すると仮定しないこと。
  ただし mu4e だけは `personal/ac1965.el` の leaf で設定している（モジュールではない）

---

## 4. Before Making Changes

変更を始める前に、必ず次を行う。

1. `git status` を実行する。
2. `README.org` 内の該当セクションと `#+begin_src` ブロックを特定する。
3. 周辺の Org セクション（モジュールの Commentary を含む）を読む。
   コードだけを読んで設計意図を推測してはならない。
4. ブロックの `:tangle` ターゲットと所属する層を確認する。
5. `lisp/modules.el` でロード順と依存する層を確認する。
6. 追加の前に `README.org` を検索し、既存の設定を確認する
   （モジュール間での重複宣言を避ける）。
7. Changelog の文章を一次情報として信用しない。主張は実際の source block と
   照合して検証する。たかおの明示的な方針は、事実を述べる前にコードで
   裏取りすること。
8. このリポジトリは 2026-09-10 に作り直されている。`git log` の最古の
   コミットは同日の初期コミット（`1423c31`）で、履歴は浅い取得
   （shallow）ではなく全件ある（確認済み）。それ以前の履歴は `git log`
   に存在しない。2026-10-10 に設定を 1.0 とし、Changelog は破棄して GNU
   形式に改めた（以前の Changelog の内容は `git log -p -- README.org` で
   たどれる）。ローカル・リモートともタグは存在しない（確認済み）。
9. tangle 関係を把握する: 対象のソースファイル → 生成元の `README.org`
   → 該当 source block → そのブロックが依存する named block / noweb
   参照、の順にたどる。
10. 周辺ファイルも確認する: `Makefile` 相当の `README.org` セクション、
    CI 設定、`AGENTS.md` 自身。

---

## 5. Modification Rules

### Minimal changes

要求を正しく満たす最小の変更を選ぶ。次をしてはならない。

- 無関係な設定の書き換え
- 頼まれていないセクションの再編成
- 機能上の理由がない整形変更
- 正当な理由なく、動作中のコードを「より良い」パターンへ置換すること
- 別案のほうが良さそうだというだけで、設定を削除すること

### Refactoring

リファクタリングはコードを短くすることが目的ではない。次を維持する。

- 外部仕様（公開コマンド、キーバインド、カスタマイズ変数）
- API 互換性
- データ形式
- エラー処理
- セキュリティ上の制約
- テスト・検証可能性
- `README.org` と生成コードの対応関係

設計変更を伴う場合は、先に `README.org` の設計記述（Commentary）を更新する。

### Autonomy limits

次の変更は、ユーザーの **明示的な要求** がない限り行わない。

- `README.org` の大規模な再構成
- source block の分割・統合
- noweb 依存関係の変更
- tangle 先の変更
- API 仕様・外部インターフェースの変更
- テスト・検証の削除
- セキュリティ制約の緩和

§9 の「短い承認は自律実行の合図」は、この項目には適用しない。
「続ける」などの短い返答は、これらの変更の明示的な要求とみなさない。

### Org-Babel source blocks

- 新規に追加する source block には、可能な限り `#+NAME:` で明示的な名前を
  付ける。既存ブロックへの一括付与は行わない（上記の大規模再構成に当たる）。
- noweb の既定は `README.org` の `:noweb no-export` を維持する。
  参照が必要な箇所に限り、ブロック単位で指定する。
- ブロック間の依存は、可能な限り named block / noweb 参照で明示する。
- **字下げ:** README のコードブロックは 2 文字字下げで書く。tangle のとき、Org は
  ブロックごとに「空でない行の最小の字下げ」を取り除く（`org-src-preserve-indentation`
  が `nil` のとき。既定）。そのため、書き出された `.el` は README より 2 文字浅くなる
  ことが多い。ただし次の点に注意すること（2026-10-12 に最小の例で確認した）。
  - 取り除く量は **ブロックごとに独立**している。同じファイルに複数のブロックがあれば、
    それぞれの最小の字下げだけ取り除かれる。
  - ブロックの途中に **桁 0 の行**（docstring の継続行など）が 1 行でもあると、そのブロック
    では何も取り除かれず、書き出された `.el` の字下げが、ブロック全体で 2 文字深くなる
    （コードの意味は変わらない）。README と `.el` の字下げの差が、ブロックによって
    0 や 2 と違うのは、このため。2026-10-20 に全 emacs-lisp ブロックを `indent-region`
    相当に整形し直し、桁 0 の行を持つブロックは 0 個になった。新しく書くブロックでも、
    この状態（README は全行 2 文字字下げ、`.el` は最上位の式が桁 0）を保つこと。
  - したがって、生成された `.el` の字下げを基準に README を編集しない。README 側の
    行の字下げを実測し、docstring の継続行は、**そのブロックの共通の字下げ**の桁に置く
    （桁 0 の行を足して、ブロックの字下げを意図せず変えない）。
  - `Makefile` のブロックだけは、タブを保つため `org-src-preserve-indentation` を `t` で
    tangle する（`README.org` の `* Makefile` 節を参照）。

### Coding rules (enforced; verify before submitting a change)

1. すべての tangle 済みファイルは `lexical-binding: t` クッキーで始める。
2. `provide` のシンボルは、拡張子 `.el` を除いたファイル名と一致させる。
3. 組み込みパッケージは `leaf` 宣言で `:straight nil` を使う。
4. `leaf` のキーワード順: `:straight` → `:ensure` → `:after` →
   `:require` → `:pre-setq` → `:custom` → `:bind` → `:hook` →
   `:init` → `:config`。
5. `leaf :custom` は式を評価できない。実行時評価が必要なものは
   `:config` で `setopt` を使う。
6. 変数代入: `defcustom` には `setopt`、`defvar`・内部の可変状態には
   `setq`。`setopt` を使えない既知の `setq` 例外: `org-agenda-files`,
   `org-capture-templates`, `org-todo-keywords`, `org-refile-targets`,
   `my:modules-extra`（`defcustom` 評価の前に決定的でなければならず、
   `personal/user.el` から設定する。`modules.el` の Design Notes にある
   「ロード順に関する例外」を参照）。`org-roam-db-connector` は
   `defcustom` であり `setopt` を使う。この例外リストに戻さないこと。
   このほかに `setq` のままにしているもの（2026-10-12 の監査で確認）:
   `package-enable-at-startup`, `package-quickstart`（`setopt` が `package.el` を
   起動時に読み込んでしまう）、`custom-file`（標準的な書き方）、
   `aidermacs-extra-args`（モジュール側の `defvar` へリストを追記する）。
   **`setopt` は、自動読み込みの対象になっている `defcustom`（Org など）では
   ライブラリを読み込むことがある。** 起動時に Org を読み込まないため、Org 関連の
   `setopt` は `after-init-hook` ではなく `with-eval-after-load` の中で実行すること
   （`orgx-visual.el` で、起動後のフィーチャーが 485 から 566 に増えて発覚した）。
7. 命名: パス変数は `my:`、公開コマンドは `my/`、公開 API は `module-`、
   非公開シンボルは `module--`。
8. `defun` はモジュールのトップレベルにのみ置く。`leaf` ブロックや
   `with-eval-after-load` の内側には置かない。
9. `defcustom` 変数を持つモジュールは、自身の `defgroup` を宣言する。
10. 公開 `defun` にはすべて docstring を付ける。

### Daemon-mode pattern

- ロード時に `(display-graphic-p)` で分岐してはならない。
  `emacsclient -c` のデーモンモードでパッケージのロードが恒久的に壊れる。
  代わりに `(daemonp)` で分岐し、`after-make-frame-functions` を使う。
- 値を *設定* するコードは、`after-make-frame-functions` 内に直接書いて
  安全。
- フレームの状態（色・フェイス）を *参照* するコードは、さらに
  `(run-with-timer 0 nil ...)` による 1 tick の遅延が必要。

### Org lazy-loading

- `orgx` 層は、起動時に Org を強制ロードしないよう、`:require t` ではなく
  `declare-function` と関数内部の `require` を使う。

---

## 6. Changelog Rules

Changelog は **GNU 標準の ChangeLog 形式**で書く（2026-10-10 に 1.0 とし、
以前の Fix-ID 方式・Org 見出し方式は廃止した。古いテンプレートに従う
よう求められても、ユーザーが明示的に復活を求めない限り使わないこと）。

`README.org` の `* Changelog` 節の `#+NAME: changelog` ブロック
（`text`、`:tangle ChangeLog`）に、新しい項目を **先頭に** 追加する。

```
YYYY-MM-DD  氏名  <メールアドレス>

<TAB>* ファイル (関数名): 変更内容の日本語による説明。
<TAB>続きの行も同じ TAB で字下げする。
```

- 見出しは `日付  氏名  <メールアドレス>`（日付と氏名の間は空白 2 つ）。
  同じ日・同じ作者の項目は 1 つの見出しにまとめ、見出しと項目の間に
  空行を入れる。
- 字下げは **TAB 文字**（空白ではない）。項目は `* ファイル (関数):` で始める。
- 変更の理由は Changelog ではなく、モジュール自身の Commentary セクションに
  書く。履歴は `git log` にもある。
- ブロックは Org の `emacs-lisp` ではないため、`make check-tangle` の
  対象外（`:tangle ChangeLog` を明示してあるので孤立もしない）。

`README.org` の編集を終えたら:

1. 上の形式で Changelog に項目を追記する。
2. 構造の整合性を検証する: `#+begin_src`/`#+end_src` の対応、
   `:CUSTOM_ID:` の一意性、触れたすべての Elisp ブロックの括弧の深さ。
3. 強調・括弧のチェックでは、Changelog の文章ではなく
   `#+begin_src emacs-lisp ... #+end_src` ブロックの内側だけを走査する
   （二重カウントを避けるため）。
4. 編集内容に照らして `AGENTS.md` を確認し、乖離していれば同じ変更の
   中で更新する（§10 参照）。

---

## 7. Org Markup Constraints

- 太字で verbatim/code スパンを囲めない。
  `*なぜ =foo= なのか*` は無効で、外側の太字がスパンを消費してしまう。
  強調を分割すること。
- `org-emphasis-regexp-components`（日本語の句読点を強調の境界文字として
  許可するために必要）は `(with-eval-after-load 'org ...)` の内側で設定
  する。`leaf org :config` ブロック内では設定しない。
- 図ブロック（Graphviz/Mermaid）は、GitHub での可読性のため
  `bgcolor="white"` を使う。

---

## 8. Build and Verification

Makefile を使う。手動で tangle しない。リポジトリ側で定義された tangle
方法を、汎用の `emacs --batch ... org-babel-tangle-file` より常に優先する。

| Target | Purpose |
|---|---|
| `make tangle` | `README.org` と `org/*.org` を `.el` ターゲット・`scripts/` へ tangle |
| `make reload` | `clean` + `tangle` + `check-cookies` + `clean-orphans`。古い `.elc` と、tangle 先から外れた古い生成物（統合・改名・ブランチ切替で残る `.el` など）を避けるため、単独の `tangle` より推奨。途中で `Makefile` が書き換わったら、新しい `Makefile` でもう一度 `reload` を実行する（pull などで `Makefile` が更新されたときだけ。初回は `emacs -Q --batch` の tangle のあと 1 回で足りる） |
| `make lint` | `check-tangle` + `check-emphasis` + `check-cookies` + `check-fboundp-guards` + `check-modules` + `check-orphans` + `check-docs` + `checkdoc` を実行 |
| `make check-tangle` | 見出しレベルの誤りで `:tangle` を継承できない src ブロックを検出 |
| `make check-emphasis` | 太字が verbatim を囲む場合を含む、無効な Org 強調記法を検出（Elisp ではなく Python 製） |
| `make check-cookies` | tangle された全 `.el` が `lexical-binding: t` クッキーで始まることを検証 |
| `make check-fboundp-guards` | `(fboundp 'X)` でガードされた呼び出しを、実際には解決できないシンボルの `#'X` 参照と照合（Python 製。事前の tangle が必要） |
| `make check-modules` | `lisp/modules.el` の一覧と `lisp/` 配下の整合性（実在・`provide`・層の順序・上位層への `require`・どちらにも載らない `.el`）を、Emacs なしで検査（Python 製） |
| `make check-orphans` / `make clean-orphans` | `lisp/`・`personal/`・`scripts/` の `.el`・`.elc`・`.py`・`.sh` のうち、どの Org 文書も tangle しないもの（孤立した生成物）を一覧する（あれば失敗）／削除する。本体は `scripts/orphans.el`。tangle 先を 20 件未満しか取れないときは何も消さない |
| `make verify` | `lint` + `check-bytecomp` + `check-cheatsheet` + `smoketest` をまとめて実行する、コミット前の確認用。後の 3 つは約 40 秒ずつかかるため `lint` には含めず、ここでまとめる |
| `make check-docs` | README.org と AGENTS.md に書いた数と名前を、実コードと照合する（Python 製）。AGENTS.md の `.el` の総数・各ディレクトリの数・構成図、README の図のファイル数と枚数、README の散文の `=…=` で書いた記号・パス・`make` のターゲットが実在するか。意味までは見ない。事前の tangle が必要。`lint` に含まれる。数を変えたら、この検査が教える |
| `make smoketest-daemon` | `emacs --daemon` で起動し、フレームを作る前（全モジュールのロード、リーダーキーの全割り当て、警告なし、フックの登録）と、`emacsclient -c` でグラフィカルなフレームを作った後（フォント、テーマ、posframe、ligature）を確かめる。この設定には `(daemonp)` で分岐する箇所が多く、バッチの `smoketest` は通らない。グラフィカルなフレームが数秒出る。画面が無い環境は `SMOKETEST_DAEMON_ARGS=--no-frame`。`lint`・`verify` には含めない |
| `make test-fresh` | 一時ディレクトリへ clone し、README の手順（`emacs -Q --batch` のブートストラップ → `make reload` → `make lint` → `--init-directory` での起動と `smoketest` → `make install-hooks` と pre-commit を通るコミット）で組み立てて、動くかを確かめる。パッケージは共有の straight キャッシュを使う。既定はコミット済みの状態。`FRESH_ARGS=--working-tree` で未コミットの状態も重ねる。`--isolate-cache` は空のキャッシュで作り直す（数十分、ネットワークが要る）。数分かかる |
| `make install-hooks` / `make uninstall-hooks` | `scripts/pre-commit` を `.git/hooks/pre-commit` へ入れる／外す。フックは、README・AGENTS・`org/`・`svg/`・Makefile・`.gitignore` のいずれかがステージされているコミットの前に、`make reload && make lint` を実行する。`SKIP_HOOKS=1 git commit` で飛ばせる。`make verify` を済ませたあとのコミットでは、二重になるので `SKIP_HOOKS=1` を付けてよい |
| `make bench-startup` | GUI の Emacs を `--runs` 回（既定 5）起動し、使える状態になるまでの時間（`my:startup-phases` の最後）を測る。`BENCH_ARGS="--runs 8 --save"` で基準を `.var/bench-startup.json` に保存し、以後は基準と比べて、10% 以上かつばらつき以上に遅ければ失敗する。Emacs のウィンドウが毎回出る。`lint`・`verify` には含めない |
| `make stats` | 層ごとのファイル数・行数・コメント率を表示。本書の数値を更新するときの根拠にする（Python 製） |
| `make checkdoc` | Elisp の docstring/スタイルチェック |
| `make package-lint` | パッケージメタデータのチェック（任意。`load-path` に `package-lint` が必要） |
| `make cheatsheet` / `make check-cheatsheet` | キーバインド一覧 `svg/emacs_keybindings_cheatsheet.svg` を、init を読み込んだ Emacs の実際のキーマップから書き直す / 書き出さずに、コミット済みの SVG が今のキーマップと同じかを調べる（違えば終了コード 1）。約 40 秒かかるため `lint` には含めない。本体は `scripts/cheatsheet.el` |
| `make check-bytecomp` | `early-init.el` と `init.el` を読み込んだ Emacs で、`lisp/`・`personal/`・`early-init.el`・`init.el` をすべてバイトコンパイルし、警告があれば一覧して終了コード 1 で終わる。読み込めずコンパイルに失敗したファイルと、init の後でロードされていないモジュールも警告に数える（出力は一時ディレクトリで `.elc` は作らない。約 40 秒かかるため `lint` には含めない。本体は `scripts/check-bytecomp.el`） |
| `make smoketest` | `early-init.el` と `init.el` を読み込んだ Emacs で、起動（警告・全モジュールのロード）、vertico / consult / embark / corfu の設定とキー、主要なキーが command に結びつくこと、vcs 層の leader キー・diff-hl・gh 経由の forge トークン・一時 git リポジトリでのコミット補助とワークツリー、Org の日々の運用（agenda ビュー・capture・アーカイブ・clock・org-ql）を一時ファイルで動かした確認、各層（core・ui・auth・dev・utils）の状態の確認（リーダーキーの全割り当てがコマンドに結びつくことを含む）、`dev-run` のテスト実行、一時バッファでの補完の実動作、Eglot と dabbrev の合流（Eglot は代役）、`utils-ime` の修飾キーでの英字化と復帰（偽の `macism`）を確かめる。失敗が 1 件でもあれば終了コード 1。バッチの Emacs が書き直す straight のビルドキャッシュを、開始時に退避して終了時に戻す（戻さないと GUI 起動で `consult-line` などが未定義になる）。約 40 秒かかるため `lint` には含めない。本体は `scripts/smoketest.el` |
| `M-x my/smoketest`（`my/smoketest-lsp` / `-terminal` / `-gui`） | `make` ではなく、起動中の Emacs で実行する確認。実 LSP サーバへの接続、`emacs -nw` を疑似端末で動かした corfu-terminal の描画、GUI の posframe・Corfu・popupinfo の子フレームを確かめ、結果を `*Smoketest*` に出す。本体は `org/utils.org` の `utils-diagnostics`。端末の確認は子にキー入力を送らない（復元されたファイルへ文字が入る事故があった）。実行中は入力しない |
| `make stage` | `TRACKED`（`README.org`, `org`, `AGENTS.md`, `demo.png`, `puppeteer-config.json`, `.gitignore`, `svg`）だけを `git add -A` し、それ以外がステージ済みなら中止する。`git add .` の代わりに使う。追跡するファイルが増えたら `README.org` の `* Makefile` の `TRACKED` に足す。`TRACKED` の外にある追跡済みファイルの**削除**（名前変更の旧パスを含む）は、中止の対象にしない |

`make help` の `make lint` の説明行は、`check-fboundp-guards` を含む形に直した
（2026-10-12）。それでも、実際の `lint` の依存は上の表と
`README.org` の `* Makefile` セクションのレシピが正であり、`help:` の出力を
根拠にしないこと。

変更の標準ワークフロー:

1. `README.org` を編集する。
2. `make reload` を実行する。
3. Emacs を再起動する。デーモンで動かしている場合は
   `emacsclient -c` で再接続する。
4. 完了とみなす前に `make lint` を実行する。

### Verification scope

- tangle だけで作業を完了としてはならない。
- 変更後は `git diff` と `git status` で、生成物の差分が想定どおりか
  確認する。
- `make lint` に加え、リポジトリで定義されている構文チェック、unit/
  integration test、型チェックに相当するものがあれば、可能な範囲で実行
  する。既存のテストがあれば、変更前後で結果を比較する。
- 実行できなかった検証は、§11 の Remaining に記載する。

### Org / generated mismatch

`README.org` と生成ソースに矛盾（Org source ≠ generated source）を検出
したら、作業を止めて原因を調査する。`make reload` は `clean` を含んで
生成物を上書きするため、**調査より前に実行しない**。確認すべき点:

- 手動で変更された生成ソース
- tangle 設定（header args、`:tangle` ターゲット）の変更
- noweb 参照の破損
- source block の重複
- 同一ファイルへの複数 tangle
- 出力先の変更

原因を特定しないまま上書きしてはならない。矛盾した生成ソースを黙って
修正せず、`README.org`・tangle 設定・生成物の関係を調べる。

---

## 9. Communication Style

たかおは簡潔に、多くは日本語だけでやり取りする。短い確認（「続ける」、
一文字の返答など）は、再確認せず自律的に進めてよいという意味である。
修正指示は生のエラーメッセージで届く。確認のための質問を先にせず、
それを修正の仕様として扱うこと。

ただし、§5 の Autonomy limits に挙げた変更には、この規則は適用されない。
それらは明示的な要求がある場合にのみ実行する。

---

## 10. Keeping AGENTS.md in Sync with README.org

**恒久ルール。** `README.org` と `org/*.org`（§1 の表）が正本であり、`AGENTS.md` は
その派生的な要約である。これらの Org 文書が変わると、AGENTS.md は気づかない
うちに乖離する。実際に乖離が起きた（2026-09-13 の同期: 古いモジュール数、
フラットな木から移動した `lisp/` 配置、誰も削除しなかった廃止済み
`DESIGN_SPEC` ガード、`setopt` になっていた `setq` 例外）。黙って
繰り返さないこと。

`README.org` を編集するたびに（大規模な改修に限らず）、その編集が
`AGENTS.md` の事実に関する記述を無効にしていないか確認し、していれば
同じ変更の中で `AGENTS.md` を更新する。具体的に再確認する対象:

- 層ごとのモジュール/ファイル数（§1, §2）と tangle ターゲットの総数。
  モジュールの追加・削除、または `lisp/<layer>/` と `personal/` 間の移動の
  たびに確認する。
- ディレクトリ構成図（§2）と「Derived files」リスト（§1）。トップレベルの
  ファイルやディレクトリの追加・削除・移動（`lisp/` への出入りなど）の
  たびに確認する。
- 層のリストとその不変条件（§3）。`my:modules`、`my:modules-extra`、
  autoload 専用モジュールのリスト、LSP バックエンドの切替など、
  `lisp/modules.el` や `personal/user.el` がロード内容を変えたときに
  確認する。
- 番号付きコーディング規則とその例外（§5）。特に規則 6 の `setopt`/`setq`
  の区分は、設定の進化に伴い個々の変数が行き来しうる。規則 3, 4, 6 は
  `README.org` のコメントから番号で直接引用されている
  （`grep -n "コーディング規則" README.org`）。番号を振り直さず、内容
  のみ修正すること。
- Makefile のターゲット表（§8）。`Makefile` の `lint` 依存リスト、
  `reload` レシピ、利用可能なターゲットが変わったときに確認する。
  `README.org` 自身の `* Makefile` セクション（`:tangle Makefile`）が
  正であり、`help:` ターゲットの echo 文は古くなりうるため根拠にしない。
- 特定のファイル、スクリプト、変数を名指しする記述。信用する前に、
  §4.7 に従い `grep`/`find` でまだ存在するか検証する。

乖離したかどうか迷ったら、どちらかの文書の文章を信じるのではなく、
実際の `README.org` の内容とディスク上の tangle 出力に照らして検証する
（今回の同期で行ったように）。

---

## 11. Work Report

作業終了時に、次の 5 項目を報告する。各項目は 1〜2 行で簡潔にまとめる。

- **Changed**: 変更した `README.org` のセクション・source block と、
  再生成されたファイル
- **Design**: 変更した設計上の理由
- **Generated**: tangle によって生成・更新されたファイル
- **Validation**: 実行した検証と結果
- **Remaining**: 未解決の問題、未実施の検証、注意事項

Validation の例:

```
make reload: PASS
make lint: PASS
```

---

## 12. Core Principle

このリポジトリでは「生成されたソースを正本として扱わない」。
`README.org` と生成ソースに矛盾がある場合は、生成ソースを黙って修正する
のではなく、`README.org`・tangle 設定・生成物の関係を調査する（§8）。

AI エージェントの目的は、コードを変更することだけではなく、

```
仕様 → 設計 → 実装 → 生成 → 検証
```

の一貫性を維持することである。
