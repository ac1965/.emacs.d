# AGENTS.md
# ac1965/.emacs.d — Agent Instructions

> 本ファイルの記述言語: 見出しは英語、本文は日本語。

## 1. Project Overview

このリポジトリは YAMASHITA Takao (ac1965) の個人用 Emacs 設定である。
Org 文書とソースコードを一体として管理する literate program であり、
`README.org` を設計・仕様・実装の正本（Source of Truth）とする。
`org-babel-tangle` により、厳格な 10 層アーキテクチャにまたがる
97 個の `.el` ターゲット（`lisp/` 配下 88 個〔`lisp/modules.el` を含む〕、
`personal/` 配下 7 個、ルートの `early-init.el` と `init.el`）が生成される。

```
README.org (正本)
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

Emacs 設定に手を入れる前に、`README.org` 内の該当する
`#+begin_src emacs-lisp ... #+end_src` ブロックを特定して読むこと。
ブロックを「動かない説明文」として扱ってはならない。

### Derived files

次は tangle の出力（派生物）である。

- `early-init.el`, `init.el`, `Makefile`, `LICENSE`, `lisp/modules.el`
- `lisp/{core,ui,auth,completion,orgx,vcs,dev,utils}/` 配下のすべて
- `personal/` 配下のすべて
- `ChangeLog`（`make changelog-tangle` により、Changelog サブツリーを
  `ox-ascii` で出力して生成）
- `scripts/*.py`（`check_emphasis.py`, `check_fboundp_guards.py`,
  `reload.py`, `claude_org_roam_export.py`）: README.org の Python
  ブロックから tangle
- `svg/*.dot` / `svg/*.mmd`: tangle される図ソース（git 管理外の中間
  ファイル。`make dot-tangle` / `make mmd-tangle`）
- `svg/*.svg`: 上記から描画してコミットするもの
  （`make dot-svg` / `make mmd-svg`）

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

この禁止は emacs-lisp ブロックにのみ適用する。Appendix の Python ブロック
（`scripts/check_emphasis.py`, `scripts/check_fboundp_guards.py`,
`scripts/reload.py`, `scripts/claude_org_roam_export.py`）は、各節の
`:header-args:python:` で現在も `:comments link` を使っている。これは
意図された設定であり、emacs-lisp の禁止を理由に変更しないこと。

### Directory layout

```
.emacs.d/
├── README.org
├── Makefile        (README.org から tangle — 手編集禁止)
├── early-init.el
├── init.el
├── lisp/
│   ├── modules.el
│   ├── core/       (tangle ターゲット 15)
│   ├── ui/         (16)
│   ├── auth/       (3)
│   ├── completion/ (12)
│   ├── orgx/       (12; 標準 8 + 任意 4)
│   ├── vcs/        (4)
│   ├── dev/        (15)
│   └── utils/      (10)
├── personal/       (7) — ユーザー/デバイス別オーバーレイ。lisp/modules.el より前にロード
├── scripts/        (4) — tangle された Python ヘルパー (lint, reload, export)
├── svg/            図ソース (.dot/.mmd, tangle) と描画済み .svg
├── puppeteer-config.json   mmdc (Mermaid CLI) 設定。手で管理
├── demo.png        README.org から参照するスクリーンショット
├── .var/           実行時の状態 — 削除禁止
└── .etc/           外部リソース
```

`.var/`, `.etc/`（ドット始まり）は `.emacs.d/` 直下にあり、
`lisp/` や `personal/` と並ぶ。キャッシュ（`eln-cache/`, `straight/` を含む。
削除可、自動再生成）は `.emacs.d/.cache/` ではなく、`early-init.el` の
`my:d:cache` が決める。値は環境変数で変わり、`$XDG_CACHE_HOME` があれば
`$XDG_CACHE_HOME/emacs/`、なければ `$HOME/.cache/emacs/` である（固定の
パスではない。パスを仮定せず `my:d:cache` を参照すること）。
ディレクトリ作成の標準ヘルパーは
`early-init.el` の `my/ensure-directory-exists` である。

`design_spec.org` は廃止され、完全に削除された。13 個の図 `.dot` ソースは
README.org の Appendix（`Appendix: 01_boot_flow.dot` から
`13_personal_override_contract.dot` まで）に、それぞれ対象モジュールの
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
- `README.org` の Changelog（廃止を記録したエントリ）

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
  `personal/user.el` で設定している
- `orgx-roam-ui` は、標準では extras リストから意図的に除外している。
  含めると起動時に `org-roam`/`org` が強制ロードされ、Org の遅延ロードの
  意味がなくなる
- 一部のモジュール（`dev-lsp-eglot`, `dev-lsp-mode`, `ui-doom-modeline`,
  `ui-nano-modeline`, `ui-nano-palette`）は autoload 専用で、
  `core-switches` / `ui-theme` 経由でオンデマンドにロードされ、どちらの
  リストにも現れない
- LSP バックエンド（`eglot` / `lsp-mode` / `lsp-bridge`）は
  `core-switches` で選択する。コードはバックエンド非依存を保つこと
- メール系モジュール（`auth-mail`, `dev-mail`, `utils-notmuch`）は
  このリポジトリには存在しない。存在すると仮定しないこと

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
   に存在しないため、それより古い Changelog エントリは `git log` と 1:1 に
   対応しない。対応するのはそれ以降に追加されたエントリのみ。なお、
   以前の履歴が「スカッシュされた」という経緯そのものは、git からは確認
   できない。
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

**Fix-ID 方式は廃止済み（恒久、2026-07 以降）。** 古いテンプレートに従う
よう求められても、ユーザーが明示的に復活を求めない限り、
`*** Fix <ID>: ...` 見出し、重大度の絵文字（🔴🟠🟡）、「違反した規則を
引用する」書き方を使わないこと。

現在の形式:

```org
** <変更内容の日本語による説明>
```

プレーンな見出し、日本語の文章、Fix ID なし、重大度マーカーなし。
変更の理由は Changelog ではなく、モジュール自身の Commentary セクションに
書く。それ以前の履歴は `git log` にある。

`README.org` の編集を終えたら:

1. 現在の形式で Changelog エントリを追記する。
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
| `make tangle` | `README.org` を `.el` ターゲットへ tangle |
| `make reload` | `clean` + `tangle` + `check-cookies`。古い `.elc` を避けるため、単独の `tangle` より推奨 |
| `make lint` | `check-tangle` + `check-emphasis` + `check-cookies` + `check-fboundp-guards` + `checkdoc` を実行 |
| `make check-tangle` | 見出しレベルの誤りで `:tangle` を継承できない src ブロックを検出 |
| `make check-emphasis` | 太字が verbatim を囲む場合を含む、無効な Org 強調記法を検出（Elisp ではなく Python 製） |
| `make check-cookies` | tangle された全 `.el` が `lexical-binding: t` クッキーで始まることを検証 |
| `make check-fboundp-guards` | `(fboundp 'X)` でガードされた呼び出しを、実際には解決できないシンボルの `#'X` 参照と照合（Python 製。事前の tangle が必要） |
| `make checkdoc` | Elisp の docstring/スタイルチェック |
| `make package-lint` | パッケージメタデータのチェック（任意。`load-path` に `package-lint` が必要） |

`make help` の echo 文は古い（確認済み）: `make lint` の説明行に
`check-fboundp-guards` が含まれていない。実際の `lint` の依存は上の表の
とおり（`README.org` の `* Makefile` セクションのレシピが正）。`help:` の
出力を根拠にしないこと。

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

**恒久ルール。** `README.org` が唯一の正本（§1）であり、`AGENTS.md` は
その派生的な要約である。README.org が変わると、AGENTS.md は気づかない
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
