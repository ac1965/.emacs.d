# Makefile — モジュール式 Emacs 設定のためのワンパス・ビルダー
# - 既定 / `make all` : onepass-init（tangle -> 差分バイトコンパイル）
# - `make onepass-q`  : -Q（最小環境）で tangle -> 全バイトコンパイル
# - パスはリポジトリルートから絶対化する（"lisp/personal" の混同を防ぐため）

SHELL := /bin/sh

# ---- リポジトリルートと絶対化したディレクトリ -----------------------------------
ROOT := $(CURDIR)

EMACS  ?= emacs
ORG    ?= README.org
EARLY  ?= early-init.el
INIT   ?= init.el
RSVG_CONVERT ?= rsvg-convert

# これらは常にリポジトリルート直下として扱う
LISPDIR_REL     ?= lisp
PERSONALDIR_REL ?= personal
SVGDIR_REL      ?= svg

LISPDIR     := $(abspath $(ROOT)/$(LISPDIR_REL))
PERSONALDIR := $(abspath $(ROOT)/$(PERSONALDIR_REL))
SVGDIR      := $(abspath $(ROOT)/$(SVGDIR_REL))
ORG := $(abspath $(ROOT)/$(ORG))
EARLY := $(abspath $(ROOT)/$(EARLY))
INIT  := $(abspath $(ROOT)/$(INIT))

STRICT_BYTE_WARN ?= 0   # Treat byte-compile warnings as errors
NATIVE_COMPILE   ?= 1   # Prefer native-compile if available

# ---- Emacs の実行方法と共通の eval スニペット -----------------------------------
EMACS_BATCH := "$(EMACS)" --batch
EMACS_Q     := $(EMACS_BATCH) -Q

EVAL_STRICT := $(if $(filter 1,$(STRICT_BYTE_WARN)),--eval "(setq byte-compile-error-on-warn t)",)
EVAL_NATIVE := $(if $(filter 1,$(NATIVE_COMPILE)),--eval "(setq comp-deferred-compilation t)",)

# -Q 実行時の leaf の任意注入
STRAIGHT_BASE_DIR ?= $(shell \
  if [ -f "$(EARLY)" ]; then \
    $(EMACS_Q) -l "$(EARLY)" \
      --eval "(princ (expand-file-name (or (ignore-errors STRAIGHT_BASE_DIR) \
                                           (ignore-errors (and (boundp 'straight-base-dir) straight-base-dir)) \
                                           (expand-file-name \"straight\" user-emacs-directory))))"; \
  else \
    printf "%s" "$$HOME/.emacs.d/straight"; \
  fi)
LEAF_DIR   := $(STRAIGHT_BASE_DIR)/repos/leaf
LEAFKW_DIR := $(STRAIGHT_BASE_DIR)/repos/leaf-keywords

EVAL_LEAF := \
  --eval "(let* ((ldir \"$(LEAF_DIR)\") (kwdir \"$(LEAFKW_DIR)\")) \
            (when (file-directory-p ldir)  (add-to-list 'load-path ldir)) \
            (when (file-directory-p kwdir) (add-to-list 'load-path kwdir)) \
            (ignore-errors (require 'leaf)) \
            (ignore-errors (require 'leaf-keywords)) \
            (when (featurep 'leaf-keywords) (leaf-keywords-init)))"

# ---- 既定ターゲット（引数なし）--------------------------------------------------
.PHONY: all onepass-init onepass-q clean distclean show-files echo-paths tangle reload check-cookies check-tangle check-emphasis figures clean-figures
all: onepass-init

# ---- ワンパス（early+init 環境）: tangle -> 差分コンパイル -----------------------
onepass-init: $(ORG)
	@echo "[onepass-init] tangle -> incremental byte-compile (init loaded)"
	@$(EMACS_BATCH) -l "$(EARLY)" -l "$(INIT)" \
	  $(EVAL_STRICT) $(EVAL_NATIVE) \
	  --eval "(setq org-confirm-babel-evaluate nil)" \
	  --eval "(require 'org)" \
	  --eval "(org-babel-tangle-file \"$(ORG)\")" \
	  --eval "(let* ((dirs (delq nil (list (and (file-directory-p \"$(LISPDIR)\") \"$(LISPDIR)\") \
	                                        (and (file-directory-p \"$(PERSONALDIR)\") \"$(PERSONALDIR)\"))))) \
	            (dolist (d dirs) (byte-recompile-directory d 0)) \
	            (when (and (featurep 'comp) (bound-and-true-p comp-deferred-compilation)) \
	              (dolist (d dirs) (ignore-errors (native-compile-async d 'recursively)))))" \
	  --eval "(message \"[onepass-init] done\")"

# ---- ワンパス（-Q 最小環境）: tangle -> 全コンパイル -----------------------------
onepass-q: $(ORG)
	@echo "[onepass-q] -Q tangle -> full byte-compile (init not loaded)"
	@$(EMACS_Q) \
	  $(EVAL_LEAF) $(EVAL_STRICT) $(EVAL_NATIVE) \
	  --eval "(setq org-confirm-babel-evaluate nil)" \
	  --eval "(require 'org)" \
	  --eval "(org-babel-tangle-file \"$(ORG)\")" \
	  --eval "(let* ((dirs (delq nil (list (and (file-directory-p \"$(LISPDIR)\") \"$(LISPDIR)\") \
	                                        (and (file-directory-p \"$(PERSONALDIR)\") \"$(PERSONALDIR)\"))))) \
	            (dolist (d dirs) (byte-recompile-directory d t)) \
	            (when (and (featurep 'comp) (bound-and-true-p comp-deferred-compilation)) \
	              (dolist (d dirs) (ignore-errors (native-compile-async d 'recursively)))))" \
	  --eval "(message \"[onepass-q] done\")"

# ---- ユーティリティ -------------------------------------------------------------
show-files:
	@echo "[list] $(LISPDIR)";    { [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.el' | sort; } || true
	@echo "[list] $(PERSONALDIR)"; { [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.el' | sort; } || true

echo-paths:
	@echo "ROOT=$(ROOT)"; \
	echo "EARLY=$(EARLY)"; \
	echo "INIT=$(INIT)"; \
	echo "LISPDIR=$(LISPDIR)"; \
	echo "PERSONALDIR=$(PERSONALDIR)"; \
	echo "SVGDIR=$(SVGDIR)"; \
	echo "STRAIGHT_BASE_DIR=$(STRAIGHT_BASE_DIR)"; \
	echo "LEAF_DIR=$(LEAF_DIR)"; \
	echo "LEAFKW_DIR=$(LEAFKW_DIR)"

clean:
	@echo "[clean] remove *.elc under $(LISPDIR) and $(PERSONALDIR)"
	@{ [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.elc' -delete; } 2>/dev/null || true
	@{ [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.elc' -delete; } 2>/dev/null || true

distclean: clean
	@echo "[distclean] remove stray *.eln"
	@find "$(ROOT)" -type f -name '*.eln' -delete

# ---- figures : *.svg を rsvg-convert で同名の *.pdf へ事前変換 -------------------
# design_spec.org（および他の org 文書）の LaTeX/PDF エクスポートは、SVG を
# svg パッケージ経由で Inkscape に渡す方式を採用していない（mactex-no-gui 環境に
# Inkscape が無く、実機で `Inkscape version not detected` により失敗することを
# 確認済み）。代わりに $(SVGDIR)/*.svg を事前に同名の *.pdf へ変換しておき、
# orgx-export--svg-to-pdf-graphics（orgx/orgx-export.el 側）が LaTeX エクスポート時に
# \includegraphics の拡張子を .svg -> .pdf に書き換える運用を前提とする。
#
# 差分のみ変換する（*.svg が対応する *.pdf より新しい、または *.pdf が無い場合のみ
# rsvg-convert を実行）。rsvg-convert コマンドが無ければ、その場でインストール手順を
# 案内して失敗する（CI では未インストールを黙って無視しない）。
figures:
	@command -v "$(RSVG_CONVERT)" >/dev/null 2>&1 || { \
	  echo "[figures] $(RSVG_CONVERT) not found. Install with: brew install librsvg"; \
	  exit 1; \
	}
	@if [ ! -d "$(SVGDIR)" ]; then \
	  echo "[figures] $(SVGDIR) not found; nothing to do"; \
	  exit 0; \
	fi
	@echo "[figures] converting *.svg -> *.pdf under $(SVGDIR) ..."
	@count=0; \
	 for f in $$(find "$(SVGDIR)" -name '*.svg' | sort); do \
	   pdf="$${f%.svg}.pdf"; \
	   if [ ! -f "$$pdf" ] || [ "$$f" -nt "$$pdf" ]; then \
	     echo "  $$f -> $$pdf"; \
	     "$(RSVG_CONVERT)" -f pdf -o "$$pdf" "$$f" || exit 1; \
	     count=$$((count + 1)); \
	   fi; \
	 done; \
	 echo "[figures] done ($$count converted)"

# ---- clean-figures : figures が生成した *.pdf を削除 ------------------------------
# *.svg と同名の *.pdf のみを対象とする（$(SVGDIR) 配下に手動で置いた無関係な
# *.pdf を誤って消さないよう、対応する *.svg が存在するものだけ削除する）。
clean-figures:
	@echo "[clean-figures] remove generated *.pdf under $(SVGDIR)"
	@if [ -d "$(SVGDIR)" ]; then \
	  for f in $$(find "$(SVGDIR)" -name '*.svg' | sort); do \
	    pdf="$${f%.svg}.pdf"; \
	    [ -f "$$pdf" ] && rm -f "$$pdf"; \
	  done; \
	fi

tangle:
	@echo "[tangle] $(ORG)"
	@$(EMACS_Q) \
	  --eval "(require 'org)" \
	  --eval "(require 'ob-core)" \
	  --eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))" \
	  --eval "(setq org-confirm-babel-evaluate nil noninteractive t)" \
	  --eval "(org-babel-tangle-file \"$(ORG)\")"

# ---- check-cookies : 全 .el の 1 行目にある lexical-binding クッキーを検証 -------
# `make tangle' の後に実行し、生成物すべてにクッキーがあることを確認する。
# すべて合格なら 0 を返す。欠落があれば該当ファイルを列挙して 1 を返す。
check-cookies:
	@echo "[check-cookies] scanning $(LISPDIR) ..."
	@fail=0; \
	 for f in $$(find "$(LISPDIR)" -name '*.el' | sort); do \
	   line1=$$(head -1 "$$f"); \
	   if ! echo "$$line1" | grep -q 'lexical-binding: t'; then \
	     echo "  MISSING: $$f"; \
	     echo "    line1: $$line1"; \
	     fail=1; \
	   fi; \
	 done; \
	 if [ $$fail -eq 0 ]; then \
	   echo "[check-cookies] all files OK"; \
	 else \
	   echo "[check-cookies] FAILED — run: make reload"; \
	   exit 1; \
	 fi

# ---- reload : .elc を削除してから再 tangle ---------------------------------------
# README.org を新しい Claude の出力に差し替えた後に使う。古いバイトコンパイル済み
# モジュールが、新しく tangle した .el を覆い隠さないことを保証するためである。
# このターゲットは、ソースには存在しない警告をディスク上の古いモジュールが出すという
# 古いモジュールが出す）に対する運用上の対処である。
reload: clean tangle check-cookies
	@echo "[reload] on-disk modules now reflect $(ORG); restart Emacs to load."

# ---- checkdoc : すべての公開 defun の docstring を検証 ---------------------------
# LISPDIR 配下の全 .el を走査し、docstring の欠落や不正を検出する。
# 全ファイルが合格すれば 0、警告が出れば 1 を返す。
checkdoc:
	@echo "[checkdoc] scanning $(LISPDIR) ..."; \
	 fail=0; \
	 for f in $$(find "$(LISPDIR)" -name '*.el' | sort); do \
	   result=$$($(EMACS_Q) --eval \
	     "(progn (find-file \"$$f\") (checkdoc-current-buffer t) (kill-buffer))" \
	     2>&1); \
	   if echo "$$result" | grep -q 'Warning\|Error'; then \
	     echo "  FAIL: $$f"; echo "$$result" | grep 'Warning\|Error' | head -5; \
	     fail=1; \
	   fi; \
	 done; \
	 [ $$fail -eq 0 ] && echo "[checkdoc] all files OK" || exit 1

# ---- checkdoc-strict : 失敗時に終了する checkdoc ---------------------------------
# 公開 defun に docstring が無いときビルドを失敗させたい CI で使う。
# Emacs セッションがモジュールの load-path を参照できる必要がある。

# ---- check-tangle : tangle 先を継承できない emacs-lisp ブロックを検出 -------------
# Org の :header-args: はサブツリーにのみ継承される。見出しレベルを 1 段間違えて
# "**** foo Design Notes" を "*** foo Design Notes" と書くと、親（*** foo）の
# 兄弟になってしまい、その配下の src ブロックが :tangle を受け取れない。
# tangle は静かに成功し、当該 .el だけが生成されない — 実機で 2 回踏んだ事故である。
#
# 本ターゲットは README.org を Org のパーサで走査し、実効 :tangle が nil または
# "no" の emacs-lisp ブロックを行番号付きで列挙して失敗する。
.PHONY: check-tangle
check-tangle:
	@echo "[check-tangle] verifying :tangle inheritance in $(ORG) ..."
	@$(EMACS) -Q --batch \
	  --eval "(require 'org)" \
	  --eval "(with-current-buffer (find-file-noselect \"$(ORG)\") \
	            (let ((bad 0)) \
	              (org-babel-map-src-blocks nil \
	                (when (string= lang \"emacs-lisp\") \
	                  (let ((tgt (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info t)))))) \
	                    (when (or (null tgt) (equal tgt \"no\")) \
	                      (setq bad (1+ bad)) \
	                      (message \"  ORPHAN src block at line %d\" (line-number-at-pos)))))) \
	              (if (> bad 0) \
	                  (progn (message \"[check-tangle] FAILED: %d block(s) without a tangle target\" bad) \
	                         (kill-emacs 1)) \
	                (message \"[check-tangle] ok\")))))"

# ---- check-emphasis : 効かない Org 強調記法（=verbatim=/~code~/*bold*）を検出 -----
# org-emphasis-regexp-components の既定値では、開始マーカーの直前・終了マーカーの
# 直後が半角空白・タブ・- – — ( " ' { （直前側）／ . , : ! ? ; ' " ) } [ （直後側）・
# 行頭・行末のいずれかでなければ強調は成立しない。全角文字（、。（）「」等）は
# この集合に含まれないため、和文の直後に =code= を続けるとマーカーが素通しされ、
# リテラルの "=" 文字としてそのまま表示される（シンタックスハイライトも失われる）。
# tangle には影響しないため、見た目の欠陥として気づかれにくい。
#
# 検査本体は scripts/check_emphasis.py（Appendix を参照）。標準ライブラリのみに
# 依存する Python スクリプトであり、Emacs のバッチモードは介さない。
.PHONY: check-emphasis
check-emphasis:
	@python3 scripts/check_emphasis.py "$(ORG)"

# ---- lint : check-tangle + check-emphasis + check-cookies + checkdoc をまとめて実行 --
# コミット前に静的品質チェックをすべて走らせるための単一ターゲット。
.PHONY: checkdoc lint
lint: check-tangle check-emphasis check-cookies checkdoc
	@echo "[lint] all checks passed"

# ---- package-lint : 任意 — load-path 上に package-lint が必要 --------------------
# MELPA 形式のパッケージヘッダと依存宣言を検証する。
# 個人設定のワークフローでは不要だが、モジュールを独立パッケージとして切り出す際に
# 有用である。
# インストール: M-x package-install RET package-lint RET
.PHONY: package-lint
package-lint:
	@echo "[package-lint] scanning $(LISPDIR) ..."; \
	 $(EMACS_Q) $(EVAL_LEAF) \
	   --eval "(require 'package-lint nil t)" \
	   --eval "(if (featurep 'package-lint) \
	               (let ((files (directory-files-recursively \"$(LISPDIR)\" \"\\\\.el$$\"))) \
	                 (dolist (f files) (package-lint-batch-and-exit))) \
	               (message \"[package-lint] not installed; skipping\"))""
