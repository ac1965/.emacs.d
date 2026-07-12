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

# これらは常にリポジトリルート直下として扱う
LISPDIR_REL     ?= lisp
PERSONALDIR_REL ?= personal

LISPDIR     := $(abspath $(ROOT)/$(LISPDIR_REL))
PERSONALDIR := $(abspath $(ROOT)/$(PERSONALDIR_REL))
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
.PHONY: all onepass-init onepass-q clean distclean show-files echo-paths tangle reload check-cookies
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

# ---- lint : checkdoc + check-cookies をまとめて実行 -------------------------------
# コミット前に静的品質チェックをすべて走らせるための単一ターゲット。
.PHONY: checkdoc lint
lint: check-cookies checkdoc
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
