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
  DOT    ?= dot
  MMDC   ?= mmdc
  # mmdc が同梱の Chrome を解決できない環境向け。puppeteer 用の JSON 設定ファイルへの
  # パスを渡すと `mmdc -p $(PUPPETEER_CONFIG)` として使われる。空なら渡さない。
  #   例: {"executablePath": "/path/to/chrome", "args": ["--no-sandbox"]}
  PUPPETEER_CONFIG ?=

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

  # design_spec.org — 図版（*.dot / *.mmd）の tangle 元のひとつ。ORG（README.org）とは
  # 別文書だが、同じ svg/ ディレクトリへ tangle するため、dot/mermaid 系ターゲットは
  # 両方を対象にする。
  DESIGN_SPEC ?= design_spec.org
  DESIGN_SPEC := $(abspath $(ROOT)/$(DESIGN_SPEC))

  STRICT_BYTE_WARN ?= 0   # Treat byte-compile warnings as errors
  NATIVE_COMPILE   ?= 1   # Prefer native-compile if available

  # ---- Emacs の実行方法と共通の eval スニペット -----------------------------------
  EMACS_BATCH := "$(EMACS)" --batch
  EMACS_Q     := $(EMACS_BATCH) -Q

  EVAL_STRICT := $(if $(filter 1,$(STRICT_BYTE_WARN)),--eval "(setq byte-compile-error-on-warn t)",)
  EVAL_NATIVE := $(if $(filter 1,$(NATIVE_COMPILE)),--eval "(setq comp-deferred-compilation t)",)

  # org-babel-tangle は既定で各ソースブロックの「共通の先頭空白」を列（column）単位で
  # 計算して取り除く。この計算はタブをいったんスペースへ展開してから行うため、この
  # ブロックの recipe 行（先頭がタブ 1 文字）はタブが tab-width 分のスペース列へ展開
  # されたうえで共通分だけが差し引かれ、結果として recipe 行の先頭がタブではなく
  # 半端なスペース列に化けてしまう（Make が "missing separator" で落ちる原因）。
  # org-src-preserve-indentation を非 nil にして、この先頭空白の除去処理自体を
  # 無効化し、ソースに書かれたタブをタブのまま出力させる。
  #
  # 注意: .RECIPEPREFIX（GNU Make 3.82+ の機能）で recipe 先頭マーカーをタブ以外の
  # 文字に変える案も検討したが、macOS 標準の /usr/bin/make は GPLv3 回避のため
  # 3.81 に固定されており .RECIPEPREFIX を認識しない。タブそのものを使う今の方式は
  # Make のバージョンに依存しない。
  EVAL_PRESERVE_INDENT := --eval "(setq org-src-preserve-indentation t)"

  # ---- 既定ターゲット（引数なし）--------------------------------------------------
  .PHONY: all onepass-init onepass-q clean distclean show-files echo-paths tangle reload check-cookies check-tangle check-emphasis figures clean-figures dot-tangle dot-svg mmd-tangle mmd-svg diagrams clean-diagrams help
  all: onepass-init

  # ---- ワンパス（early+init 環境）: tangle -> 差分コンパイル -----------------------
  onepass-init: $(ORG)
	@echo "[onepass-init] tangle -> incremental byte-compile (init loaded)"
	@$(EMACS_BATCH) -l "$(EARLY)" -l "$(INIT)" \
  	  $(EVAL_STRICT) $(EVAL_NATIVE) \
  	  --eval "(setq org-confirm-babel-evaluate nil)" \
  	  --eval "(require 'org)" \
  	  --eval "(org-babel-tangle-file \"$(ORG)\")" \
  	  --eval "(let ((org-src-preserve-indentation t)) (org-babel-tangle-file \"$(ORG)\" nil \"makefile\"))" \
  	  --eval "(let* ((dirs (delq nil (list (and (file-directory-p \"$(LISPDIR)\") \"$(LISPDIR)\") \
  	                                        (and (file-directory-p \"$(PERSONALDIR)\") \"$(PERSONALDIR)\"))))) \
  	            (dolist (d dirs) (byte-recompile-directory d 0)) \
  	            (when (and (featurep 'comp) (bound-and-true-p comp-deferred-compilation)) \
  	              (dolist (d dirs) (ignore-errors (native-compile-async d 'recursively)))))" \
  	  --eval "(message \"[onepass-init] done\")"

  # ---- onepass-q 専用: -Q 実行時の leaf の任意注入 --------------------------------
  # -Q（init.el を読まない最小環境）は straight 経由でインストールした leaf /
  # leaf-keywords を自動では load-path に乗せない。onepass-q はこれを手動で
  # 注入するため、ここで STRAIGHT_BASE_DIR / LEAF_DIR / LEAFKW_DIR / EVAL_LEAF
  # を解決する。
  #
  # スコープの注記: この一連の変数を参照するのは onepass-q と、デバッグ用の
  # echo-paths（値の表示のみ）だけである。reload（= clean tangle
  # check-cookies）・tangle 単体・clean はいずれも参照しない。STRAIGHT_BASE_DIR
  # は再帰展開変数（?=）なので、$(shell ...) はどこかのターゲットが実際に
  # この変数を参照したときにのみ評価される — 参照されなければ、straight や
  # leaf が未インストールの環境で `make reload` 等を実行してもこの行が原因で
  # 失敗することはない。
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

  # ---- ワンパス（-Q 最小環境）: tangle -> 全コンパイル -----------------------------
  onepass-q: $(ORG)
	@echo "[onepass-q] -Q tangle -> full byte-compile (init not loaded)"
	@$(EMACS_Q) \
  	  $(EVAL_LEAF) $(EVAL_STRICT) $(EVAL_NATIVE) \
  	  --eval "(setq org-confirm-babel-evaluate nil)" \
  	  --eval "(require 'org)" \
  	  --eval "(org-babel-tangle-file \"$(ORG)\")" \
  	  --eval "(let ((org-src-preserve-indentation t)) (org-babel-tangle-file \"$(ORG)\" nil \"makefile\"))" \
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

  # ---- dot-tangle : README.org / design_spec.org の dot ソースブロックを svg/*.dot として
  # 書き出す -------------------------------------------------------------------------
  # 各 Appendix の :header-args:dot: :tangle svg/*.dot に従い、org-babel-tangle-file の
  # LANG 引数に "dot" を渡して dot 言語のブロックだけを対象に書き出す。両方の .org を
  # 対象にし、存在しない方は静かにスキップする（design_spec.org は任意）。
  dot-tangle: $(ORG)
	@echo "[dot-tangle] $(ORG), $(DESIGN_SPEC) -> $(SVGDIR)/*.dot"
	@$(EMACS_Q) \
     --eval "(require 'org)" \
     --eval "(require 'ob-core)" \
     --eval "(setq org-confirm-babel-evaluate nil noninteractive t)" \
     --eval "(dolist (f (list \"$(ORG)\" \"$(DESIGN_SPEC)\")) (when (file-exists-p f) (org-babel-tangle-file f nil \"dot\")))"

  # ---- dot-svg : svg/*.dot を dot -Tsvg で同名の svg/*.svg へレンダリングする -----------
  # figures と同じ差分ルール（*.dot が対応する *.svg より新しい、または *.svg が無い場合の
  # み再生成）。dot コマンドが無ければ、その場でインストール手順を案内して失敗する。
  # 日本語ラベルを含む図は、fontconfig で解決できる CJK 対応フォント
  # （例: Noto Sans CJK JP）が dot ソース側の fontname に指定されている前提。
  dot-svg: dot-tangle
	@command -v "$(DOT)" >/dev/null 2>&1 || { \
     echo "[dot-svg] $(DOT) not found. Install with: brew install graphviz (or apt-get install graphviz)"; \
     exit 1; \
   }
	@if [ ! -d "$(SVGDIR)" ]; then \
     echo "[dot-svg] $(SVGDIR) not found; nothing to do"; \
     exit 0; \
   fi
	@echo "[dot-svg] rendering *.dot -> *.svg under $(SVGDIR) ..."
	@count=0; \
   for f in $$(find "$(SVGDIR)" -name '*.dot' | sort); do \
     svg="$${f%.dot}.svg"; \
     if [ ! -f "$$svg" ] || [ "$$f" -nt "$$svg" ]; then \
       echo "  $$f -> $$svg"; \
       "$(DOT)" -Tsvg "$$f" -o "$$svg" || exit 1; \
       count=$$((count + 1)); \
     fi; \
   done; \
   echo "[dot-svg] done ($$count rendered)"

  # ---- mmd-tangle : README.org / design_spec.org の mermaid ソースブロックを svg/*.mmd
  # として書き出す ---------------------------------------------------------------------
  # :header-args:mermaid: :tangle svg/*.mmd に従い、LANG 引数に "mermaid" を渡して
  # tangle する。現状 mermaid ブロックを持つのは README.org（tangle-flow.mmd）のみだが、
  # design_spec.org 側に増えても同じ仕組みで拾えるよう両方を対象にしておく。
  mmd-tangle: $(ORG)
	@echo "[mmd-tangle] $(ORG), $(DESIGN_SPEC) -> $(SVGDIR)/*.mmd"
	@$(EMACS_Q) \
     --eval "(require 'org)" \
     --eval "(require 'ob-core)" \
     --eval "(setq org-confirm-babel-evaluate nil noninteractive t)" \
     --eval "(dolist (f (list \"$(ORG)\" \"$(DESIGN_SPEC)\")) (when (file-exists-p f) (org-babel-tangle-file f nil \"mermaid\")))"

  # ---- mmd-svg : svg/*.mmd を mmdc で同名の svg/*.svg へレンダリングする ----------------
  # mmdc（@mermaid-js/mermaid-cli）は内部で Puppeteer 経由の Chrome を必要とする。
  # mmdc が同梱する puppeteer-core は特定バージョンの Chrome に固定されているため、
  # `npx puppeteer browsers install chrome`（や chrome-headless-shell）で入れた
  # 別バージョンでは一致せず、依然として "Could not find Chrome" で失敗することを実機で
  # 確認済み。確実なのは PUPPETEER_CONFIG に executablePath を明示すること —
  # バージョン不一致があっても executablePath を直接指定すれば通ることを確認済み。
  # （例: make mmd-svg PUPPETEER_CONFIG=puppeteer-config.json）。
  mmd-svg: mmd-tangle
	@command -v "$(MMDC)" >/dev/null 2>&1 || { \
     echo "[mmd-svg] $(MMDC) not found. Install with: npm install -g @mermaid-js/mermaid-cli"; \
     exit 1; \
   }
	@if [ ! -d "$(SVGDIR)" ]; then \
     echo "[mmd-svg] $(SVGDIR) not found; nothing to do"; \
     exit 0; \
   fi
	@echo "[mmd-svg] rendering *.mmd -> *.svg under $(SVGDIR) ..."
	@count=0; \
   for f in $$(find "$(SVGDIR)" -name '*.mmd' | sort); do \
     svg="$${f%.mmd}.svg"; \
     if [ ! -f "$$svg" ] || [ "$$f" -nt "$$svg" ]; then \
       echo "  $$f -> $$svg"; \
       if [ -n "$(PUPPETEER_CONFIG)" ]; then \
         "$(MMDC)" -i "$$f" -o "$$svg" -b transparent -p "$(PUPPETEER_CONFIG)" || { \
           echo "[mmd-svg] mmdc failed even with PUPPETEER_CONFIG=$(PUPPETEER_CONFIG)."; \
           echo "[mmd-svg] Check that the executablePath inside it actually exists."; \
           exit 1; \
         }; \
       else \
         "$(MMDC)" -i "$$f" -o "$$svg" -b transparent || { \
           echo "[mmd-svg] mmdc failed. mmdc's bundled puppeteer-core is pinned to a"; \
           echo "[mmd-svg] specific Chrome build, so \"npx puppeteer browsers install\""; \
           echo "[mmd-svg] usually fetches a DIFFERENT version and still fails to match."; \
           echo "[mmd-svg] The reliable fix is to point PUPPETEER_CONFIG at any Chrome/"; \
           echo "[mmd-svg] Chromium binary you already have (version mismatch is fine"; \
           echo "[mmd-svg] once executablePath is set explicitly):"; \
           echo "[mmd-svg]   {\"executablePath\": \"/path/to/chrome\", \"args\": [\"--no-sandbox\"]}"; \
           echo "[mmd-svg]   make mmd-svg PUPPETEER_CONFIG=puppeteer-config.json"; \
           exit 1; \
         }; \
       fi; \
       count=$$((count + 1)); \
     fi; \
   done; \
   echo "[mmd-svg] done ($$count rendered)"

  # ---- diagrams : dot-svg + mmd-svg をまとめて実行するショートハンド --------------------
  diagrams: dot-svg mmd-svg
	@echo "[diagrams] $(SVGDIR)/*.dot, $(SVGDIR)/*.mmd -> $(SVGDIR)/*.svg up to date"

  # ---- clean-diagrams : diagrams が生成した *.dot / *.mmd / *.svg を削除 ---------------
  # *.dot・*.mmd と同名の *.svg のみを対象とする（$(SVGDIR) 配下に手動で置いた無関係な
  # ファイル、たとえば demo.png や emacs_keybindings_cheatsheet.svg のような tangle 元を
  # 持たない手動アセットを誤って消さないよう、tangle 由来のソースが存在するものだけ削除する）。
  clean-diagrams:
	@echo "[clean-diagrams] remove generated *.dot / *.mmd / *.svg under $(SVGDIR)"
	@if [ -d "$(SVGDIR)" ]; then \
     for f in $$(find "$(SVGDIR)" \( -name '*.dot' -o -name '*.mmd' \) | sort); do \
       svg="$${f%.*}.svg"; \
       [ -f "$$svg" ] && rm -f "$$svg"; \
       rm -f "$$f"; \
     done; \
   fi

  # ---- help : 主要ターゲットの実行例を一覧表示する -----------------------------------
  help:
	@echo "使い方の例（ROOT はこの Makefile があるディレクトリ）:"
	@echo ""
	@echo "  # ビルド"
	@echo "  make                          # 既定 = onepass-init（tangle -> 差分バイトコンパイル）"
	@echo "  make onepass-q                # -Q 最小環境で tangle -> 全バイトコンパイル"
	@echo "  make tangle                   # README.org を .el 群へ tangle するだけ"
	@echo "  make reload                   # clean + tangle + check-cookies（README.org 差し替え後に使う）"
	@echo "  make clean                    # *.elc を削除"
	@echo "  make distclean                # clean + 迷子の *.eln を削除"
	@echo ""
	@echo "  # 図版（dot / mermaid）"
	@echo "  make diagrams                 # dot-svg + mmd-svg をまとめて実行（推奨はこれ）"
	@echo "  make dot-tangle               # README.org / design_spec.org の dot ブロックを svg/*.dot へ"
	@echo "  make dot-svg                  # svg/*.dot を dot -Tsvg で svg/*.svg へレンダリング"
	@echo "  make mmd-tangle               # 同様に mermaid ブロックを svg/*.mmd へ"
	@echo "  make mmd-svg                  # svg/*.mmd を mmdc で svg/*.svg へレンダリング"
	@echo "  make mmd-svg PUPPETEER_CONFIG=puppeteer-config.json"
	@echo "                                # mmdc が Chrome を自動検出できない環境向け"
	@echo "  make clean-diagrams           # diagrams が生成した *.dot / *.mmd / *.svg を削除"
	@echo "  make figures                  # diagrams 実行後、svg/*.svg を同名の *.pdf へ変換（LaTeX/PDF export 用）"
	@echo "  make clean-figures            # figures が生成した *.pdf を削除"
	@echo ""
	@echo "  # 静的チェック"
	@echo "  make lint                     # check-tangle + check-emphasis + check-cookies + checkdoc"
	@echo "  make check-tangle             # :tangle を継承できていない src block を検出"
	@echo "  make check-emphasis           # 効かない =verbatim=/~code~/*bold* を検出"
	@echo "  make check-fboundp-guards     # (fboundp 'X) ガードと #'X 参照の整合性を検査"
	@echo "  make check-cookies            # 全 .el の lexical-binding クッキーを検証"
	@echo "  make checkdoc                 # 公開 defun の docstring を検証"
	@echo "  make package-lint             # MELPA 形式のパッケージヘッダ検証（要 package-lint）"
	@echo ""
	@echo "  # その他"
	@echo "  make show-files               # lisp/ と personal/ 配下の .el 一覧"
	@echo "  make echo-paths               # 解決済みの各種パスを表示（デバッグ用）"
	@echo ""
	@echo "個別の変数上書き例: make onepass-init STRICT_BYTE_WARN=1 NATIVE_COMPILE=0"

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
  # diagrams（dot/mmd -> svg）を先に実行し、変換元の *.svg が揃っていることを保証してから
  # svg -> pdf の変換に入る。
  #
  figures: diagrams
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
  	  --eval "(org-babel-tangle-file \"$(ORG)\")" \
  	  $(EVAL_PRESERVE_INDENT) \
  	  --eval "(org-babel-tangle-file \"$(ORG)\" nil \"makefile\")"

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

  # ---- check-fboundp-guards : (fboundp 'X) ガード + #'X 参照の整合性を検査する -------
  # (when/if (fboundp 'X) ... #'X ...) という形（X をコマンドとして登録・束縛している
  # 箇所）だけを対象に、X が実際に解決可能か（ローカル定義 / leaf の :commands /
  # 同名 leaf パッケージの eager load / ドキュメント上の autoload 明記）を機械的に
  # 検査する。dev-music.el の emms 抜け、restart-emacs の宣言忘れ、のような
  # 「エラーは出ないが黙って何も起きない」バグを次に防ぐためのツール。
  # $(LISPDIR) と $(PERSONALDIR) を対象にするため、事前に tangle 済みであること。
  # 検査本体は scripts/check_fboundp_guards.py（Appendix を参照）。
  .PHONY: check-fboundp-guards
  check-fboundp-guards:
	@python3 scripts/check_fboundp_guards.py "$(LISPDIR)" "$(PERSONALDIR)" "$(EARLY)" "$(INIT)"

  # ---- lint : check-tangle + check-emphasis + check-cookies + check-fboundp-guards +
  # checkdoc をまとめて実行 -----------------------------------------------------------
  # コミット前に静的品質チェックをすべて走らせるための単一ターゲット。
  .PHONY: checkdoc lint
  lint: check-tangle check-emphasis check-cookies check-fboundp-guards checkdoc
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
