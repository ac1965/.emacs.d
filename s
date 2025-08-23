** System Information

/Below are the system details and Emacs build configurations for two machines./

*Primary Development Machine (Apple Silicon)*

#+begin_src shell :eval never :tangle no
  uname -a
  Darwin pooh.local 24.4.0 Darwin Kernel Version 24.4.0: Fri Apr 11 18:32:05 PDT 2025; root:xnu-11417.101.15~117/RELEASE_ARM64_T8132 arm64
#+end_src

- *GNU Emacs 31.0.50*

|Property|Value|
|--------|-----|
|Commit|c3f30ee2046f4a2caeb009565ea20e977af00990|
|Branch|master|
|System|aarch64-apple-darwin24.5.0|
|Date|2025-07-27 01:07:27 (JST)|
|Patch|without ns-inline.patch|
|Features|ACL DBUS GLIB GNUTLS LCMS2 LIBXML2 MODULES NATIVE_COMP NOTIFY KQUEUE NS PDUMPER PNG RSVG SQLITE3 THREADS TOOLKIT_SCROLL_BARS TREE_SITTER WEBP XIM XWIDGETS ZLIB|
|Options|--with-native-compilation --with-gnutls=ifavailable --with-json --with-modules --with-tree-sitter --with-xml2 --with-xwidgets --with-librsvg --with-mailutils --with-native-image-api --with-cairo --with-mac --with-ns CPPFLAGS=-I/opt/homebrew/opt/llvm/include 'LDFLAGS=-L/opt/homebrew/opt/llvm/lib -L/opt/homebrew/opt/llvm/lib/c++ -Wl,-rpath,/opt/homebrew/opt/llvm/lib/c++'|

*Secondary Development Machine (Intel)*

#+begin_src shell :eval never :tangle no
  uname -a
  Darwin alice.local 24.3.0 Darwin Kernel Version 24.3.0: Fri Dec  9 19:45:54 PST 2024; root:xnu-11215.80.501.2~1/RELEASE_x86_64 x86_64
#+end_src

- *GNU Emacs 31.0.50*

|Property|Value|
|--------|-----|
|Commit|aa12cebaa684d7b3ea7e131666d33bcc71b45625|
|Branch|master|
|System|x86_64-apple-darwin24.4.0|
|Build Date|2025-03-23 10:35:38 (JST)|
|Patch Status|without ns-inline.patch|
|Key Features|NATIVE_COMP, TREE_SITTER, XWIDGETS, MODULES, SQLITE3|
|Build Options|--with-native-compilation --with-tree-sitter --with-xwidgets --with-librsvg|

