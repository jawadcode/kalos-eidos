# Kalos Eidos

Just following along with [https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl01.html].

# Build Instructions

## Debug

```bash
# -Db_lundef=false is due to https://github.com/mesonbuild/meson/issues/764
# We also can't use `--buildtype=debug` as this causes `_FORTIFY_SOURCE` warnings
meson setup builddir/debug --buildtype=plain -Db_sanitize=address,undefined -Db_lundef=false
meson compile -C builddir/debug --verbose
```

## Release

```bash
meson setup builddir/release --buildtype=release -Db_lto=true
meson compile -C builddir/release
```
