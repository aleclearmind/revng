{ pkgs, windowsCross, windowsLlvm }:
# Workaround for an lld-on-mingw quirk: `--out-implib` writes archive
# members named `libFoo.dll` (matching the DLL filename). Downstream
# `ld.lld` invocations classify archive members by name extension —
# `.dll` members are treated as DLL files and rejected as link inputs
# with "unknown file type: libFoo.dll". binutils dlltool writes
# import libs with `.o`-suffixed member names that lld accepts.
#
# Rather than rebuild LLVM after every implib-policy tweak, this
# derivation symlinks the existing `windowsLlvm` and rewrites only
# `lib/*.dll.a` using `dlltool` + a `.def` derived from each matching
# `.dll`'s export table.
let
  dlltool = "${pkgs.pkgsCross.mingwW64.buildPackages.binutils}/bin/x86_64-w64-mingw32-dlltool";
  objdump = "${pkgs.pkgsCross.mingwW64.buildPackages.binutils}/bin/x86_64-w64-mingw32-objdump";
in
pkgs.runCommand "windows-llvm-implib-fixed-${windowsLlvm.name or "x86_64-w64-mingw32"}"
{
  passthru = {
    inherit (windowsLlvm) outPath drvPath;
    original = windowsLlvm;
  };
}
  ''
    # `cp -a`-style copy is cheap because we hardlink everything from
    # the original out-path back into ours, then replace just the
    # .dll.a files in place.
    mkdir -p "$out"
    cp -a --reflink=auto "${windowsLlvm}"/. "$out"/
    chmod -R u+w "$out"

    for dlla in "$out"/lib/*.dll.a; do
      base=$(basename "$dlla" .dll.a)
      dll="$out/libexec/$base.dll"
      [ -e "$dll" ] || continue

      defFile=$(mktemp --suffix=.def)
      {
        echo "LIBRARY $base.dll"
        echo "EXPORTS"
        ${objdump} -p "$dll" \
          | awk '/^\[Ordinal\/Name Pointer\] Table/{p=1; next} p && /^[[:space:]]+\[[[:space:]]*[0-9]+\]/{print $NF}'
      } > "$defFile"

      rm -f "$dlla"
      ${dlltool} --input-def "$defFile" --dllname "$base.dll" --output-lib "$dlla"
      rm -f "$defFile"
    done
  ''
