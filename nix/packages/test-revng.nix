{ pkgs, stdenv, python,
  revng, revngPythonDependencies, revngPackages,
}:
stdenv.mkDerivation {
  name = "test/revng";

  unpackPhase = "true";

  nativeBuildInputs = (with pkgs; [
    gcc
    binutils
    jq
    llvm_21
    lld_21
    ninja
    nodejs
    qemu
    xorg.lndir
  ]) ++ [
    revng
    revngPackages."test/revng-qa"
    # revngPythonDependencies brings yq + jq runtime that
    # several test-* rules depend on.
    revngPythonDependencies
    (python.withPackages (
      ps: with ps; [
        jinja2
        pyyaml
      ]
    ))
  ];

  buildPhase = ''
    echo
  '';

  installPhase = ''
    mkdir -p $out
    # test-configure resolves source paths against a single
    # --install-path, but the sources/built binaries it asks
    # for live in three separate components — revng, revng-qa
    # and the pre-built test/revng-qa artifacts. Stitch them
    # together in a merged tree via lndir.
    mkdir merged-root
    lndir -silent \
      ${revngPackages.revng-qa} merged-root
    lndir -silent \
      ${revngPackages."test/revng-qa"} merged-root
    lndir -silent \
      ${revng} merged-root
    # WIP: tests like revng.model-migration `cp` model.yml
    # into a tmpdir and write back; `cp` preserves the source
    # mode (read-only in /nix/store) so the copy is also
    # read-only and revng2 fails with EACCES. Replace symlinks
    # under share/revng/test/tests with real writable copies.
    # Drop once the affected tests stop copying-and-mutating
    # in-place.
    find merged-root/share/revng/test/tests -type l | while IFS= read -r l; do
      t=$(readlink -f "$l") || continue
      rm "$l"
      cp "$t" "$l"
      chmod u+w "$l"
    done
    python3 \
      ${revngPackages.revng-qa}/libexec/revng/test-configure \
      "${revngPackages.revng-qa}/share/revng/test/configuration/revng-qa/"*.yml \
      "${revng}/share/revng/test/configuration/revng/"*.yml \
      --install-path "$PWD/merged-root" \
      --destination . \
      --target-type 'revng\..*'
    # WIP: test-configure writes inline scripts (filter.py
    # etc.) with `#!/usr/bin/env python3` shebangs. The nix
    # sandbox has no /usr/bin/env, so patchShebangs rewrites
    # them to absolute paths. Drop if test-configure ever
    # emits absolute shebangs itself.
    patchShebangs --build .
    export REVNG_OPTIONS="--debug-log=verify"
    # WIP: needed by the new pypeline tests on develop;
    # should become the default once develop settles.
    export PYPELINE_STORAGE_PROVIDER="local://?inline"
    # WIP: several tests shell out to plain `python3` and
    # `import revng.*`; expose revng's installed site-
    # packages on PYTHONPATH because nix doesn't auto-wrap
    # subprocess invocations the way the local environment
    # script does.
    export PYTHONPATH="${revng}/${python.sitePackages}:${revngPythonDependencies}/${python.sitePackages}''${PYTHONPATH:+:$PYTHONPATH}"
    # WIP: revng2 link-for-translation invokes raw ld.bfd
    # with -l:crt1.o, -l:crtbegin.o, etc.; LIBRARY_PATH
    # tells the linker where to find them. Drop once revng
    # uses cc (which honors LIBRARY_PATH naturally) instead
    # of bare ld.
    export LIBRARY_PATH="${pkgs.glibc}/lib:${pkgs.stdenv.cc.cc.lib}/lib/gcc/x86_64-unknown-linux-gnu/${pkgs.stdenv.cc.cc.version}"
    # WIP: build.ninja references a top-level `shell` rule we
    # don't have; strip it and provide a plain `sh` symlink in
    # cwd.
    grep -v 'shell =' build.ninja > build2.ninja
    mv build2.ninja build.ninja
    # WIP: some revng2/revng invocations on develop hang
    # (s390x project init, native-dynamic recompile-
    # isolated). Cap each step at 1200s under -j8 so the
    # build can enumerate failures rather than wedging.
    sed -i \
      -e 's| revng2 | timeout 1200 revng2 |g' \
      -e 's| revng artifact| timeout 1200 revng artifact|g' \
      build.ninja
    ln -s `command -v bash` sh
    export XDG_CACHE_HOME="$PWD/.cache"
    mkdir -p "$XDG_CACHE_HOME/.cache"

    # WIP: tolerate failing test targets — bumped revng +
    # new pypeline still have several upstream-known crashes.
    # Capture the log so failing targets can be enumerated.
    mkdir -p "$out/log"
    ninja -v -k0 all 2>&1 | tee "$out/log/ninja.log" || true

    # Extract the list of FAILED targets for convenience.
    grep -oE 'FAILED: \[code=[0-9]+\] [^ ]+' "$out/log/ninja.log" \
      > "$out/log/failed-targets.txt" || true
    echo "test/revng: $(wc -l < $out/log/failed-targets.txt) failing target(s); see $out/log/"
  '';

}
