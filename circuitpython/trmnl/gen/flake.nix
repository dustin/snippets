{
  description = "Render HTML pages to 296x128 4-gray BMPs for an Adafruit MagTag";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];

      forAll = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs {
        inherit system;
        # allow only this one unfree package (macOS has no nixpkgs chromium)
        config.allowUnfreePredicate = pkg:
          builtins.elem (nixpkgs.lib.getName pkg) [ "google-chrome" ];
      }));

      mkRender = pkgs:
        let
          browser =
            if pkgs.stdenv.isLinux
            then "${pkgs.chromium}/bin/chromium"
            else "${pkgs.google-chrome}/bin/google-chrome-stable";
        in
        pkgs.writeShellApplication {
          name = "magtag-render";
          runtimeInputs = [ pkgs.python3 pkgs.imagemagick pkgs.coreutils pkgs.findutils pkgs.curl ];
          text = ''
            # usage: magtag-render <page-url> [dither]
            #   dither: None (default, best for text) or FloydSteinberg (photos/gradients)
            #   the page must set <html data-ready> once it has drawn
            #   uploads to $MAGTAG_PUT_URL (default http://bee1:1880/magtrmnl.bmp)
            #   gives up after $MAGTAG_TIMEOUT seconds (default 30) waiting for data-ready
            #
            # Caching: the page is fetched as served (one plain HTTP GET) and its bytes,
            # with the URL and dither, key a cache of rendered BMPs. On a hit the browser
            # is skipped and the cached BMP is uploaded again.
            #   MAGTAG_MAX_AGE    seconds a cached render may be reused (default 3600;
            #                     covers pages that change with the clock, not the HTML)
            #   MAGTAG_CACHE_DIR  default ~/.cache/magtag-render
            #   MAGTAG_FORCE=1    always render
            #   MAGTAG_VERBOSE=1  say whether it rendered or reused
            if [ $# -lt 1 ]; then
              echo "usage: magtag-render <page-url> [None|FloydSteinberg]" >&2
              exit 2
            fi

            url="$1"
            dither="''${2:-None}"
            put_url="''${MAGTAG_PUT_URL:-http://bee1:1880/magtrmnl.bmp}"
            timeout_s="''${MAGTAG_TIMEOUT:-30}"
            max_age="''${MAGTAG_MAX_AGE:-3600}"
            cache_dir="''${MAGTAG_CACHE_DIR:-''${XDG_CACHE_HOME:-$HOME/.cache}/magtag-render}"
            browser="''${CHROME:-${browser}}"

            say() { if [ -n "''${MAGTAG_VERBOSE:-}" ]; then echo "magtag-render: $*" >&2; fi; }

            tmp=$(mktemp -d)
            trap 'rm -rf "$tmp"' EXIT

            # Look for a cached render of exactly this page content.
            cached=""
            mkdir -p "$cache_dir"
            find "$cache_dir" -name '*.bmp' -mmin "+$(( max_age / 60 + 1 ))" -delete 2>/dev/null || true
            if [ -z "''${MAGTAG_FORCE:-}" ] && curl -fsS --max-time 15 -o "$tmp/page" "$url"; then
              key=$( { printf '%s\n%s\n' "$url" "$dither"; cat "$tmp/page"; } | sha256sum | cut -c1-32)
              cached="$cache_dir/$key.bmp"
            fi

            if [ -n "$cached" ] && [ -s "$cached" ] \
               && [ $(( $(date +%s) - $(stat -c %Y "$cached") )) -lt "$max_age" ]; then
              say "page unchanged, reusing render from $(date -r "$cached" '+%F %T')"
              bmp="$cached"
            else
              if [ ! -x "$browser" ]; then
                echo "magtag-render: browser not found: $browser (set CHROME)" >&2
                exit 1
              fi

              python3 ${./shot.py} "$browser" "$url" "$tmp/screen.png" "$timeout_s"

              magick "$tmp/screen.png" -colorspace Gray -dither "$dither" -posterize 4 \
                -type Palette "BMP3:$tmp/screen.bmp"
              bmp="$tmp/screen.bmp"

              if [ -n "$cached" ]; then
                cp "$bmp" "$cached.tmp" && mv "$cached.tmp" "$cached"
              fi
              say "rendered"
            fi

            # Always upload: cheap, and it restores Node-RED's copy after a restart.
            curl -fsS --max-time 30 -X PUT \
              -H "Content-Type: application/octet-stream" \
              --data-binary "@$bmp" "$put_url"
          '';
        };
    in
    {
      packages = forAll (pkgs: {
        default = mkRender pkgs;
        magtag-render = mkRender pkgs;
      });

      devShells = forAll (pkgs: {
        default = pkgs.mkShell {
          packages = [ (mkRender pkgs) pkgs.imagemagick ];
          # Nix-provided fonts to copy/symlink next to your HTML for @font-face
          MAGTAG_FONTS = "${pkgs.inter}/share/fonts";
        };
      });
    };
}
