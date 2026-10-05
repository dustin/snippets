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
          runtimeInputs = [ pkgs.python3 pkgs.imagemagick pkgs.coreutils pkgs.curl ];
          text = ''
            # usage: magtag-render <page-url> [dither]
            #   dither: None (default, best for text) or FloydSteinberg (photos/gradients)
            #   the page must set <html data-ready> once it has drawn
            #   uploads to $MAGTAG_PUT_URL (default http://bee1:1880/magtrmnl.bmp)
            #   gives up after $MAGTAG_TIMEOUT seconds (default 30) waiting for data-ready
            if [ $# -lt 1 ]; then
              echo "usage: magtag-render <page-url> [None|FloydSteinberg]" >&2
              exit 2
            fi

            url="$1"
            dither="''${2:-None}"
            put_url="''${MAGTAG_PUT_URL:-http://bee1:1880/magtrmnl.bmp}"
            timeout_s="''${MAGTAG_TIMEOUT:-30}"
            browser="''${CHROME:-${browser}}"

            if [ ! -x "$browser" ]; then
              echo "magtag-render: browser not found: $browser (set CHROME)" >&2
              exit 1
            fi

            tmp=$(mktemp -d)
            trap 'rm -rf "$tmp"' EXIT

            python3 ${./shot.py} "$browser" "$url" "$tmp/screen.png" "$timeout_s"

            magick "$tmp/screen.png" -colorspace Gray -dither "$dither" -posterize 4 \
              -type Palette "BMP3:$tmp/screen.bmp"

            curl -fsS --max-time 30 -X PUT \
              -H "Content-Type: application/octet-stream" \
              --data-binary "@$tmp/screen.bmp" "$put_url"
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
