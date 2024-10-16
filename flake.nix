{
  description = "Rust stable & LLVM";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      rust-overlay,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        toolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" ];
        };
      in
      {
        devShells.default =
          with pkgs;
          mkShell {
            packages = [
              # Dev Env
              git
              neovim
              fd
              fzf
              ripgrep
              fish
              starship
              bat
              # LLVM dep
              llvm_18
            ];
            nativeBuildInputs = [
              clang
              # Use mold when we are runnning in Linux
              (lib.optionals stdenv.isLinux mold)
            ];
            buildInputs = [
              toolchain # Including cargo, clippy & cargo-fmt
              rust-analyzer-unwrapped # rust-analyzer comes from nixpkgs toolchain, here is the unwrapped version
            ];

            # Env Vars
            ## Some environment to make rust-analyzer work correctly (Still the path prefix issue)
            RUST_SRC_PATH = "${toolchain}/lib/rustlib/src/rust/library";
            LLVM_SYS_180_PREFIX = llvm_18.dev;

            shellHook = ''
              exec ${fish.outPath}/bin/fish
            '';
          };
      }
    );
}
