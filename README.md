
# `haskell-permission-tool` - analyse function dependencies in Haskell source code

## Usage

### Generate `.hi` files with simplified core

Use the GHC option `-fwrite-simplified-core-info`

This adds an extra section to the `.hi` files that we need to analyse dependencies

### Generate `environment` files

Use the cabal option `--write-ghc-environment-files=always`

example command: `cabal build --write-ghc-environment-files=always --ghc-options=-fwrite-if-simplified-core`

### Process `.hi` files with `haskell-permission-tool` and `nix`

See `nix run github:TimoBlok/haskell-permission-tool#ghc927 -- --help`

## Troubleshooting

### "hi file versions don't match" error

`haskell-permission-tool` must be compiled with the same GHC version that generated the `.hi` files.

Update the `#ghcXXX` part of your `nix build` or `nix run` invocation.

For more information on the backwards-(in)compatability of `.hie` files, see [this GHC ticket](https://gitlab.haskell.org/ghc/ghc/-/issues/18329)

### `nix run .#ghcXXX` is slow for some values of `XXX`

For versions of GHC without packages cached online in nixpkgs, `nix` must recompile all the Haskell depndencies of `haskell-permission-tool` from scratch.

You can pre-compile a batch of several versions with `nix build .#ghcXXX .#ghcYYY .#ghcZZZ ...`

After this completes, `nix run` will be instant for those versions.

## Development

`nix build .#ghcXXX` type-checks & compiles with GHC version XXX

## Related

- [CabalAudit](https://github.com/TristanCacqueray/cabal-audit/tree/main)
- GHC module [GHC.IfaceToCore](https://hackage.haskell.org/package/ghc-9.6.1/docs/GHC-IfaceToCore.html)

## Special thanks

Special thanks to Lawrence Chonavel for letting me use his project as a template.
