
# `haskell-permission-tool` - analyse function dependencies in Haskell source code

## Usage

### Generate `.hi` files with simplified core

Use the GHC option `-fwrite-simplified-core-info`. This requires ghc version >= 9.6.2.

This adds an extra section to the `.hi` files that we need to analyse dependencies.

Please also use the option `-O0`, which turns off optimisation, preserving function names and structure.

IMPORTANT: make sure you add this option inside a cabal.project file. Only then will it also rebuild the projects build dependencies with this flag.

Example cabal.project file

```cabal
packages:
  .

package *
  ghc-options: -fwrite-if-simplified-core -O0 
```

### Generate `environment` files

If you want to analyse any of your own modules, you must setup your `.cabal` file such that it contains a lirary with the module you'd like to analyze.

Use the cabal option `--write-ghc-environment-files=always`

example command: `cabal build exe:example-project --write-ghc-environment-files=always`

### Process `.hi` files with `haskell-permission-tool` and `nix`

See `nix run github:TimoBlok/haskell-permission-tool#ghc962  -- --help`

or `cabal run haskell-permission-tool -- --help`

## Troubleshooting

### "hi file versions don't match" error

`haskell-permission-tool` must be compiled and ran with the same GHC version that generated the simplified core.

Update the `#ghcXXX` part of your `nix build` or `nix run` invocation.

Or set a differenc ghc version using `ghcup`.

### `nix run .#ghcXXX` is slow for some values of `XXX`

For versions of GHC without packages cached online in nixpkgs, `nix` must recompile all the Haskell depndencies of `haskell-permission-tool` from scratch.

You can pre-compile a batch of several versions with `nix build .#ghcXXX .#ghcYYY .#ghcZZZ ...`

After this completes, `nix run` will be instant for those versions.

## Development

`nix build .#ghcXXX` type-checks & compiles with GHC version XXX

## Related

- Parts of this project are heavily inpsired by [cabal-audit](https://github.com/TristanCacqueray/cabal-audit/tree/main)
- GHC module [GHC.IfaceToCore](https://hackage.haskell.org/package/ghc-9.6.1/docs/GHC-IfaceToCore.html)

## Special thanks

Special thanks to Lawrence Chonavel for letting me use his project as a template.
