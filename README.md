
# `haskell-permission-tool` - analyse function dependencies in Haskell source code

## Usage

### Generate `.hie` files

Use the GHC option `-fwrite-ide-info`

For more information on generating `.hie` files, see [this GHC blog post](https://www.haskell.org/ghc/blog/20190626-HIEFiles.html)

### Process `.hie` files with `haskell-permission-tool` and `nix`

See `nix run github:TimoBlok/haskell-permission-tool#ghc927 -- --help`

## Troubleshooting

### "hie file versions don't match" error

`haskell-permission-tool` must be compiled with the same GHC version that generated the `.hie` files.

Update the `#ghcXXX` part of your `nix build` or `nix run` invocation.

For more information on the backwards-(in)compatability of `.hie` files, see [this GHC ticket](https://gitlab.haskell.org/ghc/ghc/-/issues/18329)

### `nix run .#ghcXXX` is slow for some values of `XXX`

For versions of GHC without packages cached online in nixpkgs, `nix` must recompile all the Haskell depndencies of `haskell-permission-tool` from scratch.

You can pre-compile a batch of several versions with `nix build .#ghcXXX .#ghcYYY .#ghcZZZ ...`

After this completes, `nix run` will be instant for those versions.

## Development

`nix build .#ghcXXX` type-checks & compiles with GHC version XXX
