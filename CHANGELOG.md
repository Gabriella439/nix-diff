1.0.21

* [Support relocated Nix store](https://github.com/Gabriella439/nix-diff/pull/82)
  * If configured through `NIX_REMOTE` environment variable.
* [Compact input derivation diffs](https://github.com/Gabriella439/nix-diff/pull/86)
* [Fix support for `less -R` when coloring multiple lines](https://github.com/Gabriella439/nix-diff/pull/96)

1.0.20

* [Add `--context` flag](https://github.com/Gabriella439/nix-diff/pull/81)
  * This limits the context to the specified number of characters/words/lines
* [Bump upper bounds](https://github.com/Gabriella439/nix-diff/pull/79)
  * Allow `unix-2.8` and few more

1.0.19

* [New Haskell API](https://github.com/Gabriella439/nix-diff/pull/60)
  * Previously `nix-diff` only had a command-line API, but now there is a
    Haskell API available under the `Nix.Diff` module hierarchy
* [Machine-readable output](https://github.com/Gabriella439/nix-diff/pull/61)
  * `nix-diff` now supports a `--json` flag for machine-readable JSON output
* [New `--skip-already-compared` command-line option](https://github.com/Gabriella439/nix-diff/pull/69)
  * This compresses a sub-tree of the diff if the leaves have already been
    compared
* [New `--squash-text-diff` command-line option](https://github.com/Gabriella439/nix-diff/pull/70)
  * This compresses textual diffs into larger spans when possible
* [Ignore `!${OUTPUT}`](https://github.com/Gabriella439/nix-diff/pull/66)
  * `nix-diff` will now strip everything after `!` from a derivation argument
    so that arguments like `/nix/store/…!bin` no longer fail

1.0.18

* [Document the `--color` command-line option](https://github.com/Gabriel439/nix-diff/pull/54)

1.0.17

* [Handle `.drv` and `.nix` files that contain non-UTF8 text](https://github.com/Gabriel439/nix-diff/pull/50)

1.0.16

* [Accept realised store paths as input](https://github.com/Gabriel439/nix-diff/pull/47)
  * `nix-diff` will attempt to find the corresponding derivations using
    `nix-store --query --deriver` if you provide a realised store path as input

1.0.15

* [Fix non-exhaustive pattern match](https://github.com/Gabriel439/nix-diff/pull/45)
    * This non-exhaustive pattern match would cause the program to fail if
      the new derivation added or removed input sources

1.0.14

* [Improve whitespace handling](https://github.com/Gabriel439/nix-diff/pull/40)
    * `--word-oriented` now treats all whitespace characters as word
      boundaries instead of just spaces

      This matches the behavior of `Prelude.words`

    * `--word-oriented` no longer highlights the intervening spaces between
      words

      This is for consistency with `--line-oriented`, which doesn't
      highlight the intervening spaces between lines.  Also, you could argue
      that if the user specifies `--word-oriented` then they want all new
      tokens to be highlighted on a word-by-word basis.

    * `--line-oriented` now no longer introduces a trailing newline at the
      end of a newline-free segment of text

    * More generally, both `--word-oriented` and `--newline-oriented`
      exactly preserve the original whitespace that was used for word or
      line boundaries.

1.0.13

* [Add new `--word-oriented` mode, which is now the new default mode](https://github.com/Gabriel439/nix-diff/pull/38)
    * This now diffs on word boundaries instead of line-boundaries

1.0.12

* [Use "patience" diff algorithm](https://github.com/Gabriel439/nix-diff/pull/33)
    * This improves the quality of the diff

1.0.11

* [Add support for diffing input sources](https://github.com/Gabriel439/nix-diff/pull/30)
