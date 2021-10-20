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
