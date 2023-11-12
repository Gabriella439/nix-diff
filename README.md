# `nix-diff`

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/Gabriella439/nix-diff/ci.yaml?branch=main)](https://github.com/Gabriella439/nix-diff/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/nix-diff.svg?color=success)](https://hackage.haskell.org/package/nix-diff)
[![Dependencies](https://img.shields.io/hackage-deps/v/nix-diff?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=nix-diff)

This package provides a `nix-diff` executable which explains why two Nix
derivations differ.  The most common use cases for this are:

* Understanding why you have a cache miss
* Verifying a Nix change had the intended effect at the derivation level
* Improving your understanding of how Nix works

For example, consider the following Nix derivation to build a bare-bones NixOS
system:

```nix
let
  nixos = import <nixpkgs/nixos> {
    system = "x86_64-linux";

    configuration = {
      boot.loader.grub.devices = [ "/dev/sda" ];

      fileSystems."/" = {
        device = "/dev/sda";
      };
    };
  };

in
  nixos.system
```

We can use `nix-instantiate` to compute the derivation for the above file:

```bash
$ nix-instantiate example.nix  # Your exact hash might differ
warning: you did not specify ‘--add-root’; the result might be removed by the garbage collector
/nix/store/6z9nr5pzs4j1v9mld517dmlcz61zy78z-nixos-system-nixos-18.03pre119245.5cfd049a03.drv
```

Now, let's add a service to our system definition:

```nix
let
  nixos = import <nixpkgs/nixos> {
    system = "x86_64-linux";

    configuration = {
      boot.loader.grub.devices = [ "/dev/sda" ];

      fileSystems."/" = {
        device = "/dev/sda";
      };

      services.apache-kafka.enable = true;
    };
  };

in
  nixos.system
```

... and compute the derivation for the updated system:

```bash
$ nix-instantiate example.nix
warning: you did not specify ‘--add-root’; the result might be removed by the garbage collector
/nix/store/k05ibijg0kknvwrgfyb7dxwjrs8qrlbj-nixos-system-nixos-18.03pre119245.5cfd049a03.drv
```

We can use `nix-diff` to compare the two computed derivations to determine what
changed about our system:

```bash
$ nix-diff /nix/store/6z9nr5pzs4j1v9mld517dmlcz61zy78z-nixos-system-nixos-18.03pre119245.5cfd049a03.drv /nix/store/k05ibijg0kknvwrgfyb7dxwjrs8qrlbj-nixos-system-nixos-18.03pre119245.5cfd049a03.drv
```

... which produces the following output:

![](https://i.imgur.com/KUB4rXx.png)

It's also possible to pass store paths or links to store paths, for example:

```ShellSession
$ nix-build example.nix
$ nix-diff /run/current-system ./result
```
## Testing

Run tests, using `cabal test` command.

You need to have nix installed in your PC.

## Development status

I don't currently plan to add any new features, but I do welcome feature
requests.  Just open an issue on the issue tracker if you would like to request
an improvement.

## License (BSD 3-clause)

    Copyright (c) 2017 Gabriella Gonzalez
    All rights reserved.

    Redistribution and use in source and binary forms, with or without modification,
    are permitted provided that the following conditions are met:
        * Redistributions of source code must retain the above copyright notice,
          this list of conditions and the following disclaimer.
        * Redistributions in binary form must reproduce the above copyright notice,
          this list of conditions and the following disclaimer in the documentation
          and/or other materials provided with the distribution.
        * Neither the name of Gabriella Gonzalez nor the names of other contributors
          may be used to endorse or promote products derived from this software
          without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
