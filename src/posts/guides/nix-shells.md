---
authors:
  - "Yannik Sander"
date: 2021-11-30
title: One too many shell
subtitle: Clearing up with nix shells
tags: nix, tech, guide, flakes
description: Nix 2.4+ introduces replacements for commonly known nix commands. The `nix-shell` replacements cause some confusion which this post aims to solve.  
image: https://images.unsplash.com/photo-1562530370-8f4fe23d1e8d?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1920
status: published
image-credits: |
   Photo by Joshua J. Cotten on Unsplash: https://unsplash.com/photos/04ZHiZFgTqQ
...

# A new Nix command

This post follows the [release](https://discourse.nixos.org/t/nix-2-4-released/15822) of Nix 2.4, which among many other things introduced an all new (experimental) `nix` command along with the [previously discussed](../internals/2021-01-01-flake-ification) flake feature.

The `nix` command aims to collect most common commands such as `nix-build`, `nix-copy-closure`, `nix-env`, ... as subcommands of one common program. Unsurprisingly, this does not spare `nix-shell`.

Yet, unlike some of the other commands which received a more or less one-to-one replacement it is not so easy with `nix-shell`. This command was actually broken up into two commands both with different semantics: `nix shell` and `nix develop`. Yet, depending on what you used `nix-shell` for before the new commands may not exactly do what you would expect. Indeed, they happen to cause quite some [confusion](https://www.reddit.com/r/NixOS/comments/r15hx4/nix_shell_vs_nix_develop/) already. Most notably, while `nix-shell` invoked without any other flags previously set up the build environment of a derivation (which could be somewhat abused to define general development environments) that is _not_ what `nix shell` will do...

# What the shell...

Yes, but from the start...

## Development shells (`nix-shell [<derivation>]`)

Development shells using the new `nix` command are now created with `nix develop`.

That command is a more focused version with additional tools for development. It is meant to be used with flakes but supports standalone derivations using the `-f` flag. How to use it then and what will it do?

### A development aid

The intended use of `nix develop` is to recreate build environments for single packages. That is, you call `nix develop nixpkgs#hello` and are dropped into a shell that is as close as possible to the nix builder environment.

_"yes,.. and?"_

This allows you to build a package step by step, or better phase by phase, meaning that in particular the build instructions used by nix can be reused for development purposes. Thus, we can use nix without needing to repeatedly go through full build processes, including wasteful copying, configuring, etc.

To make this process even easier, `nix develop` now comes with special arguments to run those phases directly.

:::{.note header="Recap `nix develop`"}

`nix develop` creates a shell with _all `buildInputs`_ and environment variables of a derivation loaded.

This allows to _run phases_ individually in the shell using `$ unpackPhase`, `$ configurePhase` `$ buildPhase`, etc. 

...or directly using `nix develop --<PHASE>` or `nix develop --phase <PHASE>` (for non-standard phases).

...or run an arbitrary command in the shell using `nix develop --command <COMMAND> [<ARGS>+]`

**Why should you use this?**

It is most useful for locally developed packages.

Use it to set up the environment using e.g. the `configurePhase` and perform the subsequent development using `build` and `check` phases.
If your workflow includes things that are not part of a phase use `nix develop --command`
:::

In essence, this is exactly what `nix-shell` was intended for!


:::{.warning}
A slight annoyance with phases arises when the targeted derivation overrides standard phases, i.e. `{unpack,configure,build,install}Phase`s.
As the default implementation in nix's `stdenv` is done as functions an internal use of `runHook` will give precedence to those functions over the overriden phases stored as environment variables.

**Solution**

Enter a shell using `nix develop` and run the overriden phases using `eval $buildPhase` or `--command eval '\$buildPhase'`
:::


Practically, `nix-shell` was also used for another purpose; reproducible development environemts.

### Development environments

Particularly useful combined with tools like [`direnv`](https://direnv.net/), one can leverage the fact that the resulting shell of ~~`nix-shell`~~ `nix develop` includes all declared `buildInputs` and environment variables to put together an environment with all sorts of dependencies and development tools available. Importantly, nix will ensure the `setupHook` is run when the shell is opened allowing for some impure setup to happen.

:::{.help}
A helpful tool to achieve this is `mkShell`. This functions provides an easy interface to collect packages for an evironement.

```nix
pkgs.mkShell = {
  # a list of packages to add to the shell environment
  packages ? [ ]
, # propagate all the inputs from the given derivations
  inputsFrom ? [ ]
, buildInputs ? [ ]
, nativeBuildInputs ? [ ]
, propagatedBuildInputs ? [ ]
, propagatedNativeBuildInputs ? [ ]
, ...
}: ...
```

_All extra attributes get are applied as env variables._

:::

Being closely connected to flakes `nix develop` supports loading a flakes development shell directly if a `devShell` out put is defined (see my previous [post](../internals/2021-01-01-flake-ification.md) for an example).
