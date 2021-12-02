---
authors:
  - "Yannik Sander"
date: 2021-12-01
title: One too many shell
subtitle: Clearing up with nix' shells nix shell and nix-shell
tags: nix, guide, flakes, newnixshellguide
description: Nix 2.4+ introduces replacements for commonly known nix commands. The `nix-shell` replacements cause some confusion which this post aims to solve. 
image: https://images.unsplash.com/photo-1562530370-8f4fe23d1e8d?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1920
status: published
image-credits: |
   Photo by Joshua J. Cotten on <a hrep=https://unsplash.com/photos/04ZHiZFgTqQ>Unsplash</a>:
...

# A new Nix command

This post follows the [release](https://discourse.nixos.org/t/nix-2-4-released/15822) of Nix 2.4, which among many other things introduced an all new (experimental) `nix` command along with the [previously discussed](../internals/2021-01-01-flake-ification) flake feature.

The `nix` command aims to collect most common commands such as `nix-build`, `nix-copy-closure`, `nix-env`, ... as subcommands of one common program. Unsurprisingly, this does not spare `nix-shell`.

Yet, unlike some of the other commands which received a more or less one-to-one replacement it is not so easy with `nix-shell`. This command was actually broken up into multiple commands with different semantics: `nix shell`, `nix develop` and `nix run`. Yet, depending on what you used `nix-shell` for in the past the new commands may not exactly do what you would expect. Indeed, they happen to cause quite some [confusion](https://www.reddit.com/r/NixOS/comments/r15hx4/nix_shell_vs_nix_develop/) already. Most notably, while `nix-shell` invoked without any other flags previously set up the build environment of a derivation (which could be somewhat abused to define general development environments) that is _not_ what `nix shell` will do...

# What the shell...^[Seemingly, no-one has given any thought about google'ability of the new commands... google **"newnixshellguide"** to find this post in the meantime] 

Yes, but from the start...

If you are here just for the commands you can read the tl;dr for [`nix develop`](#tldr-nix-develop), [`nix shell`](#tldr-nix-shell), or [`nix run`](#tldr-nix-run) directly or jump to the [notes](#notes-and-resources) section.


## Development shells (`nix-shell [DERIVATION]`)

Development shells using the new `nix` command are now created with `nix develop`.

That command is a more focused version with additional tools for development. It is meant to be used with flakes but supports standalone derivations using the `-f` flag. How to use it then and what will it do?

### A development aid

The intended use of `nix develop` is to recreate build environments for single packages. That is, you call `nix develop nixpkgs#hello` and are dropped into a shell that is as close as possible to the nix builder environment.

_"yes,.. and?"_

This allows you to build a package step by step, or better phase by phase, meaning that in particular the build instructions used by nix can be reused for development purposes. Thus, we can use nix without needing to repeatedly go through full build processes, including wasteful copying, configuring, etc.

To make this process even easier, `nix develop` now comes with special arguments to run those phases directly.

:::{.note #tldr-nix-develop header="tl;dr"}

`nix develop` creates a shell with _all `buildInputs`_ and environment variables of a derivation loaded and `shellHook`s executed.

This allows to _run phases_ individually in the shell using `$ unpackPhase`, `$ configurePhase` `$ buildPhase`, etc. 

...or directly using `nix develop \--<PHASE>` or `nix develop \--phase PHASE` (for non-standard phases).

...or run an arbitrary command in the shell using `nix develop \--command COMMAND [ARGS...]`

**Why should you use this?**

It is most useful for locally developed packages.

Use it to set up the environment using e.g. the `configurePhase` and perform the subsequent development using `build` and `check` phases.
If your workflow includes things that are not part of a phase use `nix develop \--command`
:::

In essence, this is exactly what `nix-shell` was intended for!


:::{.warning header=}
A slight annoyance with phases arises when the targeted derivation overrides standard phases, i.e. `{unpack,configure,build,install}Phase`s.
As the default implementation in nix's `stdenv` is done as functions, an internal use of `runHook` will give precedence to those functions over the overridden phases stored as environment variables.

**Solution**

Enter a shell using `nix develop` and run the overridden phases using `eval \$buildPhase` or `\--command eval '\$buildPhase'`.

:::


Practically, `nix-shell` was also used for another purpose; reproducible development environments.

### Development environments

Particularly useful combined with tools like [`direnv`](https://direnv.net/), one can leverage the fact that the resulting shell of ~~`nix-shell`~~ `nix develop` includes all declared `buildInputs` and environment variables to put together an environment with all sorts of dependencies and development tools available. Importantly, nix will also ensure all `setupHook`s are run when the shell is opened allowing for some impure setup to happen.

:::{.help}
A helpful tool to achieve this is `mkShell`. This function provides an easy interface to collect packages for an environment.

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

_All extra attributes unknown to `mkDerivation` are applied as env variables._

You can provide a `shellHook` to run commands whenever you enter the shell

:::

Being closely connected to flakes, `nix develop` supports loading a flake's development shell directly if a `devShell` output is defined.

:::{.help header=Example caption="(Adapted from my previous [post](../internals/2021-01-01-flake-ification.md)).
"}

```nix
{
  description = "Flake utils demo";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    let
    in
      flake-utils.lib.eachDefaultSystem (
        system:
          let
            pkgs' = import nixpkgs { inherit system; };
          in
            rec {
              packages = { myPackage = ... }; # packages defined here
              devShell  = pkgs'.mkShell = {
                # a list of packages to add to the shell environment
                packages ? [ jq ]
                , # propagate all the inputs from the given derivations
                  # this adds all the tools that can build myPackage to the environment
                inputsFrom ? [ pacakges.myPackage ]
              };
            }
      );
}
```


:::



## Temporary Programs

The fact that nix is built on the idea of the nix store from which user environments are created by cherry-picking the desired packages may raise the question, whether we may be able to amend our current environment imperatively^[Functional purists, please bear with me here].
And in fact, we can. It is possible to add software to a user's profile by imperatively by the means of ~~`nix-env -iA`~~ or nowadays `nix profile install`.

:::{.warning header=}
Beware that `nix profile` is incompatible with `nix-env` and therefore (today) also with `home-manager`.
:::

Yet, we also know that installing software this way is _not the Nix way_ of doing things. 

For the times when we *do* want to have some program at our disposal, either to try it out, use a different version or just needing it only temporarily, there should be a way to get this without going through the effort of adding it to your `configuration.nix`, `home.nix`, project `default.nix`, etc. and rebuilding your environment. In these cases, traditional distributions reach back to installing software or relying on containerization (i.e. docker). In Nix, while you could install the piece of software, the aforementioned usage of the nix store allows making software available temporarily without installing it.

This is what `nix-shell -p <package+>` is used for. `nix-shell` will retrieve the desired packages and open a shell with these packages mixed in.

:::{.help header="Example"}

Given you need to quickly convert some `asciidoc` to HTML usually the response of your terminal will be

```
$ asciidoc -b html5 manual.adoc
zsh: command not found: asciidoc
```

While you could go and add asciidoc to your configuration you might need it just once. In that case we can make use of `nix-shell`:

```
$ nix-shell -p asciidoc
[nix-shell] $ asciidoc -b html5 manual.adoc
```

This should now just work. Likewise, this works with almost anything available to install through `nixpkgs`.
:::


:::{.info header="Under the hood"}
Internally, what happens when `nix-shell -p asciidoc` is called is that nix constructs a derivation with the programs as `buildInputs` and popularizes them through the same mechanism described above.

In this case the derivation shell'ed into is:

```
with import <nixpkgs> { }; (pkgs.runCommandCC or pkgs.runCommand) "shell" { buildInputs = [ (asciidoc) ]; } ""
```

Note that instead of derivations you can also add expressions as the `-p` argument as these are just plugged in, i.e.:

```
$ nix-shell -p "import ./some.nix {}" 
```
:::

But this is not about `nix-shell`...

Nix 2.4 allows the same thing using `nix shell` now focussing on flakes.

With `nix shell` any output of a flake can be added to the environment by running

```
$ nix shell nixpkgs#asciidoc
```

*Here, nix strictly expects a [flake URL](../internals/2021-01-01-flake-ification/#flake-reference-conventions).*

Like `nix-shell` this command supports multiple arguments.

:::{.note #tldr-nix-shell header="tl;dr"}

`nix shell` creates a shell from the _specified inputs_.

This is useful to install temporary software

...from a [flake specifier](../../internals/2021-01-01-flake-ification/#flake-reference-conventions). 

...from a `*.nix` file/arbitrary expression using `\--impure \--expr EXPR` flags

**Why should you use this?**

The strong point about Nix is its declarative way to manage installations. Software that is used constantly can and should be packaged by the respective tool, be it a system configuration, home configuration.

For project development tools one can use development shells as discussed above.

Yet, sometimes a program or library is needed temporarily only, or once in a different version etc. In these cases programs can be loaded into the shell using `nix shell`. Derivations from this kind command are eventually garbage collected and removed from the nix store, so they do not use up dist space unnecessarily.
:::

:::{.warning header=}
Notably, not mentioned here is the use of `nix shell` to load `FANCYLANGUAGE` with `FANCYLANGUAGEPACKAGES`. Sadly, this hits the limits of the new command. See the section about `shellHook`s [below](#shell-hooks).
:::

## Run scripts

Apart from dropping into development shells, `nix-shell` can also be used to run commands and programs from derivation not currently installed to the user's profile. This is it can build a shell as before and run a command inside transparently.

We discussed the use of `nix-shell \--command COMMAND ARGS` above, where we would run a command from within the build environment of a derivation. Similarly, we may want to just run a program provided by a derivation. For this `nix-shell` provided the `\--run` argument

:::{.info header="\"`\--command`\" vs \"`run`\""}

As a development aid, `\--command` is interactive, meaning among other things, that *if a command fails or is interrupted by the user, the user is dropped into the shell with the build environment loaded*.

This behavior translates into an invocation using the `-p PROGRAM` argument as well as seen in the following box.


```bash
$ asciidoc 
zsh: command not found: asciidoc

$ nix-shell -p asciidoc --command asciidoc
Man page:     asciidoc --help manpage
Syntax:       asciidoc --help syntax

$ asciidoc # still available as were in the build shell with asciidoc present
Man page:     asciidoc --help manpage
Syntax:       asciidoc --help syntax
```

`\--run` runs non-interactive and closes the shell after the command returns
```bash
$ asciidoc 
zsh: command not found: asciidoc

$ nix-shell -p asciidoc --run asciidoc
Man page:     asciidoc --help manpage
Syntax:       asciidoc --help syntax

$ asciidoc # not available anymore
zsh: command not found: asciidoc
```

:::


### `nix shell -c`

As the functions of `nix-shell DERIVATION` and `nix-shell -p DERIVATION` were separated, the new tools come with new clearer semantics.

The generic `nix-shell \--run` function is now `nix shell -c`. Given an installable, nix allows to run any command in an environment where the installable is present. Note that this command is run in a non-interactive shell. The shell is dropped as the command ends.

:::{.info}
The above example using the new command would look like this:

```
$ nix shell nixpkgs#asciidoc -c asciidoc
```
:::

### `nix run`

Yet, `nix shell -c` will still require to type the name of the executed program. As for most programs this command is the same as the derivation name e.g. `nix shell nixpkgs#asciidoc -c asciidoc` another command was introduced named `nix run`. With `nix run` the previous command can be run as `nix run nixpkgs#asciidoc`.

Naturally, the functionality of `nix run` goes further and as is the case for many other new commands mainly concerns flakes. Next to `packages`, `nixosModules` and others, flakes can now also define `apps`. Written as records with two fields -`type` (currently, necessarily `app`) and `program` (an executable path) - these apps can be run directly using `nix run`.

This app definition

```nix
...
outputs = {self}: {
  ...
  apps.x86_64-linux.watch = {
    type = "app";
    program = "${generator-watch}";
  };
}
```

... could be used to watch and build this blogs source directly using `nix run .#watch`, given `generator-watch` is a script in the nix store. Note that *`program` only accepts paths* in the store and *no default arguments*.

If an attribute to `nix run` is not found as an app, nix will look up a `program` using this key instead and execute `programs.\${<program>}/bin/<program>` instead.

:::{.note #tldr-nix-run header="tl;dr"}

The new `nix` command comes with a new way to run programs not installed in your system for an even greater "run and forget" experience.

With `nix shell DERIVATION+ -c COMMAND`

... run any command in an environment with all specified `DERIVATION`s present (again consider the [section about `shellHook`s](#shell-hooks))

With `nix run INSTALLABLE` you can

... run scripts defined in a flake under the `apps` output 

... run the executables of any derivation as long as it is located in `bin/INSTALLABLE` of the derivation with the attribute name `INSTALLABLE`
:::

## Shell interpreter `#!/usr/bin/env nix-shell`

Lastly, a useful feature of `nix-shell` is its usage as a shell interpreter. What that means is that `nix-shell` can be used to dynamically fetch dependencies for a script file and execute the file in that context. Shell interpreters are defined using a special syntax at the start of script files. 

:::{.info header="Shebang Interpreter Line"}

Bash, Zsh and other shells interpret a first line starting with `#!` as an interpreter instruction. This way, instead of running a file using e.g. bash or python explicitly, we can write `#!/usr/bin/env python` to instruct the shell to use python to execute the file.


```{.python caption="file.py"}
#!/usr/bin/env python

# file.py

print("hello")
```

Here `$ python file.py` would be equivalent to `./file.py`, given the user has permission to execute the file.
:::

The `nix-shell` command can be used to use nix to provide the actual interpreter using

```.bash
#! /usr/bin/env nix-shell
#! nix-shell -i real-interpreter -p packages
```

Therefore, the above example would therefore continue to work even without python installed, if defined as

```{.python caption="file.py"}
#! /usr/bin/env nix-shell
#! nix-shell -i python -p python3
print("hello")
```

:::{.warning}
As it stands this function does not [yet](https://github.com/NixOS/nix/pull/5189) have a replacement.
:::

# Notes and Resources




## To flake or not to flake

Most of the new nix commands are designed in a flake first way. Most notably `nix {shell,develop,run}` expect [flake URLs](../internals/2021-01-01-flake-ification/#flake-reference-conventions) as argument. Traditional `*.nix` files can be used with the `\--expr` argument all commands support. As flake mode imposes greater purity strictness, imports have to happen with the `\--impure` flag given:

```sh
$ nix shell --impure --expr "import my.nix {}"
```



## A word about `shellHook`s {#shell-hooks}

While shell hooks were previously introduced as a means to run certain commands to set up a development shell with `nix develop` and `nix-shell`, they find use in other places. Concretely, neither `nix run` nor `nix shell` are running these hooks. This is even though their `nix-shell` equivalent does so.

What that means is that `nix shell` cannot be used to load e.g. a Python environment with packages directly as python modules use the `shellHook` to set up the `PYTHONPATH`. The same goes for Perl and other ecosystems with similar principles. This means 

```
nix shell nixpkgs#{python,python3Packages.numpy}
```

and

```
nix-shell -p python python3Packages.numpy
```

Do not evaluate to the same thing.

Instead, one needs to work around with custom expressions or `mkShell`s passed to nix develop.

### Workarounds

For Python in particular we can use `python3.withPackages` to build an ad-hoc derivation with the paths set up correctly:

```
nix shell --impure --expr "with import <nixpkgs> {}; python.withPackages (pkgs: with pkgs; [ prettytable ])" 
```

This might work as well with other language ecosystems 

Alternatively, we can (ab)use `nix develop` by passing it a `devShell`:

```
nix develop --impure --expr "with import <nixpkgs> {}; pkgs.mkShell { packages = [python3 python3Packages.numpy ];}" 
```

Both approaches work to some degree but are clunky (i.e. *not improving UX as promised*) and rely on the supposed-to-be-superseded channels.
## The second-hardest problem

Picking up the confusion mentioned in the beginning, there is another problem with `nix shell`... naming.
Being so closely named to its _semantically different_ predecessor, it is impossible to query google for meaningful, targeted results. This is a pain for newcomers and more experienced nix'ers alike. And indeed, there is a heated [discussion](https://github.com/NixOS/nix/issues/4715) on renaming the shell command. Yet, until that is resolved, I hope this guide helps to understand the differences a bit better.

*newnixshellguide*

# Appendix
## Summary table

| /                        | `nix-shell`                                                     | `nix develop`                                                   | `nix shell`                       | `nix run`                         |
| ------------------------ | ------------------------ | ------------------------ | ------------------------ | ------------------------ |
| **runs `shellHook`s**    | yes                                                             | yes                                                             | no                                | no                                |
| **use as interpreter**   | yes                                                             | no                                                              | no                                | no                                |
| **supports flakes**      | no                                                              | yes                                                             | yes                               | only                              |
| **evaluate nix file**    | yes                                                             | with `\--impure`, `-f` or `\--expr`                               | with `\--impure`, `-f` or `\--expr` | with `\--impure`, `-f` or `\--expr` |
| **modifies environment** | `PATH`, attributes mk`mkDerivation` and changes by `shellHooks` | `PATH`, attributes mk`mkDerivation` and changes by `shellHooks` | `PATH`                            | *nothing*                         |
