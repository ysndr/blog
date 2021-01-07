---
title: Building with Nix Flakes for, eh .. reasons!
subtitle: Migrating to Nix Flakes, why, how and what are Flakes in the first place
tags: internal, technical, nix, haskell
description: Nix Flakes are the long awaited addition coming with the next major release of Nix. What are Flakes, what do they offer, how can you use them now and should you consider migrating?
image: https://unsplash.com/photos/R1OSU00xo78/download?force=true&w=1920
status: published
image-credits: |
    <span>Photo by <a href="https://unsplash.com/@joshstyle">JOSHUA COLEMAN</a> on <a href="https://unsplash.com/">Unsplash</a></span>
...

# Walking through Snow, eh Nix Flakes for OSS

:::{.note}
This article is an update to my series on how this blog is built using [Haskell](/tags/haskell.html) and [Nix](/tags/nix.html).

- Building a reproducible blog with Nix ([link](/posts/internals/2020-04-10-built-with-nix))
- How this page is generated ([Part 01](/posts/internal/2019-12-31-built-with-hakyll-part-1.html), [Part 02](/posts/internal/2020-03-22-built-with-hakyll-part-2.html))
:::

I am happy to get back posting on this blog again after a while of silence. I had been kept busy lately with finishing my Bachelor's and moving to Stockholm to start my Master's at the Royal Institute of Technology. In the meantime I also started working for [NGI-ZERO](https://www.ngi.eu/ngi-projects/ngi-zero/), an initiative supporting open source projects led by the European Commission. As part of my work for them I package supported projects for the Nix ecosystem. The goal of this effort is to provide a reproducible and easy way to install and use this software. Due to the fact that those projects come in various shapes and sizes, need to be reviewed and should not clutter `nixpkgs` unnecessarily, [nig-nix](https://github.com/ngi-nix) from builds on top of the latest feature to be added to Nix, **Flakes**.

Although still not available in stable Nix, Flakes can already be used using wrappers and the unstable nix package. Inspired by my work and eager to try them for some private project i migrated my blog's tooling to use Flakes while also keeping up with the latest additions to the Haskell/Hakyll world.

Was it worth it? We will see but first, what am I even talking about?

# Flake (**noun**, /fleɪk/)

> a person you cannot trust to remember things or to do what they promise, or someone who behaves in a strange way
>
> Cambridge Dictionary[^flake]

Well, this was not too helpful and does not sound too promising either. So which other definition do we find there?

> a small, thin piece of something, especially if it has come from a surface covered with a layer of something

This will definitely sound relatable in a moment.

## Why?

Nix derivations are not completely sand boxed! Well the build is, meaning during the phases no network access, or access to to the hosts file system is possible. What is not so hermetic is the nix expression itself. Have you ever seen this line?

```nix
{pkgs ? import <nixpkgs> {}}:
```

Yes you have, certainly! One of the reasons why a derivation might fail on your system is due to this. The problem is that it relies on the `nixpkgs` channel currently active on *your system*, which is a major impurity, because people tend to not have the same version active or might even set it to different releases (`release-xx.xx`, `unstable`, ...).\
The common way around this is to pin the `nixpkgs` used by the developer. This is only a convention though. There is no definite way how to do this. You can see an example in my [previous aticle](/posts/internals/2020-04-10-built-with-nix#default.nix) about how I use nix on this blog. The main idea is to state the current version of `nixpkgs` in a file, fetch the tarball of that version and build against that. This is fine but requires us to update the version manually, and is not consistent among projects.\
Alternatively, the tool that emerged from this is [`niv`](https://github.com/nmattia/niv). Niv provides an easi interface to add external sources and import them from the projects Nix file. Niv in the background writes a lockfile and streamlines updating.

So there might be a way to pin packages and with some discipline we can get around this impurity. How about this then?

```nix
{ develop ? true }:
...
if develop then ...
           else ...
```

So easily your derivation becomes dependent on some command line argument. In this case it's a simple flag (*how do you know?*). In other cases it could expect a string even, like setting the ghc version used. This poses multiple problems. First you create an impurity, you depend on some command line flag to be given. More importantly, these inputs are not discoverable without inspecting the nix file, for string inputs, it might even be hard to figure out which values it takes, in which format, etc for this you need to dig through the definition.
Also, even with defaults set, importing the project from another place expects the importer to know which arguments they can/must give and require them to inspect the definition again.

Another issue you might come across:

```nix
{...}:
let
...
in {
  inherit shell app some-other-app;
}
```

How do you know what to import, what does it do? There are simply no inherent semantics and also no conventions.

Additionally to this Eelco also [lists](https://www.tweag.io/blog/2020-05-25-flakes/) some more issues with this. Firstly, due to missing strong conventions and semantics composing packages is difficult, or not nice of an experience for the user. It also means that discovering packages can not be automated easily.

:::{.note}
**Summary of problems**

Lets summarize the shortcomings we just discussed:

1. impure imports
2. no ultimate way to pin `nixpkgs`
3. no common (import) interface
4. impure arguments
5. no common (output) interface
6. poor discoverability
7. poor composability
:::



# Creating a flake for this blog

To understand how flakes aim to solve those problems, lets - once again - have a look on the new definition of this blog.

```{.nix caption=./flake.nix}
{
  description = "Flake utils demo";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.nur.url = "github:nix-community/NUR";
  inputs.flake-utils.url = "github:numtide/flake-utils";


  inputs.uikit-src = {
    url = "https://github.com/uikit/uikit/archive/v3.5.8.tar.gz";
    flake = false;
  };


  outputs = { self, nixpkgs, nur, flake-utils, uikit-src }:
    let
    in
      flake-utils.lib.eachDefaultSystem (
        system:
          let
            pkgs = import nixpkgs { inherit system; overlays = [ nur.overlay ]; };
            blog = pkgs.callPackage ./blog.nix {
              inherit pkgs;
              nur = pkgs.nur;
              thirdparty = [
                {
                  name = "uikit";
                  path = "${uikit-src}/src";
                }
              ];
            };
          in
            rec {
              packages = { inherit (blog) generator generator-with-thirdparty ci shell; htop = pkgs.htop; };
              defaultPackage = blog.generator-with-thirdparty;
              apps.compile =
                flake-utils.lib.mkApp { drv = blog.ci.compile; exePath = ""; };
              defaultApp = apps.compile;
              devShell = blog.shell;
            }
      );
}
```

So that's a lot! And notice, its even simplified through the use of [`flake-utils`](https://github.com/numtide/flake-utils).
But it does come with some structure, which we will inspect more closely now.

*So what do we see here?*

## Pure imports

In flake mode nix expects the flake files to only contain the top level attributes `description::string`, `inputs::set` , `outputs::{inputs..}->set`. Notice, flakes are not a function  at the top level anymore.\
What does that imply? First, there are no arguments to be given by command line (via `--arg`/`--argstr`). This means no `nixpkgs` attribute to be passed from a higher level. Instead, your dependencies are expected to be included in `inputs` you give. `inputs` is a set where each attribute is the identifier of one input.

The corresponding part in the above `flake.nix` is
```nix
  description = "Flake utils demo";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.nur.url = "github:nix-community/NUR";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.uikit-src = {
    url = "https://github.com/uikit/uikit/archive/v3.5.8.tar.gz";
    flake = false;
  };
```

Each of these input follows this structure:

```nix
inputs.<inputname> = {
  url = git+<url-to-git-repo>[?ref=****[&rev=****]]
      | (github|gitlab):<user>/<repo>[/<ref> | ?ref=****[&rev=****]];
      | <known-flake>[/<ref> | ?ref=****[&rev=****]]
      | /absolute/path
      | ./relative/path
  flake = true | false; # default = true
}
```

Input url
  ~ use `git+<url>` to checkout a git repository at `<url>`
  ~ use `/local/absolute/path` or `./relative/path` to load a local source
  ~ use `gitlab:<user>/<repo>`/`github:<user>/<repo>` to shortcut gitlab or github repositories
  ~ use `<known-flake>` to shortcut to a globally defined alias

`known-flake`
  ~ nix will manage a registry of named flakes that can be included as is and are a shortcut to another repo
  ~ by default following global registry items are defined in [this repo](https://github.com/NixOS/flake-registry) and available in every nix installation:
    ```
    global flake:blender-bin github:edolstra/nix-warez
    global flake:dwarffs github:edolstra/dwarffs
    global flake:hydra github:NixOS/hydra
    global flake:nimble github:nix-community/flake-nimble
    global flake:nix github:NixOS/nix
    global flake:nixops github:NixOS/nixops
    global flake:nixos-hardware github:NixOS/nixos-hardware
    global flake:nixos-homepage github:NixOS/nixos-homepage/flake
    global flake:nur github:nix-community/NUR
    global flake:nixpkgs/release-19.09 github:edolstra/nixpkgs/release-19.09
    global flake:nixpkgs github:NixOS/nixpkgs
    global flake:templates github:NixOS/templates
    global flake:patchelf github:NixOS/patchelf
    ```
  ~ read more about registry on the [wiki](https://nixos.wiki/wiki/Nix_command/registry)

Specifying a commit or branch
  ~ by default, the main branch of the repository pointed to will be used as to provide the dependency flake.
  ~ Nix will also update dependencies to the latest commit on `nix flake update` calls
  ~ to pin repositories to a certain commit or override the default branch append  a  `?ref=commit-ref` or `?<branch>` respectively

Non flake inputs
  ~ nix will try to interpret inputs as flakes, to suppress that, set `<inputname>.flake = false`{.nix}

Overriding dependency inputs
  ~ traditionally, if one wanted  a dependency to use a certain nixpkgs version (i.e. to make use of binary caches)  one would specify a certain `nixpkgs` argument while `import`'ing. Flakes allow the same thing by setting `inputs.<inputname>.inputs.<inputname>.follows = <local inputsname>`

:::{.notice}
If using a `<known-flake>` nix will not use your respective channel even if one with the same name such as `nixpkgs` exists.
:::

See also the [reference below](#reference) for more information and examples.

:::{.note}
**Issues solved here:**

1. impure imports
2. no ultimate way to pin `nixpkgs`
3. no common (import) interface

First, a formal definition even a simple one as this alleviates us from the danger of importing dependencies impurely. All imports are formally defined and locked to a specific revision in the projects `flake.lock` file. By being able to also add a specific `ref` tag to inputs, this interface allows for intuitive pinning of dependencies. Finally, the way imports are specified and overridden is the same for all flakes, providing a familiar and unified interface.
:::

So far so good, but we have only discussed about 10 lines of the flake s far...


## Pure outputs

As mentioned above, nix flakes do not serve as top level function anymore. That does not mean there is no function involved. For flakes the output is the result of a function as well.

```
outputs = { self, nixpkgs, nur, flake-utils, uikit-src }:
  ...
```

This third and last attribute takes as arguments all the inputs defined earlier. But unlike classical nix approaches, only a certain set of attributes are allowed and expected as the result of this function.\
*An extensive list of output attributes is provided [below](#output-attributes)*.

Generally the format for these is:

```
<kind>.<arch>.<output> = ...;
```

or concretely

```
packages."x86_64-darwin".generator = blog.generator;
```

Notice how the architecture and OS of the target platform is explicitly encoded, allowing for specializations or exclusive packaging for specific platforms. This also means that the used `nixpkgs` needs to be instantiated for all desired platforms. In practice one will want to use a  shortcut for that and while this is used for all our [projects packaged for NGI](https://github.com/ngi-nix) so far, for my blog this shortcut is provided by  [`flake-utils`](https://github.com/numtide/flake-utils).

Using these, defining the outputs for this blog just becomes:

```{.nix}
  let
    in
      flake-utils.lib.eachDefaultSystem (
        system:
          let
            pkgs' = import nixpkgs { inherit system; overlays = [ nur.overlay ]; };
            blog = pkgs'.callPackage ./blog.nix {
              pkgs = pkgs';
              nur = pkgs'.nur;
              thirdparty = [
                {
                  name = "uikit";
                  path = "${uikit-src}/src";
                }
              ];
            };
          in
            rec {
              packages = {
                inherit (blog) generator generator-with-thirdparty ci shell;
              };
              defaultPackage = blog.generator-with-thirdparty;
              apps.compile =
                flake-utils.lib.mkApp { drv = blog.ci.compile; exePath = ""; };
              defaultApp = apps.compile;
              devShell = blog.shell;
            }
      );
```

To take this apart again, we face three different sections here.

1. `flake-utils.lib.eachDefaultSystem`, like its name suggests will provide the attribute set in its scope for all default systems (basically linux and darwin on x86_64 hardware). Hence, we don't need to worry about these tags.
2. The actual output still needs to conform to flakes' expected attributes. In this case we set `packages`, `defaultPackage`, `apps`, `defaultApp` and `devShell`. There are even more most importantly `overlay` which are referenced [below](#output-attributes)
3. third one is the middle part and shows an important aspect of nix flakes.
   Because there is no access to impure variables (`--arg`, environment variables, the current system, etc.), `nixpkgs` must be imported explicitly for each system. Additionally, this gives the opportunity to apply any possible overlay.
   In the case of this blog everything is defined in an external `blog.nix` file and imported from there.

### Output attributes

:::{.warning}
This section highlights some commands that are not yet available on stable nix. In fact, you will need to run either `nixUnstable` or `nixFlakes` from a recent `nixpkgs` set, at best `unstable`. Also you need to add `experimental-features = nix-command flakes` to your `nix.conf` file.

With that set trying out the commands can be done in a shell using

```
$ nix-shell -p nixUnstable
```
:::

Coming back to the outputs. `packages` is the actual output that is used with `nix shell` and `nix build` commands in particular (yes there are/will be [a lot more](https://nixos.org/manual/nix/unstable/command-ref/experimental-commands.html)).

To open a shell with specific packages available (alike `nix-shell -p`) run:

```
$ nix shell ./#generator-with-thirdparty

or from an external repo:

$ nix shell "gitlab:pi-lar/neuropil#generator-with-thirdparty"
```

:::{.info}
Replace `shell` with `build` to build it to a `./result` output link.

Shell does actually accept multiple derivation arguments so
```
$ nix shell nixpkgs#{htop,nodePackages.act,tealdeer}
```
is possible to use.

*Notice* that some inputs might need to be quoted due to substitution rules in your shell.
:::

`apps` is used together with `nix run`, similarly to `npm run`. You can define binaries that can then be run directly without explicitly building them first. This allowed me to change the somewhat cryptic `$(nix-build -A packages.x86_64-linux.ci.compile --no-out-link)` to `nix run ./#compile` in the CI script. Actually given that `defaultApp` point to the same `compile` attribute, the argument `./#compile` could be elided paying with decreased clarity.

`devShell` is another special one used in combination with `nix develop` which opens a *`bash`* shell with the given derivation prepared.


Of course, project shells using for example `mkShell` do also still work (and are used here as well). Either by using `nix develop` with such a derivation or `nix shell` in combination with `buildEnv`.

:::{.warning}
`nix develop` **always** opens a *`bash`* shell. If you use `zsh` some work-arounds or relying on legacy `nix-shell` is needed for `zsh` enabled shells.

Using flakes, one alternative is to use a `buildEnv` derivation which is loaded using `nix `**`shell`**.
:::

:::{.note}
**Issues solved here:**

1. impure arguments
2. no common (output) interface
3. poor discoverability

Here we see how a defined output interface helps to understand to organize the specific outputs and give the user semantic cues what the outputs are used for. It also allows nix to be more ergonomic as seen in the case of `nix run`. Although, being a matter of the tooling as well, having a well defined `flake.nix` file available to inspect, discovering attributes over several repositories becomes much more feasible. Finally due to the missing impurity, guaranties that some derivation will actually produce the same outputs are getting even stronger.
:::

### Composing flakes

In the last two sections I presented solutions for six of the seven problems that were identified in the beginning. That leaves us with:

7. poor composability

Actually we're almost there.. Remember how I mentioned the possibility to inject overlays when importing `nixpkgs`? Composing multiple sources of packages is easy if one can just drop them in there.\
Moreover, there are conventional output arguments `overlay` and `overlays` that make discovering the fakes; overlay(s) a bit easier.

```{.nix}
{
  description = "Flake utils demo";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  # assuming that this flake exports an overlay attribute
  inputs.someflake.url = "foo.com/bar";

  outputs = { self, nixpkgs, flake-utils, someflake }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        artifacts = final: prev: {
          my_hello = prev.hello;
        };
        pkgs = import nixpkgs {
          overlays = [ someflake.overlay artifacts ];
        };
      in
      # and here is the core of the consumer flake
      {
        defaultPackage = pkgs.my_hello;
        overlay = artifacts;
      }
    );
}
```

Adapted from [this answer on the forum](https://discourse.nixos.org/t/how-to-determine-intended-use-of-nix-flakes-outputs/8633/2) this example shows how `someflake`'s overlay can be imported, and how to define your own overlay in the same manner. Here it is used rather uselessly, but one can imagine to define outputs in a separate file as overlay, that can then be plugged for use and export in the flake.

:::{.info}
As a kind of *new* convention now enforced by `nix check` is to call your overlay arguments `final: prev:` for greater clarity than the classical `self: super:` combination.
:::

# Retrospective

So I changed the nix setup of this blog to flakes... Was it worth it?

Of course it depends. For the project that is this blog the benefits of committing to flakes are rather unconvincing. No one uses this as a dependency or wants to discover it as such. I am the only one developing this, so I might have my own conventions to import things, pin my dependencies and define my interface. Also, maybe I do want to use CLI arguments to nix.

Personally though it did convince me (obviously). I am a great supporter of the Nix philosophy and the Flakes do make a great addition to the ecosystem. I first got into contact with them when starting to work on the NGI0 project at which time Eelco had just written the first public post about flakes and the documentation was sparse to say the least... But as a new thing it still caught my interest. Looking for projects outside NGI this blog seemed like a great candidate to apply this new technology to as it posed literally no risk whatsoever if it didn't work out. It gave me a chance to make myself comfortable with the technology and dig deeper to write this article.

Now I really like how this turned out, especially the `nix run` part comes in handy here and the native caching might come in handy for quicker `direnv` shells.

One downturn is still that `nix develop` is not able to open zsh shells so the usual `mkShell` technique does not work with flakes and one needs to fall back to `nix-shell` or `buildEnv` alternatives. Also there are still flags and a special nix binary required to make use of the flake related nix features.

# References

## Flake reference conventions

Nix commands will eventually reference outputs. There are multiple ways how these can be referenced depending on where they come from:

A flake reference is actually structured similar to an URL:

```
<flake url>[?<git ref/rev>][#<output>]
    │
    ├── git+<url>
    ├── gitlab:<user>/<repo> or github:<user>/<repo>
    ├── /local/absolute/path or ./relative/path
    └── a known source such as `nixpkgs`
```

If git repositories are pointed to directly query parameters can be added to select a specific branch and/or commit.

As nix has not yet adapted to some git providers renaming their `master` branches to `main` you might occasionally be confronted with an error similar to

>
```
fetching Git repository 'https://gitlab.com/pi-lar/neuropil'fatal: couldn't find remote ref refs/heads/master
```

To resolve this the actual branch `main` or any tag might be explicated using the `ref` argument:

```
git+https://gitlab.com/pi-lar/neuropil.git?ref=main
```
If using the abbreviated GitLab/GitHub urls this can be expressed like this:

```
gitlab:pi-lar/neuropil/main
```

Similarly, if an exact commit is targeted, this can be pointed to using the `rev` argument

```
   gitlab:pi-lar/neuropil?rev=1043b9c1736c2a9b92938f0304ddb55ce4ab68a6
or gitlab:pi-lar/neuropil/1043b9c1736c2a9b92938f0304ddb55ce4ab68a6
```

The output is finally selected in the format known from the `-A` argument of old nix commands such as `nix-shell`:

```
   nixpkgs#haskellPackages.pandoc
or .#generator-with-thirdparty      (used to build this blog's generator)
```

## Output attributes

Following is a quick reference over all flake output attributes.

```{.nix}
{
    # Executed by `nix flake check`
    checks."<system>"."<attr>" = derivation;

    # Executed by `nix build .#<name>`
    packages."<system>"."<attr>" = derivation;

    # Executed by `nix build .`
    defaultPackage."<system>" = derivation;

    # the derivation that is prepared when running `nix develop`
    # defaults to defaultPackage."<system>"
    devShell."<system>" = derivation;

    # Executed by `nix run .#<name>
    apps."<system>"."<attr>" = {
      type = "app";
      program = "<store-path>";
    };
    defaultApp."<system>" = { type = "app"; program = "..."; };

    # TODO: Only seen used in connection with nixpkgs
    legacyPackages = TODO;

    # overlay to easily override/packages in the package set of the importer
    overlay = final: prev: { };

    # TODO: Same idea as overlay but several.
    overlays."<attr>" = final: prev: { };

    # Additionally to packages also modules for Nixos can be provided (Flakes can be used with `nixos-rebuild`)
    nixosModule = Module definition;

    # TODO: Same idea as nixosModule but several
    nixosModules."<attr>" = Module definition;

    # Attrset of nixos configurations by hostname.
    nixosConfigurations."<hostname>" = {};

    # Hydra checks
    hydraJobs."<attr>"."<system>" = derivation;

    # Used by `nix flake init -t <flake>`
    defaultTemplate = {
      path = "<store-path>";
      description = "template description goes here?";
    };
    # Used by `nix flake init -t <flake>#<attr>`
    templates."<attr>" = { path = "<store-path>"; description = ""; );
  }
```


:::{.note}
**Further Reading/Resources**

- [wiki page on flakes](https://nixos.wiki/wiki/Flakes)
- [zimbatm's blog article](https://zimbatm.com/NixFlakes/)
- Eelco's blog article over at tweag.io
  - [Part 1](https://www.tweag.io/blog/2020-05-25-flakes/)
  - [Part 2](https://www.tweag.io/blog/2020-06-25-eval-cache/)
  - [Part 3](https://www.tweag.io/blog/2020-07-31-nixos-flakes/)
- [`nix` commands](https://nixos.wiki/wiki/Nix_command)
:::

As always, thank you for reading this far I hope you enjoyed this article!

It helped me a lot to structure my knowledge about nix flakes and I am glad to share this to a wider audience.

If you have comments drop me a mail, or comment publicly on the media you where found this. For additions or corrections and critique feel free to use [GitHub issue tracker](https://github.com/ysndr/blog/issues/new).

[^flake]: [Definition of Flake](https://dictionary.cambridge.org/dictionary/english/flake)
